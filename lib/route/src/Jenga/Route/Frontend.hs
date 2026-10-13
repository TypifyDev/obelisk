{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Frontend routing surface: re-exports the canonical Effectful effect
-- surface from 'Jenga.Route.Effect' plus the browser History
-- interpreter ('runBrowserRouting', 'runBrowserRoutingWith').
--
-- Consumers may import either 'Jenga.Route.Effect' (for just the effect
-- surface) or this module (for the effect surface plus the browser
-- runners).
module Jenga.Route.Frontend
  ( -- * Route types (re-exported)
    module Jenga.Route

    -- * Effect surface (re-exported)
  , module Jenga.Route.Effect

    -- * Browser History interpreter
  , runBrowserRouting
  , runBrowserRoutingWith
  ) where

import qualified Data.IORef                        as IORef
import qualified Data.Proxy                        as P
import           Data.Text                         (Text)

import           Effectful                         ((:>), Eff, IOE)
import qualified Effectful.Dispatch.Dynamic        as ED
import qualified Effectful.Dispatch.Static         as ES
import qualified Data.Maybe                        as Maybe

import qualified Control.Monad.IO.Class            as CMIO

import qualified Reflex.Class                      as R
import qualified Reflex.Dynamic                    as RD

import qualified Reflex.Effectful.DomHost          as DH
import qualified Reflex.Host.HeadlessEffectful     as HH

import qualified Language.Javascript.JSaddle       as JS

import           Jenga.Route
import           Jenga.Route.Effect
import qualified Jenga.Route.Servant               as Servant

-- ─── Browser interpreter ───────────────────────────────────────

-- | Run routing effects using the browser History API.
--
-- @
-- main = mainWidgetEff $ runBrowserRouting (Proxy \@FrontendPages) FR_Main $ do
--   switchRoute_ $ \\case
--     FR_Main  -> text "Home"
--     FR_Login -> loginWidget
-- @
runBrowserRouting
  :: forall api r t es a.
     ( Servant.HasRoute api r
     , R.Reflex t
     , HH.Hold t :> es, HH.PostBuild t :> es
     , HH.TriggerEvent t :> es, HH.PerformEventH t :> es
     , DH.DomRenderHookE t :> es
     , DH.JSM' :> es
     , IOE :> es
     )
  => P.Proxy api
  -> r                          -- ^ Fallback route for unrecognized URLs
  -> Eff (Routed t r : SetRoute t r : RouteToUrl r : es) a
  -> Eff es a
runBrowserRouting _ =
  runBrowserRoutingWith
    (Servant.encodeRoute @api)
    (\url -> let (segs, qp) = Servant.urlToSegments url in Servant.decodeRoute @api segs qp)

-- | Transport-agnostic browser routing interpreter. Accepts explicit
-- encode/decode functions instead of a 'HasRoute' instance, so any
-- URL-scheme provider (Servant HasRoute, obelisk-route Encoder wrapper,
-- custom hand-rolled) plugs in without touching the interpreter.
--
-- 'runBrowserRouting' is a thin wrapper over this that supplies
-- @encodeRoute \@api@ and @decodeRoute \@api@.
runBrowserRoutingWith
  :: forall r t es a.
     ( R.Reflex t
     , HH.Hold t :> es, HH.PostBuild t :> es
     , HH.TriggerEvent t :> es, HH.PerformEventH t :> es
     , DH.DomRenderHookE t :> es
     , DH.JSM' :> es
     , IOE :> es
     )
  => (r -> Text)                -- ^ Route → URL (path + query)
  -> (Text -> Maybe r)          -- ^ URL → maybe route
  -> r                          -- ^ Fallback route for unrecognized URLs
  -> Eff (Routed t r : SetRoute t r : RouteToUrl r : es) a
  -> Eff es a
runBrowserRoutingWith encRoute decRoute fallback eff = do
  -- Shared trigger: both popstate AND setRoute fire through this
  (urlEv, fireUrl) <- HH.newTriggerEventE @t

  -- On postBuild: read initial URL + set up popstate listener
  pb <- HH.getPostBuildE @t
  DH.requestDomAction_ @t $ R.ffor pb $ \_ -> do
    pathname <- JS.valToText =<< JS.jsg ("window" :: Text)
      JS.! ("location" :: Text) JS.! ("pathname" :: Text)
    search <- JS.valToText =<< JS.jsg ("window" :: Text)
      JS.! ("location" :: Text) JS.! ("search" :: Text)
    CMIO.liftIO $ fireUrl (pathname <> search)

    -- Listen for popstate (browser back/forward)
    cb <- JS.function $ \_ _ _ -> do
      p <- JS.valToText =<< JS.jsg ("window" :: Text)
        JS.! ("location" :: Text) JS.! ("pathname" :: Text)
      s <- JS.valToText =<< JS.jsg ("window" :: Text)
        JS.! ("location" :: Text) JS.! ("search" :: Text)
      CMIO.liftIO $ fireUrl (p <> s)
    _ <- JS.jsg ("window" :: Text) JS.# ("addEventListener" :: Text)
      $ [ JS.toJSVal ("popstate" :: Text)
        , JS.toJSVal cb
        ]
    pure ()

  -- Parse URL text into route values
  let parseOrFallback :: Text -> r
      parseOrFallback url = Maybe.fromMaybe fallback (decRoute url)

  -- Hold the current route (updated by both popstate AND setRoute)
  routeDyn <- HH.holdDynE @t fallback (parseOrFallback <$> urlEv)

  -- Store fireUrl in IORef so SetRoute interpreter can access it
  fireRef <- ES.unsafeEff_ $ IORef.newIORef fireUrl

  -- Capture JS context so SpiderHostFrame callbacks can invoke JSaddle
  -- actions via runJSM (SpiderHostFrame is IO-based, not Eff-based).
  jsCtx <- DH.askJSM

  -- Shared commit action: push route to browser history + fire internal
  -- trigger so the routeDyn Dynamic updates. Used by both SetRoute and
  -- ModifyRoute. Runs in SpiderHostFrame via liftIO (mirrors the pattern
  -- at Reflex.Effectful.DomHost.performJSMEvent_).
  let commitRoute :: r -> IO ()
      commitRoute route = do
        let url = encRoute route
        JS.runJSM
          (do
             _ <- JS.jsg ("window" :: Text)
                    JS.! ("history" :: Text)
                    JS.# ("pushState" :: Text)
                    $ [ JS.toJSVal JS.jsNull
                      , JS.toJSVal ("" :: Text)
                      , JS.toJSVal url
                      ]
             pure ())
          jsCtx
        fire <- IORef.readIORef fireRef
        fire url

  -- Strip effects from outermost to innermost:
  -- Stack: Routed t r : SetRoute t r : RouteToUrl r : es
  -- 1. Strip RouteToUrl (outermost interpret_ call, innermost effect)
  -- 2. Strip SetRoute
  -- 3. Strip Routed (innermost interpret_ call, outermost effect)
  ED.interpret_ (\case AskRouteToUrl -> pure encRoute)
    $ ED.interpret_ (\case
        SetRoute ev ->
          HH.performEventH_ @t $
            R.ffor ev $ \route -> CMIO.liftIO (commitRoute route)
        ModifyRoute ev ->
          -- Sample the current route at each fire (attachPromptlyDynWith),
          -- apply the (r -> r) function, commit via the same path as SetRoute.
          -- The original stub `pure ()` was a silent no-op — this is the real fix.
          HH.performEventH_ @t $
            R.ffor (RD.attachPromptlyDynWith (\r f -> f r) routeDyn ev) $
              \newRoute -> CMIO.liftIO (commitRoute newRoute)
      )
    $ ED.interpret_ (\case AskRoute -> pure routeDyn)
    $ eff
