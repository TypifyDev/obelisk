{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Frontend routing effects and combinators.
--
-- Replaces obelisk-route's @RoutedT@, @SetRouteT@, @RouteToUrlT@
-- with Effectful effects. Route changes managed via browser History API.
module Jenga.Route.Frontend
  ( -- * Effects
    Routed(..), askRoute
  , SetRoute(..), setRoute, modifyRoute
  , RouteToUrl(..), askRouteToUrl

    -- * Combinators
  , switchRoute, switchRoute_
  , routeLink, routeLink'

    -- * Interpreter
  , runBrowserRouting
  , runBrowserRoutingWith
  ) where

import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Effectful (Effect, Dispatch(..), DispatchOf, (:>), Eff, IOE)
import qualified Effectful
import           Effectful.Dispatch.Dynamic (send, interpret_)
import           Effectful.Dispatch.Static (unsafeEff_)
import           Reflex.Effectful.Effect.JSM (JSM', liftJSM)
import           Data.Maybe (fromMaybe)

import           Control.Lens ((.~), (&), (%~))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)

import           Reflex (Dynamic, Event, Reflex)
import qualified Reflex as R
import           Data.Default (Default(def))
import           Reflex.Dom.Core
                   ( GhcjsDomSpace, EventResult, ElementConfig, AttributeName(..)
                   , elementConfig_initialAttributes, elementConfig_eventSpec
                   , addEventSpecFlags, preventDefault
                   , domEvent, EventName(Click)
                   )

import           Reflex.Effectful.Types (KnownTimeline)
import           Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import           Reflex.Effectful.Effect.Sample (Sample, sample)
import           Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import           Reflex.Effectful.Effect.TriggerEvent (TriggerEvent, newTriggerEvent)
import           Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import           Reflex.Effectful.Effect.Adjustable (Adjustable, runWithReplace)
import           Reflex.Effectful.Effect.Dom (Dom, el', elAttr, text, element)
import           Reflex.Effectful.Effect.Prerender (Prerender)
import           Reflex.Effectful.Effect.DomRenderHook (DomRenderHook, requestDomAction_)

import qualified Language.Javascript.JSaddle as JS

import           Jenga.Route (HasRoute(..), urlToSegments)

-- ─── Effects ───────────────────────────────────────────────────

data Routed t r :: Effect where
  AskRoute :: Routed t r m (Dynamic t r)
type instance DispatchOf (Routed t r) = 'Dynamic

askRoute :: Routed t r :> es => Eff es (Dynamic t r)
askRoute = send AskRoute

data SetRoute t r :: Effect where
  SetRoute    :: Event t r -> SetRoute t r m ()
  ModifyRoute :: Event t (r -> r) -> SetRoute t r m ()
type instance DispatchOf (SetRoute t r) = 'Dynamic

setRoute :: SetRoute t r :> es => Event t r -> Eff es ()
setRoute = send . SetRoute

modifyRoute :: SetRoute t r :> es => Event t (r -> r) -> Eff es ()
modifyRoute = send . ModifyRoute

data RouteToUrl r :: Effect where
  AskRouteToUrl :: RouteToUrl r m (r -> Text)
type instance DispatchOf (RouteToUrl r) = 'Dynamic

askRouteToUrl :: RouteToUrl r :> es => Eff es (r -> Text)
askRouteToUrl = send AskRouteToUrl

-- ─── Combinators ───────────────────────────────────────────────

-- | Switch widget based on current route. Replaces @subRoute@.
switchRoute
  :: ( KnownTimeline es t, Routed t r :> es
     , Adjustable t :> es, Hold t :> es, Sample t :> es
     , Dom t :> es, Reflex t
     )
  => (r -> Eff es a)
  -> Eff es (Dynamic t a)
switchRoute f = do
  routeDyn <- askRoute
  initial <- sample (R.current routeDyn)
  (a0, aEv) <- runWithReplace (f initial) (f <$> R.updated routeDyn)
  holdDyn a0 aEv

-- | Like 'switchRoute' but discards the result. Replaces @subRoute_@.
switchRoute_
  :: ( KnownTimeline es t, Routed t r :> es
     , Adjustable t :> es, Hold t :> es, Sample t :> es
     , Dom t :> es, Reflex t
     )
  => (r -> Eff es ())
  -> Eff es ()
switchRoute_ f = do
  routeDyn <- askRoute
  initial <- sample (R.current routeDyn)
  _ <- runWithReplace (f initial) (f <$> R.updated routeDyn)
  pure ()

-- | Create a link that navigates to a route on click.
--
-- Left-click: prevented, uses pushState (SPA navigation).
-- Right-click / Ctrl+click: normal browser behavior (href is set).
-- Crawlers: see the href, follow it normally.
--
-- Scrolls to top on navigation (matching browser behavior for
-- full page loads).
routeLink
  :: forall r t es.
     ( KnownTimeline es t, Dom t :> es
     , SetRoute t r :> es, RouteToUrl r :> es
     , Prerender t :> es
     , Reflex t
     )
  => r -> Eff es () -> Eff es ()
routeLink target child = do
  toUrl <- askRouteToUrl
  -- Build ElementConfig with preventDefault on Click
  let cfg = (def :: ElementConfig EventResult t GhcjsDomSpace)
        & elementConfig_initialAttributes
            .~ Map.mapKeys (AttributeName Nothing) (Map.singleton "href" (toUrl target))
        & elementConfig_eventSpec
            %~ addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click
                (const preventDefault)
  (e, _) <- element "a" cfg child
  setRoute (target <$ domEvent Click e)

-- | Create a text link that navigates to a route.
routeLink'
  :: forall r t es.
     ( KnownTimeline es t, Dom t :> es
     , SetRoute t r :> es, RouteToUrl r :> es
     , Prerender t :> es
     , Reflex t
     )
  => r -> Text -> Eff es ()
routeLink' target label = routeLink target (text label)

-- | Like 'routeLink' but with additional attributes.
routeLinkAttr
  :: forall r t es.
     ( KnownTimeline es t, Dom t :> es
     , SetRoute t r :> es, RouteToUrl r :> es
     , Prerender t :> es
     , Reflex t
     )
  => Map Text Text -> r -> Eff es () -> Eff es ()
routeLinkAttr attrs target child = do
  toUrl <- askRouteToUrl
  let targetBlank = Map.lookup "target" attrs == Just "_blank"
      allAttrs = Map.insert "href" (toUrl target) attrs
      cfg = (def :: ElementConfig EventResult t GhcjsDomSpace)
        & elementConfig_initialAttributes
            .~ Map.mapKeys (AttributeName Nothing) allAttrs
        & (if targetBlank then id else
            elementConfig_eventSpec
              %~ addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click
                  (const preventDefault))
  (e, _) <- element "a" cfg child
  when (not targetBlank) $
    setRoute (target <$ domEvent Click e)

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
     ( HasRoute api r
     , KnownTimeline es t, Reflex t
     , Hold t :> es, Sample t :> es, PostBuild t :> es
     , TriggerEvent t :> es, PerformEvent t :> es
     , DomRenderHook t :> es
     , Dom t :> es, JSM' :> es, IOE :> es
     )
  => Proxy api
  -> r                          -- ^ Fallback route for unrecognized URLs
  -> Eff (Routed t r : SetRoute t r : RouteToUrl r : es) a
  -> Eff es a
runBrowserRouting _ =
  runBrowserRoutingWith
    (encodeRoute @api)
    (\url -> let (segs, qp) = urlToSegments url in decodeRoute @api segs qp)

-- | Transport-agnostic browser routing interpreter. Accepts explicit
-- encode/decode functions instead of a 'HasRoute' instance, so any
-- URL-scheme provider (Servant HasRoute, obelisk-route Encoder wrapper,
-- custom hand-rolled) plugs in without touching the interpreter.
--
-- 'runBrowserRouting' is a thin wrapper over this that supplies
-- @encodeRoute \@api@ and @decodeRoute \@api@.
runBrowserRoutingWith
  :: forall r t es a.
     ( KnownTimeline es t, Reflex t
     , Hold t :> es, Sample t :> es, PostBuild t :> es
     , TriggerEvent t :> es, PerformEvent t :> es
     , DomRenderHook t :> es
     , Dom t :> es, JSM' :> es, IOE :> es
     )
  => (r -> Text)                -- ^ Route → URL (path + query)
  -> (Text -> Maybe r)          -- ^ URL → maybe route
  -> r                          -- ^ Fallback route for unrecognized URLs
  -> Eff (Routed t r : SetRoute t r : RouteToUrl r : es) a
  -> Eff es a
runBrowserRoutingWith encRoute decRoute fallback eff = do
  -- Shared trigger: both popstate AND setRoute fire through this
  (urlEv, fireUrl) <- newTriggerEvent

  -- On postBuild: read initial URL + set up popstate listener
  pb <- getPostBuild
  requestDomAction_ $ R.ffor pb $ \_ -> do
    pathname <- JS.valToText =<< JS.jsg ("window" :: Text)
      JS.! ("location" :: Text) JS.! ("pathname" :: Text)
    search <- JS.valToText =<< JS.jsg ("window" :: Text)
      JS.! ("location" :: Text) JS.! ("search" :: Text)
    liftIO $ fireUrl (pathname <> search)

    -- Listen for popstate (browser back/forward)
    cb <- JS.function $ \_ _ _ -> do
      p <- JS.valToText =<< JS.jsg ("window" :: Text)
        JS.! ("location" :: Text) JS.! ("pathname" :: Text)
      s <- JS.valToText =<< JS.jsg ("window" :: Text)
        JS.! ("location" :: Text) JS.! ("search" :: Text)
      liftIO $ fireUrl (p <> s)
    _ <- JS.jsg ("window" :: Text) JS.# ("addEventListener" :: Text)
      $ [ JS.toJSVal ("popstate" :: Text)
        , JS.toJSVal cb
        ]
    pure ()

  -- Parse URL text into route values
  let parseOrFallback :: Text -> r
      parseOrFallback url = fromMaybe fallback (decRoute url)

  -- Hold the current route (updated by both popstate AND setRoute)
  routeDyn <- holdDyn fallback (parseOrFallback <$> urlEv)

  -- Store fireUrl in IORef so SetRoute interpreter can access it
  fireRef <- unsafeEff_ $ newIORef fireUrl

  -- Strip effects from outermost to innermost:
  -- Stack: Routed t r : SetRoute t r : RouteToUrl r : es
  -- 1. Strip Routed (outermost)
  -- 2. Strip SetRoute
  -- 3. Strip RouteToUrl (innermost)
  interpret_ (\case AskRouteToUrl -> pure encRoute)
    $ interpret_ (\case
        SetRoute ev ->
          -- Callback runs in JSM (not Eff es) — see performEvent_'s
          -- signature in Reflex.Effectful.Effect.PerformEvent.
          performEvent_ $ R.ffor ev $ \route -> do
            let url = encRoute route
            -- Actually update the browser URL (history.pushState).
            -- Without this, setRoute updates internal app state but the
            -- URL bar and history stack don't move: back button broken,
            -- refresh returns to start URL, links unshareable.
            _ <- JS.jsg ("window" :: Text)
                   JS.! ("history" :: Text)
                   JS.# ("pushState" :: Text)
                   $ [ JS.toJSVal JS.jsNull
                     , JS.toJSVal ("" :: Text)
                     , JS.toJSVal url
                     ]
            -- Fire internal trigger so routeDyn updates in the same tick
            liftIO $ do
              fire <- readIORef fireRef
              fire url
        ModifyRoute _ -> pure ()
      )
    $ interpret_ (\case AskRoute -> pure routeDyn)
    $ eff

