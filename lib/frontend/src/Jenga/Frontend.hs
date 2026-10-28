{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful and MTL frontends sharing Jenga's static and browser hosts.
module Jenga.Frontend
  ( -- * Frontend type
    Frontend(..)
  , FrontendMode(..)
  , FrontendWidget

    -- * Server-side rendering
  , renderFrontendHtml
  , injectExecutableConfigs

    -- * DOM utilities
  , baseTag
  , removeHTMLConfigs
  , nodeListNodes

    -- * Upstream MTL surface — Eff-shape ports
  , ObeliskWidget
  , PrebuildAgnostic
  , FrontendWidgetT
  , setInitialRoute
  , runFrontend
  , runFrontendWithConfigsAndCurrentRoute

  , module Jenga.Frontend.Cookie
  ) where

import           Control.Monad                           (when)
import           Control.Monad.Fix                       (MonadFix)
import           Control.Monad.IO.Class                  (MonadIO, liftIO)
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString.Base64                  as B64
import           Data.Foldable                           (for_)
import           Data.Kind                               (Constraint, Type)
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Maybe                              (catMaybes)
import           Data.Monoid                             (appEndo)
import           Foreign.JavaScript.TH                   (JSContextSingleton (..))
import qualified Data.Proxy                              as P
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE
import           Effectful                               (Eff, Effect, IOE, (:>))
import qualified Effectful.Dispatch.Dynamic              as ED
import qualified GHCJS.DOM                               as DOM
import           "ghcjs-dom" GHCJS.DOM.Document          (getHead)
import qualified GHCJS.DOM.History                       as History
import qualified GHCJS.DOM.Location                      as Location
import           GHCJS.DOM.Node                          (Node, removeChild_)
import           GHCJS.DOM.NodeList                      (IsNodeList, getLength, item)
import           GHCJS.DOM.ParentNode                    (querySelectorAll)
import           GHCJS.DOM.Types                         (JSM)
import qualified GHCJS.DOM.Window                        as Window
import qualified Language.Javascript.JSaddle             as JSaddle
import qualified Network.URI                             as URI
import           System.Info                             (os)

import qualified Reflex.Class                            as R
import qualified Reflex.Host.HeadlessEffectful           as HH
import qualified Reflex.Host.Class                       as RH
import qualified Reflex.Effectful.DomHost                as DH
import           Reflex.Effectful.DomHost
  ( DomBuilderE
  , DomBuilderSpaceE
  , DomBuilderTimelineE
  , DomSpace
  , HasDocumentE
  , HasPrerender
  , JSM'
  , liftJSM
  , el
  , elAttr
  , text
  , blank
  , renderStatic
  )

import           Jenga.Frontend.Cookie
import           Jenga.Frontend.MTL
import qualified Jenga.Frontend.Effectful.Configs as ConfigE
import           Jenga.Frontend.Effectful.Configs        (Configs, runConfigs)
import           Jenga.Route.Effect
  ( Routed(..)
  , SetRoute(..)
  , RouteToUrl(..)
  , setAdaptedUriPath
  )
import qualified Jenga.Route.Frontend                    as Routing
import qualified Jenga.Route.Servant                     as Servant

-- ─── Frontend type ─────────────────────────────────────────────

-- | Constraint bag required by a frontend widget.
--
-- Same shape whether the widget is being run through the SSR
-- 'renderStatic' path (space @~ StaticDomSpace@) or the client-side
-- 'mainWidget'/hydration path (space @~ GhcjsDomSpace@ /
-- @HydrationDomSpace@). Browser-only work goes inside 'prerender' via
-- 'HasPrerender', which supplies 'JSM'@ / 'DomBuilderE' etc. inside the
-- client branch's effect row.
type FrontendWidget (es :: [Effect]) (t :: Type) (r :: Type) =
  ( R.Reflex t
  , t ~ DomBuilderTimelineE es
  , DomBuilderE t (DomBuilderSpaceE es) :> es
  , HasDocumentE (DomBuilderSpaceE es) :> es
  , DomSpace (DomBuilderSpaceE es)
  , HH.Hold t :> es
  , HH.Sample t :> es
  , HH.CreateTrigger t :> es
  , HH.PostBuild t :> es
  , HH.TriggerEvent t :> es
  , HH.NotReady t :> es
  , HH.Adjustable t :> es
  , HH.PerformEventM (RH.HostFrame t) t :> es
  , IOE :> es
  , HasPrerender t (DomBuilderSpaceE es) es
  , Routed t r :> es
  , SetRoute t r :> es
  , RouteToUrl r :> es
  , Configs :> es
  , CookiesEff :> es
  )

-- | The frontend of a Jenga application.
--
-- Holds the head and body widgets as effectful computations
-- polymorphic over the effect row and Reflex timeline.
data Frontend r = Frontend
  { _frontend_head :: !(forall (es :: [Effect]) (t :: Type).
      FrontendWidget es t r => Eff es ())
  , _frontend_body :: !(forall (es :: [Effect]) (t :: Type).
      FrontendWidget es t r => Eff es ())
  }
  | FrontendMTL (FrontendMTLWidget r) (FrontendMTLWidget r)

data FrontendPart = FrontendHead | FrontendBody

renderFrontendPart
  :: (FrontendWidget es t r, FrontendBaseMTL t m)
  => (forall a. Eff es a -> m a) -> FrontendPart -> Frontend r -> m ()
renderFrontendPart liftEffects part frontend = case frontend of
  Frontend headWidget bodyWidget -> liftEffects $ case part of
    FrontendHead -> headWidget
    FrontendBody -> bodyWidget
  FrontendMTL headWidget bodyWidget -> case part of
    FrontendHead -> renderMTLPart liftEffects headWidget
    FrontendBody -> renderMTLPart liftEffects bodyWidget

renderMTLPart
  :: (FrontendWidget es t r, FrontendBaseMTL t m)
  => (forall a. Eff es a -> m a) -> FrontendMTLWidget r -> m ()
renderMTLPart liftEffects widget = do
  configs <- liftEffects ConfigE.getConfigs
  cookies <- liftEffects askCookies
  route <- liftEffects Routing.askRoute
  encode <- liftEffects Routing.askRouteToUrl
  ((), changes) <- runFrontendMTLWidget configs cookies route encode widget
  liftEffects $ Routing.modifyRoute $ appEndo <$> changes

-- | Controls how the frontend renders.
data FrontendMode = FrontendMode
  { _frontendMode_hydrate :: !Bool
    -- ^ Hydrate existing SSR'd DOM instead of building from scratch
  , _frontendMode_adjustRoute :: !Bool
    -- ^ Use hash-based routing (for platforms without ambient URLs)
  } deriving (Show, Eq)

-- ─── Server-side rendering (SSR) ──────────────────────────────

-- | Render the frontend to an HTML shell for the initial page load.
--
-- Composes @head@ + @body@ inside @<html>@, threads all the frontend
-- effects (Configs, CookiesEff, Routed, SetRoute, RouteToUrl) through
-- static interpreters, and drives 'renderStatic' from
-- 'Reflex.Effectful.DomHost' to produce the HTML bytestring.
--
-- Upstream parity:
--
--   * 'renderStatic' + 'runCookies' + 'runConfigs' + 'runRouteToUrl' +
--     'runSetRoute' + 'runRouted' with @pure route@ mirrors upstream
--     @Obelisk.Frontend.renderFrontendHtml@ (obelisk\/lib\/frontend).
--   * @<!DOCTYPE html>@ prefix, @<html>@ wrapper, @<head>@ + @<body>@
--     child tags emitted the same way.
--   * 'injectExecutableConfigs' emits @<script>@ tags carrying the
--     base64-encoded config map with the
--     @data-jenga-executable-config-inject-key@ attribute so
--     'removeHTMLConfigs' can strip them during hydration bootstrap.
renderFrontendHtml
  :: forall m r.
     MonadIO m
  => Map Text ByteString        -- ^ Configs
  -> [(ByteString, ByteString)] -- ^ Cookies
  -> (r -> Text)                -- ^ Route encoder
  -> r                          -- ^ Current route
  -> Frontend r
  -> (forall es t. FrontendWidget es t r => Eff es ())
    -- ^ Extra head content, appended after 'frontend_head'
  -> (forall es t. FrontendWidget es t r => Eff es ())
    -- ^ Extra body content, appended after 'frontend_body'
  -> m ByteString
renderFrontendHtml configs cookies urlEnc route frontend headExtra bodyExtra = do
  (_, html) <- liftIO $ renderStatic $
    runCookies cookies $
      runConfigs configs $
        runRouteToUrlStatic urlEnc $
          runSetRouteStatic @DH.StaticDomTimeline @r $
            runRoutedStatic @DH.StaticDomTimeline @r (R.constDyn route) $
              el "html" $ do
                el "head" $ do
                  baseTag
                  injectExecutableConfigs configs
                  renderFrontendPart id FrontendHead frontend
                  headExtra @_ @DH.StaticDomTimeline
                el "body" $ do
                  renderFrontendPart id FrontendBody frontend
                  bodyExtra @_ @DH.StaticDomTimeline
  pure $ "<!DOCTYPE html>" <> html

-- | Emit one @<script data-jenga-executable-config-inject-key=…>@ tag
-- per config entry, carrying the base64-encoded config value in its
-- text content. Mirrors upstream 'Obelisk.ExecutableConfig.Inject'
-- with a jenga-namespaced attribute so 'removeHTMLConfigs' can locate
-- and strip them on hydrate.
--
-- Widget-side (goes through 'DomBuilderE'), so hydration walks it as
-- part of the head DOM.
injectExecutableConfigs
  :: forall es t
   . ( R.Reflex t
     , t ~ DomBuilderTimelineE es
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Map Text ByteString
  -> Eff es ()
injectExecutableConfigs configs = for_ (Map.toList configs) $ \(k, v) ->
  elAttr "script"
    ( Map.fromList
        [ ("type",                                       "text/plain")
        , ("data-jenga-executable-config-inject-key",    k)
        , ("data-hydration-skip",                        "")
        ]
    ) $ text $ TE.decodeUtf8 $ B64.encode v

-- ─── Static route interpreters ─────────────────────────────────

-- | SSR interpreter for 'Routed': every 'askRoute' returns the same
-- @Dynamic t r@ supplied at the outer boundary. Upstream analog is
-- @runRoutedT@ applied to a constant @Dynamic@.
runRoutedStatic
  :: R.Dynamic t r
  -> Eff (Routed t r : es) a
  -> Eff es a
runRoutedStatic d = ED.interpret_ $ \case
  AskRoute -> pure d

-- | SSR interpreter for 'SetRoute': every 'setRoute' / 'modifyRoute'
-- event is discarded. Server-side rendering emits one HTML snapshot;
-- there is no navigation to react to.
runSetRouteStatic
  :: forall t r es a
   . Eff (SetRoute t r : es) a
  -> Eff es a
runSetRouteStatic = ED.interpret_ $ \case
  SetRoute    _ -> pure ()
  ModifyRoute _ -> pure ()

-- | SSR interpreter for 'RouteToUrl': every 'askRouteToUrl' returns
-- the encoder function supplied at the outer boundary.
runRouteToUrlStatic
  :: (r -> Text)
  -> Eff (RouteToUrl r : es) a
  -> Eff es a
runRouteToUrlStatic enc = ED.interpret_ $ \case
  AskRouteToUrl -> pure enc

-- ─── DOM utilities ─────────────────────────────────────────────

-- | Emit @<base href=\"/\">@ except on iOS.
baseTag
  :: forall es
   . ( R.Reflex (DomBuilderTimelineE es)
     , DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Eff es ()
baseTag =
  if os == "ios"
    then blank
    else elAttr "base" (Map.singleton ("href" :: Text) ("/" :: Text)) blank

-- | Remove the executable-config @<script>@ tags that were injected
-- into the SSR'd @<head>@. Called once during hydration bootstrap so
-- the runtime doesn't re-observe them as duplicates.
removeHTMLConfigs :: JSM' :> es => Eff es ()
removeHTMLConfigs = liftJSM $ do
  mdoc <- DOM.currentDocument
  case mdoc of
    Nothing  -> pure ()
    Just doc -> do
      mhd <- getHead doc
      case mhd of
        Nothing -> pure ()
        Just hd -> do
          nl <- querySelectorAll hd ("[data-jenga-executable-config-inject-key]" :: Text)
          nodes <- collectNodesJSM nl
          for_ nodes $ removeChild_ hd

-- | JSM-side helper (used by 'removeHTMLConfigs').
collectNodesJSM :: IsNodeList l => l -> JSM [Node]
collectNodesJSM es = do
  len <- getLength es
  -- 'len' is unsigned; guard against traversing [0..maxBound] on an empty list.
  ns <- traverse (item es) $ if len == 0 then [] else [0 .. len - 1]
  pure $ catMaybes ns

-- | Collect all nodes in a 'NodeList' into a Haskell list.
nodeListNodes
  :: (IsNodeList l, JSM' :> es)
  => l
  -> Eff es [Node]
nodeListNodes = liftJSM . collectNodesJSM


-- ==================================================================
-- Upstream MTL surface — Eff-shape ports
-- ==================================================================

-- | Alias for 'FrontendWidget' — the port's constraint bundle for
-- frontend widgets. Direct rename of upstream's @ObeliskWidget t route m@
-- (which was itself a ConstraintKinds bundle of DomBuilder / MonadFix /
-- Prerender / PostBuild / SetRoute / RouteToUrl / HasConfigs / HasCookies
-- etc.). Effectful equivalent bundles the same capabilities as effects
-- in the row.
type ObeliskWidget es t r = (FrontendWidget es t r :: Constraint)

-- | Effectful port of upstream 'PrebuildAgnostic' — the sub-bundle of
-- 'ObeliskWidget' that does NOT require DOM-builder capability
-- (i.e. computations that run identically pre- and post-build):
-- routing writers + config reader + MonadFix.
type PrebuildAgnostic es t r =
  ( SetRoute t r :> es
  , RouteToUrl r :> es
  , Configs :> es
  , MonadFix (Eff es)
  ) :: Constraint

-- | Effectful port of upstream 'FrontendWidgetT' — the concrete widget
-- shape used by 'renderFrontendHtml' (SSR path). Upstream's version was
-- a nine-deep transformer stack: @RoutedT DomTimeline r (SetRouteT ...
-- (RouteToUrlT ... (ConfigsT (CookiesT ... (StaticDomBuilderT DomTimeline
-- (PerformEventT DomTimeline DomHost)))))@. The Effectful equivalent is
-- a rank-1 quantified 'Eff' action whose row satisfies 'FrontendWidget'
-- — same semantics without threading a nine-layer transformer.
type FrontendWidgetT r =
  forall es t. FrontendWidget es t r => Eff es ()

-- ─── setInitialRoute ─────────────────────────────────────────────

-- | Set the browser's history state to the current URL adapted for the
-- routing mode. Called once at client boot when the mode uses hash
-- routing so the shipped URL matches what the router will decode.
--
-- Direct Eff port of upstream @Obelisk.Frontend.setInitialRoute@; the
-- 'getLocationUri' upstream helper is inlined here (reads protocol /
-- authority / pathname / search / hash off @window.location@ and
-- assembles a 'URI.URI').
setInitialRoute :: JSM' :> es => Bool -> Eff es ()
setInitialRoute useHash = liftJSM $ do
  window <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  initialUri <- getLocationUriJSM location
  history <- Window.getHistory window
  History.replaceState history JSaddle.jsNull ("" :: Text) $ Just $
    show $ setAdaptedUriPath useHash "/" initialUri

-- | Inlined port of upstream @Reflex.Dom.Location.getLocationUri@ —
-- reads five 'Location' properties and assembles a 'URI.URI'.
-- Private helper used only by 'setInitialRoute'.
getLocationUriJSM :: Location.Location -> JSM URI.URI
getLocationUriJSM location = URI.URI
  <$> (T.unpack <$> Location.getProtocol location)
  <*> (Just <$> getLocationUriAuthJSM location)
  <*> (T.unpack <$> Location.getPathname location)
  <*> (T.unpack <$> Location.getSearch location)
  <*> (T.unpack <$> Location.getHash location)

-- | Inlined port of upstream @Reflex.Dom.Location.getLocationUriAuth@.
getLocationUriAuthJSM :: Location.Location -> JSM URI.URIAuth
getLocationUriAuthJSM location = do
  host <- Location.getHostname location
  port <- Location.getPort location
  let appendColonIfNotEmpty x
        | T.null x  = ""
        | otherwise = ":" <> T.unpack x
  pure $ URI.URIAuth
    ""
    (T.unpack host)
    (appendColonIfNotEmpty port)

-- ─── runFrontend + runFrontendWithConfigsAndCurrentRoute ────────

-- | Client-side boot entry — Eff port of upstream
-- @Obelisk.Frontend.runFrontend@. Composes the effect interpreter stack
-- (Configs / CookiesEff / Routed / SetRoute / RouteToUrl via
-- 'Routing.runBrowserRoutingWith') around the frontend body and drives
-- 'DH.mainWidget'.
--
-- Signature diverges from upstream in one place: takes an explicit
-- 'Servant.HasRoute' proxy + fallback route (jenga's routing pattern)
-- rather than an @Encoder@ + typed obelisk route. Selects the
-- 'FrontendMode' from the current platform (GHCJS = hydrate + no hash;
-- JSaddle-warp = immediate + hash).
runFrontend
  :: forall api r.
     ( Servant.HasRoute api r
     )
  => P.Proxy api
  -> r                        -- ^ Fallback route for unrecognized URLs
  -> Frontend r
  -> IO ()
runFrontend proxy fallback frontend = do
  let mode = FrontendMode
        { _frontendMode_hydrate =
#ifdef ghcjs_HOST_OS
            True
#else
            False
#endif
        , _frontendMode_adjustRoute =
#ifdef ghcjs_HOST_OS
            False
#else
            True
#endif
        }
  runFrontendWithConfigsAndCurrentRoute mode Map.empty proxy fallback frontend

-- | Like 'runFrontend' but takes an explicit 'FrontendMode' and
-- pre-loaded config map. Eff port of upstream
-- @Obelisk.Frontend.runFrontendWithConfigsAndCurrentRoute@.
--
-- On hydrate mode, 'removeHTMLConfigs' strips the SSR-injected script
-- tags. On hash-route mode, 'setInitialRoute' rewrites the current URL
-- into a hash-fragment shape before the router reads it.
runFrontendWithConfigsAndCurrentRoute
  :: forall api r.
     ( Servant.HasRoute api r
     )
  => FrontendMode
  -> Map Text ByteString
  -> P.Proxy api
  -> r
  -> Frontend r
  -> IO ()
runFrontendWithConfigsAndCurrentRoute mode configs _proxy fallback frontend =
  DH.mainWidgetWithHead (renderPart FrontendHead) (renderPart FrontendBody)
  where
   renderPart :: forall x es. FrontendPart -> DH.ImmediateWidget x es
   renderPart part = do
    when (_frontendMode_hydrate mode) (DH.liftEff removeHTMLConfigs)
    when (_frontendMode_adjustRoute mode) $
      DH.liftEff (setInitialRoute (_frontendMode_adjustRoute mode))
    cookies <- DH.liftEff readCookiesFromDocument
    DH.liftEff $ runCookies cookies $ runConfigs configs $
      Routing.runBrowserRoutingWith @r @DH.DomTimeline
        (Servant.encodeRoute @api)
        (\url -> let (segments, query) = Servant.urlToSegments url
                 in Servant.decodeRoute @api segments query)
        fallback $ do
        case part of
          FrontendHead -> baseTag
          FrontendBody -> pure ()
        context <- DH.liftJSM JSaddle.askJSM
        DH.runWidgetEff (renderFrontendPart DH.liftEff part frontend)
          (JSContextSingleton context)
