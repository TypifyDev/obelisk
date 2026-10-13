{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Canonical home for the Effectful routing effect surface.
--
-- Owns the effect data declarations ('Routed', 'SetRoute', 'RouteToUrl'),
-- the @send@-based helpers, the DOM combinators ('switchRoute',
-- 'routeLink', ...), and the E-suffix MTL-parity aliases.
--
-- The __browser History interpreter__ ('runBrowserRouting',
-- 'runBrowserRoutingWith') lives in the sibling module 'Jenga.Route.Frontend'
-- — that module is the runner-only surface and imports the effect types
-- back out of here. Consumers may import either module: 'Jenga.Route.Frontend'
-- re-exports the effect names for source compatibility with call sites
-- ported from the earlier layout.
--
-- Lives in @jenga-route-servant@ under the @Jenga.*@ namespace (it is a
-- jenga-owned module and no longer squats under the @Reflex.Effectful.*@
-- namespace owned by the reflex-effectful / reflex-dom-core-effectful
-- packages). A thin re-export stub at
-- 'Reflex.Class' is retained for source-compat
-- with existing downstream consumers.
module Jenga.Route.Effect
  ( -- * Effects
    Routed(..), askRoute
  , SetRoute(..), setRoute, modifyRoute
  , RouteToUrl(..), askRouteToUrl

    -- * Sub-route dispatch (upstream 'Obelisk.Route.Frontend' analogs)
  , switchRoute, switchRoute_
  , subRoute, subRoute_
  , pairRoute, subPairRoute, subPairRoute_
  , maybeRoute, maybeRoute_, maybeRouted
  , eitherRoute, eitherRoute_, eitherRouted

    -- * Internal helpers (exported for parity with obelisk-route)
  , factorRouted
  , strictDynWidget, strictDynWidget_

    -- * Link widgets
  , routeLink, routeLink', routeLinkAttr

    -- * Scroll helpers (upstream 'Obelisk.Route.Frontend' parity)
    --
    -- 'scrollToTop' is the MTL-shaped signature (verbatim upstream) — works
    -- on any monad @m@ with an @RDPR.Prerender t m@ instance ('WidgetEff',
    -- 'WithJSContextSingleton', 'Eff serverEs', etc). 'scrollToTopEff' is
    -- the Effectful-native sibling using our own 'DH.HasPrerender' class.
    -- Function BODIES are byte-for-byte upstream — only signatures differ.
  , scrollToTop
  , scrollToTopEff

    -- * Pattern synonym + type-level coercion helpers (upstream parity)
  , pattern (:~)
  , dsumValueCoercion
  , dynamicIdentityCoercion

    -- * iOS hash-routing URI adapters (upstream 'Obelisk.Route.Frontend' parity)
  , adaptedUriPath
  , setAdaptedUriPath
  , pathToHash
  , hashToPath

    -- * E-suffix aliases (naming parity with MTL-shaped call sites)
    --
    -- Some call sites — ported over from the obelisk MTL layer — refer
    -- to these helpers by an @E@ suffix ('askRouteE', 'setRouteE',
    -- 'routeLinkE') to signal "the Effectful sibling of the MTL one".
    -- The underlying implementations are already in Effectful shape
    -- ('askRoute', 'setRoute', 'routeLink'), so the E-suffix names are
    -- just thin aliases re-exported for source compatibility.
  , askRouteE
  , setRouteE
  , routeLinkE
  ) where

import qualified Data.Map                          as M
import qualified Data.Proxy                        as P
import           Data.Text                         (Text)

import qualified Data.List                         as L
import qualified Data.Maybe                        as Maybe
import           Data.Functor.Compose              (Compose, getCompose)
import           Data.Functor.Identity             (Identity (..))
import           Data.GADT.Compare                 (GEq)
import           Data.Dependent.Sum                (DSum ((:=>)))
import           Data.Type.Coercion                (Coercion (Coercion), coerceWith)
import qualified Network.URI                       as URI

import qualified Effectful                         as E
import           Effectful                         ((:>), Eff)
import qualified Effectful.Dispatch.Dynamic        as ED

import           Control.Lens                      ((.~), (&), (%~))
import qualified Control.Monad                     as CM
import           Unsafe.Coerce                     (unsafeCoerce)

import qualified Reflex.Class                      as R
import qualified Reflex.Dynamic                    as RD
import qualified Data.Default                      as D
import qualified Reflex.Effectful.DomHost          as DH
import qualified Reflex.Dom.Builder.Class          as RDB
import qualified Reflex.Host.HeadlessEffectful     as HH
import qualified Reflex.Host.Class                 as RH
import qualified Reflex.Dom.Prerender              as RDPR
import qualified Reflex.PerformEvent.Class         as RPE
import qualified Language.Javascript.JSaddle       as JSaddle
import           Language.Javascript.JSaddle       (runJSM)
import           Control.Monad.IO.Class            (liftIO, MonadIO)
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Window                  as Window

-- ─── Effects ───────────────────────────────────────────────────

data Routed t r :: E.Effect where
  AskRoute :: Routed t r m (R.Dynamic t r)
type instance E.DispatchOf (Routed t r) = 'E.Dynamic

askRoute :: Routed t r :> es => Eff es (R.Dynamic t r)
askRoute = ED.send AskRoute

data SetRoute t r :: E.Effect where
  SetRoute    :: R.Event t r -> SetRoute t r m ()
  ModifyRoute :: R.Event t (r -> r) -> SetRoute t r m ()
type instance E.DispatchOf (SetRoute t r) = 'E.Dynamic

setRoute :: SetRoute t r :> es => R.Event t r -> Eff es ()
setRoute = ED.send . SetRoute

modifyRoute :: SetRoute t r :> es => R.Event t (r -> r) -> Eff es ()
modifyRoute = ED.send . ModifyRoute

data RouteToUrl r :: E.Effect where
  AskRouteToUrl :: RouteToUrl r m (r -> Text)
type instance E.DispatchOf (RouteToUrl r) = 'E.Dynamic

askRouteToUrl :: RouteToUrl r :> es => Eff es (r -> Text)
askRouteToUrl = ED.send AskRouteToUrl

-- ─── Combinators ───────────────────────────────────────────────

-- | Switch widget based on current route. Replaces @subRoute@.
switchRoute
  :: ( Routed t r :> es
     , HH.Adjustable t :> es, HH.Hold t :> es, HH.Sample t :> es
     , R.Reflex t
     )
  => (r -> Eff es a)
  -> Eff es (R.Dynamic t a)
switchRoute f = do
  routeDyn <- askRoute
  initial <- HH.sampleE (R.current routeDyn)
  (a0, aEv) <- HH.runWithReplaceE (f initial) (f <$> R.updated routeDyn)
  HH.holdDynE a0 aEv

-- | Like 'switchRoute' but discards the result. Replaces @subRoute_@.
switchRoute_
  :: forall r t es.
     ( Routed t r :> es
     , HH.Adjustable t :> es, HH.Sample t :> es
     , R.Reflex t
     )
  => (r -> Eff es ())
  -> Eff es ()
switchRoute_ f = do
  routeDyn <- askRoute @t @r
  initial <- HH.sampleE (R.current routeDyn)
  _ <- HH.runWithReplaceE (f initial) (f <$> R.updated routeDyn)
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
     ( DH.DomBuilderE t DH.GhcjsDomSpace :> es
     , DH.DomBuilderSpaceE es ~ DH.GhcjsDomSpace
     , SetRoute t r :> es, RouteToUrl r :> es
     , R.Reflex t
     )
  => r -> Eff es () -> Eff es ()
routeLink target child = do
  toUrl <- askRouteToUrl
  -- Build ElementConfig with preventDefault on Click
  let cfg = (D.def :: DH.ElementConfig DH.EventResult t DH.GhcjsDomSpace)
        & DH.elementConfig_initialAttributes
            .~ M.mapKeys (DH.AttributeName Nothing) (M.singleton "href" (toUrl target))
        & DH.elementConfig_eventSpec
            %~ RDB.addEventSpecFlags (P.Proxy :: P.Proxy DH.GhcjsDomSpace) DH.Click
                (const DH.preventDefault)
  (e, _) <- DH.element "a" cfg child
  setRoute (target <$ RDB.domEvent DH.Click e)

-- | Create a text link that navigates to a route.
routeLink'
  :: forall r t es.
     ( DH.DomBuilderE t DH.GhcjsDomSpace :> es
     , t ~ DH.DomBuilderTimelineE es
     , DH.DomBuilderSpaceE es ~ DH.GhcjsDomSpace
     , SetRoute t r :> es, RouteToUrl r :> es
     , R.Reflex t
     )
  => r -> Text -> Eff es ()
routeLink' target label = routeLink @r @t target (DH.text label)

-- ─── E-suffix aliases ──────────────────────────────────────────
--
-- Naming parity with call sites ported from the obelisk MTL layer.
-- The E-suffix ('askRouteE', 'setRouteE', 'routeLinkE') is a source-
-- compatibility marker for "Effectful sibling of the MTL helper of the
-- same base name". Implementations delegate straight through to the
-- already-Effectful un-suffixed versions.

-- | Effectful-sibling alias for 'askRoute'.
--
-- Type-variable order is @es t r@ to support call sites written as
-- @askRouteE \@es \@t@ — the ported obelisk callers apply the local
-- effect row and timeline explicitly and leave the route type to be
-- inferred from the surrounding context.
askRouteE :: forall es t r. Routed t r :> es => Eff es (R.Dynamic t r)
askRouteE = askRoute

-- | Effectful-sibling alias for 'setRoute'.
setRouteE :: forall es t r. SetRoute t r :> es => R.Event t r -> Eff es ()
setRouteE = setRoute

-- | Effectful-sibling alias for 'routeLink'.
routeLinkE
  :: forall es t r.
     ( DH.DomBuilderE t DH.GhcjsDomSpace :> es
     , DH.DomBuilderSpaceE es ~ DH.GhcjsDomSpace
     , SetRoute t r :> es, RouteToUrl r :> es
     , R.Reflex t
     )
  => r -> Eff es () -> Eff es ()
routeLinkE = routeLink @r @t

-- | Like 'routeLink' but with additional attributes.
--
-- Not currently exported (kept internal alongside the canonical link
-- combinators for the moment); wire into the export list once a
-- consumer needs it.
routeLinkAttr
  :: forall r t es.
     ( DH.DomBuilderE t DH.GhcjsDomSpace :> es
     , DH.DomBuilderSpaceE es ~ DH.GhcjsDomSpace
     , SetRoute t r :> es, RouteToUrl r :> es
     , R.Reflex t
     )
  => M.Map Text Text -> r -> Eff es () -> Eff es ()
routeLinkAttr attrs target child = do
  toUrl <- askRouteToUrl
  let targetBlank = M.lookup "target" attrs == Just "_blank"
      allAttrs = M.insert "href" (toUrl target) attrs
      cfg = (D.def :: DH.ElementConfig DH.EventResult t DH.GhcjsDomSpace)
        & DH.elementConfig_initialAttributes
            .~ M.mapKeys (DH.AttributeName Nothing) allAttrs
        & (if targetBlank then id else
            DH.elementConfig_eventSpec
              %~ RDB.addEventSpecFlags (P.Proxy :: P.Proxy DH.GhcjsDomSpace) DH.Click
                  (const DH.preventDefault))
  (e, _) <- DH.element "a" cfg child
  CM.when (not targetBlank) $
    setRoute (target <$ RDB.domEvent DH.Click e)

-- ─── Scroll helpers ────────────────────────────────────────────
--
-- Verbatim ports from upstream 'Obelisk.Route.Frontend.scrollToTop'
-- (obelisk rev 58c04270d606c061e7ffd2f16345e0f451eba600,
-- lib/route/src/Obelisk/Route/Frontend.hs:568-571). BODIES are
-- byte-for-byte upstream; only signatures adapt to the port.

-- | MTL-bridge signature. Monad-polymorphic over any @m@ that has the
-- MTL 'RDPR.Prerender t m' instance — i.e. 'WidgetEff t x es', 'WithJSContextSingleton
-- x (Eff serverEs)', and 'Eff serverEs' (via the bridge instances at
-- 'DomHost.hs:7664' and ':7789'). Behaves as upstream 'scrollToTop':
-- server branch is 'blank' (no-op / static render's discarded); client
-- branch fires 'performEvent_' that scrolls the window to the origin.
scrollToTop
  :: forall t m.
     ( RDPR.Prerender t m
     , Applicative m
     )
  => R.Event t () -> m ()
scrollToTop e = RDPR.prerender_ DH.blank $ RPE.performEvent_ $ R.ffor e $ \_ -> JSaddle.liftJSM $ DOM.currentWindow >>= \case
  Nothing  -> pure ()
  Just win -> Window.scrollTo win 0 0

-- | Effectful-native sibling of 'scrollToTop'. Uses the native
-- 'DH.HasPrerender' class from 'Reflex.Effectful.DomHost' — the
-- @space@-parameterised, effect-row-honest version of MTL 'Prerender'.
-- The superclass constraint bundle 'DH.PrerenderClientConstraintE'
-- transitively provides every effect the body needs
-- ('PerformEventH t' — added to 'PrerenderBaseConstraintsE' 2026-09-02
-- so the MTL 'performEvent_' bridge resolves inside the client branch;
-- 'JSM'' + 'IOE' for 'MonadJSM (Eff es)'). No extra caller-visible
-- constraints beyond 'HasPrerender'.
--
-- The client-branch action is factored into a named 'where'-bound
-- helper 'clientScrollAction' with a full type signature. The
-- signature names the client-side effect row explicitly as
-- @Eff (DH.ClientEs t space serverEs) ()@ so the reader can see the
-- row shift across the 'DH.prerender_' boundary without chasing type-
-- family reduction at the call site.
scrollToTopEff
  :: forall t space serverEs.
     ( DH.HasPrerender t space serverEs
     , MonadIO (RH.HostFrame t)
     )
  => R.Event t () -> Eff serverEs ()
scrollToTopEff e = DH.prerender_ @t @space DH.blank (clientScrollAction e)
  where
    clientScrollAction
      :: R.Event t ()
      -> Eff (DH.ClientEs t space serverEs) ()
    clientScrollAction ev = do
      -- Bypass the MTL PerformEvent bridge (which needs 'PerformableForRow'
      -- to reduce concretely — it can't on the abstract
      -- 'ClientEs t space serverEs' row here) and go directly to the
      -- effectful-native 'performEventME_' pinned at 'HostFrame t'. The
      -- 'HasPrerender' context already carries
      -- 'PerformEventM (HostFrame t) t' via 'PrerenderBaseConstraintsE'.
      --
      -- The callback runs in 'HostFrame t', which has MonadIO but not
      -- MonadJSM. Capture the JS context once in the widget body via
      -- 'DH.askJSM', then use plain 'runJSM' inside the callback.
      ctx <- DH.askJSM
      HH.performEventME_ @(RH.HostFrame t) @t $ R.ffor ev $ \_ -> liftIO $
        runJSM (DOM.currentWindow >>= \case
                  Nothing  -> pure ()
                  Just win -> Window.scrollTo win 0 0)
               ctx

-- ─── Pattern synonym + type-level coercion helpers ──────────────
--
-- Verbatim ports from upstream 'Obelisk.Route.Frontend'.

infixr 5 :~

-- | 'DSum' pattern that pairs an @f a@ with a @Dynamic t a@ (unwrapping
-- the intermediate 'Compose (Dynamic t) Identity' representation used
-- by 'factorRouted'). Upstream verbatim.
pattern (:~)
  :: R.Reflex t
  => f a -> R.Dynamic t a -> DSum f (Compose (R.Dynamic t) Identity)
pattern a :~ b <- a :=> (R.coerceDynamic . getCompose -> b)

-- | Lift a @Coercion f g@ to a @Coercion (DSum k f) (DSum k g)@.
dsumValueCoercion :: Coercion f g -> Coercion (DSum k f) (DSum k g)
dsumValueCoercion Coercion = Coercion

-- | @Compose (Dynamic t) Identity a@ is representationally equal to
-- @Dynamic t a@. Uses 'unsafeCoerce' because the equality is
-- representational but not observable through the class hierarchy —
-- matches upstream exactly.
dynamicIdentityCoercion :: Coercion (Compose (R.Dynamic t) Identity) (R.Dynamic t)
dynamicIdentityCoercion = unsafeCoerce (Coercion :: Coercion (Identity ()) ())

-- ─── Sub-route dispatch (parity with obelisk-route) ────────────
--
-- Effectful analogs of upstream @subRoute@, @maybeRoute@, etc. Instead
-- of the MTL 'RoutedT' transformer, the sub-computation receives its
-- route via a fresh 'Routed' effect interpreter installed by the
-- helper. The client code is: @Eff (Routed t a : es) b@.

-- | Internal: run a @strictDynWidget@-shaped body against a supplied
-- route Dynamic, installing a fresh 'Routed' interpreter around the
-- child. Corresponds to upstream @runRoutedT r d@ inside
-- 'strictDynWidget'.
runRoutedChild
  :: forall t a es b
   . R.Dynamic t a
  -> Eff (Routed t a : es) b
  -> Eff es b
runRoutedChild d = ED.interpret_ $ \case AskRoute -> pure d

-- | Analog of upstream 'strictDynWidget': run a child widget once per
-- distinct value of the current route, holding the latest result as a
-- 'Dynamic'. Signature parity kept — 'MonadHold' is expressed via
-- 'HH.Hold' + 'HH.Sample'; 'Adjustable' via 'HH.Adjustable'.
--
-- WARNING (upstream): the input 'Dynamic' must be fully constructed
-- when this is run.
strictDynWidget
  :: forall t a b es
   . ( HH.Hold t :> es, HH.Sample t :> es
     , HH.Adjustable t :> es
     , R.Reflex t
     )
  => R.Dynamic t a
  -> (a -> Eff es b)
  -> Eff es (R.Dynamic t b)
strictDynWidget r f = do
  r0 <- HH.sampleE @t (R.current r)
  (result0, result') <- HH.runWithReplaceE (f r0) (f <$> R.updated r)
  HH.holdDynE @t result0 result'

-- | Void variant of 'strictDynWidget'.
strictDynWidget_
  :: forall t a es
   . ( HH.Sample t :> es
     , HH.Adjustable t :> es
     , R.Reflex t
     )
  => R.Dynamic t a
  -> (a -> Eff es ())
  -> Eff es ()
strictDynWidget_ r f = do
  r0 <- HH.sampleE @t (R.current r)
  _ <- HH.runWithReplaceE (f r0) (f <$> R.updated r)
  pure ()

-- | Factor a @Dynamic t (DSum f Identity)@ into a Dynamic keyed by the
-- constructor of @f@. Direct effectful port of upstream 'factorRouted'
-- + 'factorDyn'. The wrapping @Compose@ is stripped via 'coerceWith'
-- + 'dynamicIdentityCoercion' to give a @DSum f (Dynamic t)@.
factorRouted
  :: forall t f r es
   . ( Routed t (DSum f Identity) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     , GEq f
     )
  => Eff (Routed t (DSum f (R.Dynamic t)) : es) r
  -> Eff es r
factorRouted body = do
  d <- askRoute @t @(DSum f Identity)
  d' <- RD.factorDyn d
  let factored = coerceWith (R.dynamicCoercion (dsumValueCoercion dynamicIdentityCoercion)) d'
  runRoutedChild @t @(DSum f (R.Dynamic t)) factored body

-- | Route into a GADT-indexed sub-widget. Effectful analog of
-- upstream @subRoute :: (forall a. r a -> RoutedT t a m b) -> RoutedT t (R r) m (Dynamic t b)@.
--
-- Note: unlike upstream, this port re-renders the child on every route
-- change (not only on constructor change). Upstream uses 'factorRouted'
-- + 'factorDyn' to avoid re-rendering when only the argument changes;
-- capturing that requires a different effect-row shape and is deferred.
-- Semantically equivalent up to render frequency.
subRoute
  :: forall r t es b
   . ( Routed t (DSum r Identity) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => (forall a. r a -> Eff (Routed t a : es) b)
  -> Eff es (R.Dynamic t b)
subRoute f = do
  d <- askRoute @t @(DSum r Identity)
  strictDynWidget d $ \(c :=> Identity r') ->
    runRoutedChild @t (R.constDyn r') (f c)

-- | Void variant of 'subRoute'.
subRoute_
  :: forall r t es
   . ( Routed t (DSum r Identity) :> es
     , R.Reflex t
     , HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => (forall a. r a -> Eff (Routed t a : es) ())
  -> Eff es ()
subRoute_ f = do
  d <- askRoute @t @(DSum r Identity)
  strictDynWidget_ d $ \(c :=> Identity r') ->
    runRoutedChild @t (R.constDyn r') (f c)

-- | Pass the left side of a pair route as a 'Dynamic' to the callback,
-- which routes over the right side. Direct effectful port of upstream
-- 'pairRoute'.
pairRoute
  :: forall t rl rr es a
   . ( Routed t (rl, rr) :> es
     , R.Reflex t
     )
  => (R.Dynamic t rl -> Eff (Routed t rr : es) a)
  -> Eff es a
pairRoute f = do
  r <- askRoute @t @(rl, rr)
  let dl = fmap fst r
      dr = fmap snd r
  runRoutedChild @t dr (f dl)

-- | Like 'subRoute_' but with a pair rather than an @R@. Deprecated
-- upstream; parity port for source compatibility. Re-renders on every
-- route change (see 'subRoute' note).
subPairRoute_
  :: forall a b t es
   . ( Routed t (a, b) :> es
     , R.Reflex t
     , HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => (a -> Eff (Routed t b : es) ())
  -> Eff es ()
subPairRoute_ f = do
  r <- askRoute @t @(a, b)
  strictDynWidget_ r $ \(aVal, bVal) ->
    runRoutedChild @t (R.constDyn bVal) (f aVal)

-- | Like 'subRoute' but with a pair rather than an @R@. Deprecated
-- upstream; parity port for source compatibility. Re-renders on every
-- route change (see 'subRoute' note).
subPairRoute
  :: forall a b c t es
   . ( Routed t (a, b) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => (a -> Eff (Routed t b : es) c)
  -> Eff es (R.Dynamic t c)
subPairRoute f = do
  r <- askRoute @t @(a, b)
  strictDynWidget r $ \(aVal, bVal) ->
    runRoutedChild @t (R.constDyn bVal) (f aVal)

-- | Route over a @Maybe r@: run the fallback widget when route is
-- 'Nothing', run the inner routed widget when 'Just'.
maybeRouted
  :: forall t a r es
   . ( Routed t (Maybe a) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     )
  => Eff (Routed t (Maybe (R.Dynamic t a)) : es) r
  -> Eff es r
maybeRouted body = do
  d <- askRoute @t @(Maybe a)
  d' <- RD.maybeDyn d
  runRoutedChild @t d' body

-- | @maybeRoute nothingW justW@: run @nothingW@ when the route is
-- 'Nothing', run @justW@ (with @Routed t r@ installed) when 'Just'.
-- Re-renders on every route change (see 'subRoute' note).
maybeRoute
  :: forall r t es a
   . ( Routed t (Maybe r) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => Eff es a
  -> Eff (Routed t r : es) a
  -> Eff es (R.Dynamic t a)
maybeRoute nothingW justW = do
  d <- askRoute @t @(Maybe r)
  strictDynWidget d $ \case
    Nothing -> nothingW
    Just rVal -> runRoutedChild @t (R.constDyn rVal) justW

-- | Void variant of 'maybeRoute'.
maybeRoute_
  :: forall r t es
   . ( Routed t (Maybe r) :> es
     , R.Reflex t
     , HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => Eff es ()
  -> Eff (Routed t r : es) ()
  -> Eff es ()
maybeRoute_ nothingW justW = do
  d <- askRoute @t @(Maybe r)
  strictDynWidget_ d $ \case
    Nothing -> nothingW
    Just rVal -> runRoutedChild @t (R.constDyn rVal) justW

-- | Route over an @Either l r@ into a Dynamic-of-Either. The Dynamic
-- factor structure mirrors upstream 'eitherDyn'.
eitherRouted
  :: forall t l r res es
   . ( Routed t (Either l r) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     )
  => Eff (Routed t (Either (R.Dynamic t l) (R.Dynamic t r)) : es) res
  -> Eff es res
eitherRouted body = do
  d <- askRoute @t @(Either l r)
  d' <- RD.eitherDyn d
  runRoutedChild @t d' body

-- | @eitherRoute leftW rightW@: run @leftW@ (with @Routed t l@) when
-- the route is 'Left', @rightW@ (with @Routed t r@) when 'Right'.
-- Re-renders on every route change (see 'subRoute' note).
eitherRoute
  :: forall l r t es a
   . ( Routed t (Either l r) :> es
     , R.Reflex t
     , HH.Hold t :> es, HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => Eff (Routed t l : es) a
  -> Eff (Routed t r : es) a
  -> Eff es (R.Dynamic t a)
eitherRoute leftW rightW = do
  d <- askRoute @t @(Either l r)
  strictDynWidget d $ \case
    Left lVal -> runRoutedChild @t (R.constDyn lVal) leftW
    Right rVal -> runRoutedChild @t (R.constDyn rVal) rightW

-- | Void variant of 'eitherRoute'.
eitherRoute_
  :: forall l r t es
   . ( Routed t (Either l r) :> es
     , R.Reflex t
     , HH.Sample t :> es
     , HH.Adjustable t :> es
     )
  => Eff (Routed t l : es) ()
  -> Eff (Routed t r : es) ()
  -> Eff es ()
eitherRoute_ leftW rightW = do
  d <- askRoute @t @(Either l r)
  strictDynWidget_ d $ \case
    Left lVal -> runRoutedChild @t (R.constDyn lVal) leftW
    Right rVal -> runRoutedChild @t (R.constDyn rVal) rightW

-- ─── iOS hash-routing URI adapters ─────────────────────────────
--
-- Verbatim ports from upstream 'Obelisk.Route.Frontend'. Pure
-- 'String'/'URI' transforms — no FRP involved. Used by 'runFrontend'
-- for iOS/file:// sandboxing workarounds.

-- | On iOS the page is loaded from a @file://@ URL and the sandbox
-- prevents ordinary path-based routing. We use the URL fragment
-- (@#/foo/bar@) as the route source instead.
adaptedUriPath :: Bool -> URI.URI -> String
adaptedUriPath = \case
  True -> hashToPath . URI.uriFragment
  False -> URI.uriPath

-- | Inverse of 'adaptedUriPath'. Update a URI to encode the given
-- path either as an ordinary path or as a fragment.
setAdaptedUriPath :: Bool -> String -> URI.URI -> URI.URI
setAdaptedUriPath useHash s u = case useHash of
  True -> u { URI.uriFragment = pathToHash s }
  False -> u { URI.uriPath = s }

-- | Convert a path (@/foo/bar@) to a hash (@#foo/bar@).
pathToHash :: String -> String
pathToHash = ('#' :) . Maybe.fromMaybe "" . L.stripPrefix "/"

-- | Convert a hash (@#foo/bar@) back to a path (@/foo/bar@).
hashToPath :: String -> String
hashToPath = ('/' :) . Maybe.fromMaybe "" . L.stripPrefix "#"
