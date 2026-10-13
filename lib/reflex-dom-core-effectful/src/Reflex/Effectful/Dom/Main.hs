{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Effectful-native port of @Reflex.Dom.Main@ — the bootstrap that runs
-- a widget in a live browser (via jsaddle-warp on GHC, or under the
-- ambient browser ctx on GHCJS).
--
-- Structure mirrors upstream 'Reflex.Dom.Main':
--
--   * 'mainWidget' / 'mainWidgetInBody' / 'mainWidgetWithHead' /
--     'mainWidgetWithCss' / 'mainWidgetInElementById' — entry points
--     from user code. Each is a thin wrapper around 'withDomHost' +
--     'attachWidget' that resolves the target 'DOM.Element' from the
--     current document.
--
--   * 'attachWidget' / 'attachWidget'' — the primitive that (a) sets
--     up a 'HydrationEnv' pointing at a fresh 'DocumentFragment', (b)
--     runs the guest widget under the full interpreter stack, (c)
--     swaps the built DOM into the parent element on the same frame,
--     and (d) starts the trigger-event drain loop.
--
--   * 'runJSM'' — the honest live interpreter for the 'JSM'' effect,
--     capturing the ambient 'JSContextRef' once at entry and reusing
--     it across every 'LiftJSM' / 'WithJSContext' / 'AskJSContext'
--     send.
--
-- Drain-loop rationale: the loop runs INSIDE the interpreter scope in a
-- forked thread so widget-scope 'Adjustable' children's replacement
-- callbacks still have live LocalEnv machinery when they fire from a
-- trigger. Same shape used in
-- "Reflex.Class" from reflex-effectful.
--
-- The hydration bootstrap family ('mainHydrationWidgetWithHead' /
-- 'attachHydrationWidgetWithFailure' / 'runHydrationWidgetWithHeadAndBody')
-- is also ported here. Hydration flow: (a) allocate a hydrationMode
-- 'IORef' seeded with 'HydrationMode_Hydrating' + a root-nodes ref;
-- (b) build the widget in a 'FloatingWidget' scope where per-subwidget
-- 'HydrationEnv's are installed by the caller's app callbacks — each
-- subwidget queues its delayed 'HydrationRunner' actions into
-- 'hydrationEnv_delayed'; (c) at end-of-assembly the outer widget
-- installs @runWithReplaceWidget (pure ()) (delayedAction <$ syncEv)@
-- so the delayed action fires when 'syncEvent' does; (d) postBuild
-- fires, then 'syncEvent' fires, triggering the delayed action which
-- runs each root's 'HydrationRunner' and finally flips the mode to
-- 'HydrationMode_Immediate' + runs the user's 'switchoverAction'.
--
-- 'mainWidgetWithHead'' (upstream @Reflex/Dom/Main.hs@ lines 287–291)
-- — the interleaved mutual-recursion head/body variant — is ported
-- here as 'mainWidgetWithHead\''. Upstream uses @rec@ to mutually
-- bind @hOut@/@bOut@ so the head widget can observe outputs from the
-- body widget and vice versa; the effectful port relies on the
-- native 'MonadFix' instance for 'Eff es' (from the effectful
-- library) so @rec@ / @mfix@ work directly inside the widget scope.
module Reflex.Effectful.Dom.Main
  ( -- * User-facing entry points (immediate mode)
    mainWidget
  , mainWidgetInBody
  , mainWidgetWithHead
  , mainWidgetWithHead'
  , mainWidgetWithCss
  , mainWidgetInElementById
    -- * User-facing entry points (hydration mode)
  , mainHydrationWidgetWithHead
  , mainHydrationWidgetWithHead'
  , mainHydrationWidgetWithSwitchoverAction
  , mainHydrationWidgetWithSwitchoverAction'
  , mainHydrationWidgetWithSwitchoverActionWithFailure'
    -- * Primitive attach (immediate)
  , attachWidget
  , attachWidget'
  , runImmediateWidgetWithHeadAndBody
    -- * Primitive attach (hydration)
  , attachHydrationWidget
  , attachHydrationWidgetWithFailure
  , runHydrationWidgetWithHeadAndBody
  , runHydrationWidgetWithHeadAndBodyWithFailure
    -- * Widget-scope constraint synonyms
  , WidgetE
  , HydrationWidgetE
  , FloatingWidgetE
  , DomCoreWidgetE
    -- * JSM' interpreter (honest, live)
  , runJSM'
    -- * Host thread
  , withDomHost
  ) where

import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.Chan             (newChan)
import           Control.Concurrent.STM              (STM, atomically, orElse,
                                                      retry)
import           Control.Concurrent.STM.TChan        (TChan, tryReadTChan)
import           Control.Concurrent.STM.TVar         (TVar, newTVarIO,
                                                      readTVar)
import           Control.Monad                       (forM_, unless)
import           Control.Monad.IO.Class              (liftIO)
import           Data.ByteString                     (ByteString)
import           Data.Dependent.Sum                  (DSum (..))
import           Data.IORef                          (IORef, modifyIORef',
                                                      newIORef, readIORef,
                                                      writeIORef)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeUtf8)
import           System.Environment                  (lookupEnv)

import           Effectful                           (Eff, IOE, runEff, (:>))
import           Effectful.Dispatch.Dynamic          (interpret)
import           Effectful.Reader.Static             (runReader)

import           GHCJS.DOM                           (currentDocumentUnchecked)
import           GHCJS.DOM.Document                  (createDocumentFragment,
                                                      getBodyUnchecked,
                                                      getHeadUnchecked)
import           GHCJS.DOM.Element                   (setInnerHTML)
import           GHCJS.DOM.NonElementParentNode      (getElementByIdUnchecked)
import           GHCJS.DOM.Node                      (appendChild_,
                                                      getOwnerDocumentUnchecked,
                                                      toNode)
import qualified GHCJS.DOM.Types                     as DOM
import           GHCJS.DOM.Types                     (JSContextRef, JSM,
                                                      toDocument)

import           Language.Javascript.JSaddle         (askJSM, runJSM)
import qualified Language.Javascript.JSaddle.Warp    as Warp

import qualified Reflex.Class              as R

import           Reflex.Class ()
import qualified Reflex.Class    as Prim
import           Reflex.Class    (EventTriggerRef,
                                                      SomeTriggerFire (..))
import           Reflex.Class      (Global)
import           Reflex.Class  (Adjustable, runAdjustable,
                                                      runWithReplaceWidget)
import           Reflex.Class        (Hold, runHold)
import           Reflex.Class    (NotReady, runNotReady)
import           Reflex.Class
                     ( NativeFireCommand
                     , PerformEvent
                     , runNativeFireCommand
                     , runPerformEvent
                     )
import           Reflex.Class
                     ( NativeHostEnv (..)
                     , PostBuild
                     , newNativeHostEnv
                     , runPostBuild
                     )
import           Reflex.Class      (Sample, runSample)
import           Reflex.Class
                     ( TriggerEvent
                     , TriggerFiringChan
                     , TriggerInvocation (..)
                     , newTriggerEvent
                     , newTriggerEventChan
                     , runTriggerEventWithChan
                     )
import           Reflex.Class              (runTimeline)

import           Reflex.Dom.Core
                     ( DomBuilder
                     , HasDocument
                     )
import           Reflex.Dom.Core
                     (runHydrationRunnerWithFailure)
import           Reflex.Dom.Core
                     ( runDomBuilderHydration
                     , runDomBuilderImmediate
                     , runHasDocumentGhcjs
                     , runHasDocumentHydration
                     )
import           Reflex.Dom.Core
                                                      (runDomRenderHook)
import           Reflex.Dom.Core
                     ( GhcjsDomSpace
                     , HydrationDomSpace
                     )
import           Reflex.Dom.Core
                     ( DelayedHydration
                     , DomE
                     , DomRenderHook
                     , HydrationEnv (..)
                     , HydrationMode (..)
                     , JSM' (..)
                     , RunnerAction (..)
                     )

-- ---------------------------------------------------------------------
-- runJSM' — the honest live interpreter for the JSM' effect
-- ---------------------------------------------------------------------

-- | Interpret 'JSM'' against an ambient 'JSContextRef'.
--
-- Every 'LiftJSM' request runs its 'JSM' body under the captured ctx
-- via 'runJSM'; 'WithJSContext' hands the ctx to a raw-IO callback;
-- 'AskJSContext' returns the ctx directly.
--
-- The ctx is captured at interpreter entry (once) and closed over for
-- the interpreter's lifetime — matching the "grab once, reuse across
-- every subsequent send" pattern used by
-- 'Reflex.Dom.Core'.
runJSM'
  :: (IOE :> es)
  => JSContextRef
  -> Eff (JSM' : es) a
  -> Eff es a
runJSM' ctx = interpret $ \_ -> \case
  LiftJSM jsm     -> liftIO (runJSM jsm ctx)
  WithJSContext k -> liftIO (k ctx)
  AskJSContext    -> pure ctx

-- ---------------------------------------------------------------------
-- attachWidget / attachWidget'
-- ---------------------------------------------------------------------

-- | Run a widget scoped to a parent element. Returns the widget's
-- result (usually @()@). Discards the 'NativeFireCommand' handle.
attachWidget
  :: forall e a. DOM.IsElement e
  => e
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
                 , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
                 , HasDocument GhcjsDomSpace :> es
                 ) => Eff es a)
  -> JSM a
attachWidget rootElement w = fst <$> attachWidget' @e rootElement w

-- | Like 'attachWidget' but also returns the 'NativeFireCommand' — the
-- runtime uses this to fire triggers uniformly through
-- 'fireEventsAndRead'. Matches upstream's shape.
--
-- Sequence:
--
--   1. Grab the ambient jsaddle ctx via 'askJSM'.
--   2. Look up the owner 'DOM.Document' from the root element.
--   3. Create a fresh 'DocumentFragment' to build into.
--   4. Bootstrap the interpreter stack around the widget.
--   5. Fire postBuild through the native fire command WHILE the
--      fragment is still detached (matches upstream
--      'attachImmediateWidget'; postBuild handlers reading DOM
--      geometry see the detached fragment, not the live tree).
--   6. Swap the fragment into the root element.
--   7. Fork the drain loop.
attachWidget'
  :: forall e a. DOM.IsElement e
  => e
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
                 , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
                 , HasDocument GhcjsDomSpace :> es
                 ) => Eff es a)
  -> JSM (a, NativeFireCommand)
attachWidget' rootElement guest = do
  ctx <- askJSM
  doc <- getOwnerDocumentUnchecked rootElement
  df  <- createDocumentFragment doc
  liftIO $ do
    unreadyChildren <- newIORef 0
    delayed         <- newIORef (RunnerAction (pure ()) :: DelayedHydration (EffTimeline Global))
    hydrationMode   <- newIORef HydrationMode_Immediate
    eventsChan      <- newChan
    nhe             <- newNativeHostEnv
    doneVar         <- newTVarIO False
    chanRef         <- newIORef Nothing
    let hEnv :: HydrationEnv (EffTimeline Global)
        hEnv = HydrationEnv
          { hydrationEnv_document        = toDocument doc
          , hydrationEnv_parent          = Left (toNode df)
          , hydrationEnv_unreadyChildren = unreadyChildren
          , hydrationEnv_commitAction    = pure ()
          , hydrationEnv_hydrationMode   = hydrationMode
          , hydrationEnv_switchover      = R.never
          , hydrationEnv_delayed         = delayed
          , hydrationEnv_events          = eventsChan
          }
    (result, fireCmd) <- runInterpreterStack ctx nhe chanRef hEnv guest
    -- Fire postBuild BEFORE the DOM swap, matching upstream
    -- 'attachImmediateWidget' (reflex-dom-core Main.hs:337-338). This
    -- is deliberate: postBuild handlers that read DOM geometry
    -- (bounding rects, computed style) must observe the detached
    -- fragment, not the live tree, to match upstream semantics.
    _ <- runNativeFireCommand fireCmd
           [SomeTriggerFire (nheTrigger nhe) ()]
           (pure ())
    -- DOM swap: replace the root element's contents with the built
    -- fragment. Must happen under the ctx so setInnerHTML +
    -- appendChild reach the browser via jsaddle.
    _ <- runJSM (attachToRoot rootElement df) ctx
    -- Fork the drain loop. The forked thread inherits the ctx
    -- indirectly via the interpreter closures registered inside the
    -- widget scope (subscribe callbacks close over the ctx captured
    -- by 'runJSM''); the drainer itself only manipulates the chan +
    -- fires triggers, so it does not need ctx.
    _ <- forkIO (drainUntilDone chanRef doneVar)
    pure (result, fireCmd)

-- | The interpreter stack composition, factored out so its type is
-- nameable (inlining into 'attachWidget'' trips ambiguous-type errors
-- because 'guest' has a rank-2 type constrained by 'DomE').
--
-- Layered outside-in (innermost first):
--
--   * 'runPerformEvent'          — peels PerformEvent, returns FireCmd
--   * 'runHold' / 'runSample'    — FRP read primitives
--   * 'runNotReady'              — no-op (upstream mount-stall)
--   * 'runAdjustable'            — replacement children
--   * 'runTriggerEventWithChan'  — trigger enqueue → chanRef
--   * 'runPostBuild'             — one-shot postBuild trigger
--   * 'runDomRenderHook'         — schedules JSM on Event fire
--   * 'runJSM''                  — jsaddle ctx bridge
--   * 'runReader hEnv'           — HydrationEnv
--   * 'runTimeline'              — Spider-side timeline glue
--   * 'runReader nhe'            — NativeHostEnv (postBuild trigger)
--   * 'runEff'                   — IOE base
runInterpreterStack
  :: forall a. ()
  => JSContextRef
  -> NativeHostEnv
  -> IORef (Maybe TriggerFiringChan)
  -> HydrationEnv (EffTimeline Global)
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
                 , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
                 , HasDocument GhcjsDomSpace :> es
                 ) => Eff es a)
  -> IO (a, NativeFireCommand)
runInterpreterStack ctx nhe chanRef hEnv guest =
    runEff
  . runReader nhe
  . runTimeline @(EffTimeline Global)
  . runReader hEnv
  . runJSM' ctx
  . runPostBuild
  . runTriggerEventWithChan chanRef
  . runDomRenderHook
  . runAdjustable
  . runNotReady @(EffTimeline Global)
  . runSample
  . runHold
  . runHasDocumentGhcjs
  . runDomBuilderImmediate @(EffTimeline Global)
  $ runPerformEvent guest

-- | Attach a 'DocumentFragment' to a root element, replacing its
-- contents (matches upstream 'replaceElementContents').
attachToRoot :: DOM.IsElement e => e -> DOM.DocumentFragment -> JSM ()
attachToRoot e df = do
  setInnerHTML e ("" :: String)
  appendChild_ e df

-- ---------------------------------------------------------------------
-- Drain loop
-- ---------------------------------------------------------------------

-- | Chan drain loop. Blocks via STM on either the next batch of
-- trigger firings or the done-signal; fires each batch through
-- 'Prim.fireEvents', invokes completion callbacks, exits on done.
--
-- Same shape as 'Reflex.Class' in
-- reflex-effectful — the drainer must stay live for the lifetime of
-- the widget, so 'attachWidget'' forks it and lets it run
-- indefinitely (widgets do not currently expose a termination
-- signal — matches upstream, which also never terminates its
-- @processAsyncEvents@ 'forkIO' thread).
drainUntilDone
  :: IORef (Maybe TriggerFiringChan)
  -> TVar Bool
  -> IO ()
drainUntilDone chanRef doneVar = loop
  where
    loop = do
      mChan <- readIORef chanRef
      action <- atomically $ case mChan of
        Nothing   -> Nothing <$ awaitDone doneVar
        Just chan -> (Just <$> readBatch chan)
                     `orElse`
                     (Nothing <$ awaitDone doneVar)
      case action of
        Nothing      -> pure ()
        Just firings -> do
          forM_ firings $ \(trigRef :=> TriggerInvocation a _cb) ->
            Prim.fireEvents [(trigRef, a)]
          forM_ firings $ \(_ :=> TriggerInvocation _ cb) -> cb
          loop

readBatch
  :: TChan [DSum (EventTriggerRef Global) TriggerInvocation]
  -> STM [DSum (EventTriggerRef Global) TriggerInvocation]
readBatch chan = do
  m <- tryReadTChan chan
  case m of
    Just batch -> pure batch
    Nothing    -> retry

awaitDone :: TVar Bool -> STM ()
awaitDone dv = do
  d <- readTVar dv
  unless d retry

-- ---------------------------------------------------------------------
-- User-facing entry points
-- ---------------------------------------------------------------------

-- | Run a widget attached to the document's @body@ element.
--
-- On GHC: spins up jsaddle-warp on the port from the
-- @JSADDLE_WARP_PORT@ env var (default @3911@) — the user opens a
-- browser tab to @http:\/\/localhost:PORT@ manually. On GHCJS: runs
-- directly under the browser's ambient ctx.
mainWidget
  :: (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> IO ()
mainWidget w = withDomHost $ do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  attachWidget body w

-- | Alias for 'mainWidget'. Provided for API parity with upstream +
-- to make the "attach to body" intent explicit at call sites.
mainWidgetInBody
  :: (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> IO ()
mainWidgetInBody = mainWidget

-- | Run two widgets — one attached to @head@, one to @body@.
mainWidgetWithHead
  :: (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> IO ()
mainWidgetWithHead h b = withDomHost $ do
  doc  <- currentDocumentUnchecked
  hd   <- getHeadUnchecked doc
  body <- getBodyUnchecked doc
  _ <- attachWidget hd h
  attachWidget body b

-- | Run a widget with an inline @<style>@ block prepended to @head@.
mainWidgetWithCss
  :: ByteString
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> IO ()
mainWidgetWithCss css w = withDomHost $ do
  doc  <- currentDocumentUnchecked
  hd   <- getHeadUnchecked doc
  setInnerHTML hd
    ("<style>" <> T.unpack (decodeUtf8 css) <> "</style>")
  body <- getBodyUnchecked doc
  attachWidget body w

-- | Run a widget attached to a specific element (looked up by @id@).
mainWidgetInElementById
  :: Text
  -> (forall es. ( DomE (EffTimeline Global) GhcjsDomSpace es
             , DomBuilder (EffTimeline Global) GhcjsDomSpace :> es
             , HasDocument GhcjsDomSpace :> es
             ) => Eff es ())
  -> IO ()
mainWidgetInElementById eid w = withDomHost $ do
  doc  <- currentDocumentUnchecked
  root <- getElementByIdUnchecked doc eid
  attachWidget root w

-- ---------------------------------------------------------------------
-- Host-thread setup
-- ---------------------------------------------------------------------

-- | Bring up the jsaddle host and run the supplied JSM action inside
-- it. On GHC this is @jsaddle-warp@'s 'Warp.run' on a port from
-- @JSADDLE_WARP_PORT@ (default @3003@); on GHCJS the guest 'JSM' runs
-- directly under the browser's ambient ctx.
--
-- Body matches upstream @Reflex.Dom.Internal.run@ verbatim on the
-- jsaddle-warp branch: partial 'read' semantics (matches upstream —
-- crashes on unparseable env value rather than silently defaulting),
-- log line preserved, default port 3003.
--
-- Cross-platform CPP dispatch is handled by @Reflex.Dom.Internal.run@
-- (in the @reflex-dom@ umbrella package). This fn is jsaddle-warp-only;
-- callers wanting GHCJS/WKWebView/Android/WASM should route through the
-- umbrella's @mainWidget*@ instead of this one.
withDomHost :: JSM () -> IO ()
withDomHost act = do
  port <- maybe 3003 read <$> lookupEnv "JSADDLE_WARP_PORT"
  putStrLn $ "Running jsaddle-warp server on port " <> show port
  Warp.run port act

-- ---------------------------------------------------------------------
-- Hydration bootstrap: widget-scope constraint synonyms
-- ---------------------------------------------------------------------

-- | Effect row required by user immediate-mode widget code. Analog of
-- upstream's @Widget x@ = @ImmediateDomBuilderT DomTimeline
-- (DomCoreWidget x)@ — a widget that lives under a specific
-- 'HydrationEnv' (with @hydrationMode = HydrationMode_Immediate@) and
-- can build DOM in the 'GhcjsDomSpace'.
--
-- Same shape as 'HydrationWidgetE' but pinned to 'GhcjsDomSpace'
-- (upstream distinguishes 'Widget' vs 'HydrationWidget' along this
-- exact axis).
type WidgetE t es =
  ( DomE t GhcjsDomSpace es
  , DomBuilder t GhcjsDomSpace :> es
  , HasDocument GhcjsDomSpace :> es
  )

-- | Effect row required by user hydration widget code. Analog of
-- upstream's @HydrationWidget x@ = @HydrationDomBuilderT
-- HydrationDomSpace DomTimeline (DomCoreWidget x)@ — a widget that
-- lives under a specific 'HydrationEnv' and can build DOM in the
-- hydration space.
--
-- Structurally equivalent to @'DomE' t 'HydrationDomSpace' es@ with
-- the @DomBuilder@ + @HasDocument@ interpreters installed on top —
-- pinned to 'HydrationDomSpace' so upstream's 'placeRawElement' /
-- 'wrapRawElement' no-op semantics are preserved (see
-- 'runDomBuilderHydration' in
-- "Reflex.Dom.Core").
type HydrationWidgetE t es =
  ( DomE t HydrationDomSpace es
  , DomBuilder t HydrationDomSpace :> es
  , HasDocument HydrationDomSpace :> es
  )

-- | Effect row required by "floating" (parent-less) hydration widget
-- code. Analog of upstream's @FloatingWidget x@ = @TriggerEventT
-- DomTimeline (DomCoreWidget x)@ — a widget that has full FRP + JSM
-- access but is NOT bound to any DOM parent (the caller installs the
-- 'HydrationEnv' per-subwidget via 'hydrateDom').
--
-- Equal to 'DomCoreWidgetE' plus the effects the outer runtime
-- installs before dispatching to per-subwidget hydration:
-- 'DomRenderHook' (JSM render-hook queue),
-- 'TriggerEvent' (async trigger firing),
-- 'NotReady' (mount stall),
-- 'Adjustable' (replacement children — needed for the runWithReplace
-- that schedules the delayed hydration action at switchover).
type FloatingWidgetE t es =
  ( DomCoreWidgetE t es
  , DomRenderHook t :> es
  , TriggerEvent t :> es
  , NotReady t :> es
  , Adjustable t :> es
  )

-- | Effect row required by "core" widget code — the base FRP + JSM
-- stack. Analog of upstream's @DomCoreWidget x@ = @PostBuildT
-- DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline
-- DomHost))@.
--
-- The port lists effects flat (upstream stacks them via monad
-- transformers): 'PostBuild' + 'PerformEvent' for the FRP scheduling
-- primitives, 'Hold' + 'Sample' for behavior reads, 'JSM'' for
-- jsaddle bridging, 'IOE' for underlying IO.
type DomCoreWidgetE t es =
  ( PostBuild t :> es
  , PerformEvent t :> es
  , Hold t :> es
  , Sample t :> es
  , JSM' :> es
  , IOE :> es
  )

-- ---------------------------------------------------------------------
-- Hydration bootstrap: attach primitives
-- ---------------------------------------------------------------------

-- | Convenience wrapper: 'attachHydrationWidgetWithFailure' with the
-- default no-op onFailure hook. Port of upstream lines 92–103.
attachHydrationWidget
  :: JSM ()
  -- ^ 'switchoverAction' — JSM fired once, at the moment hydration
  -- completes and the widget flips to immediate mode.
  -> (forall es. FloatingWidgetE (EffTimeline Global) es
         => R.Event (EffTimeline Global) ()
         -> IORef HydrationMode
         -> IORef [(Node, HydrationEnv (EffTimeline Global), DelayedHydration (EffTimeline Global))]
         -> Eff es a)
  -> JSM (a, NativeFireCommand)
attachHydrationWidget = attachHydrationWidgetWithFailure (pure ())

-- | Bootstrap the hydration path: allocate the shared hydrationMode +
-- rootNodesRef + events chan, run the caller's floating widget, then
-- fire postBuild + syncEvent to drive the delayed hydration actions.
--
-- Port of upstream lines 105–137
-- (@attachHydrationWidgetWithFailure@). Semantically identical:
--
--   1. Allocate 'IORef' seeded with 'HydrationMode_Hydrating'.
--   2. Allocate the root-nodes ref (a list of @(Node, HydrationRunner)@
--      that per-subwidget hydration will push into).
--   3. Allocate the trigger-events chan + native postBuild env.
--   4. Bootstrap the interpreter stack around the widget, installing
--      an outer @runWithReplaceWidget@ that fires the delayed action
--      when 'syncEvent' fires.
--   5. Fire the postBuild trigger through the native fire command.
--   6. Fire the 'syncEvent' trigger through the same fire command.
--   7. Fork the drain loop.
--
-- Note on shape divergence from upstream: upstream returns
-- @IO (a, FireCommand DomTimeline DomHost)@ — the widget lives entirely
-- in @IO@ because the outer host is @runDomHost@ (SpiderHost). The
-- port returns @JSM (a, NativeFireCommand)@ — we run inside a live
-- 'JSM' host so the initial ctx grab (@askJSM@) and the DOM lookups
-- ('currentDocumentUnchecked') happen inside 'JSM' before we drop to
-- IO. Semantically the two are equivalent.
attachHydrationWidgetWithFailure
  :: forall a
   . IO ()
  -- ^ 'onFailure' — fires when hydration diverges from the prerendered
  -- DOM at switchover. Threaded down to 'runHydrationRunnerWithFailure'
  -- per-subwidget.
  -> JSM ()
  -- ^ 'switchoverAction' — JSM fired once, at the moment hydration
  -- completes and the widget flips to immediate mode.
  -> (forall es. FloatingWidgetE (EffTimeline Global) es
         => R.Event (EffTimeline Global) ()
         -> IORef HydrationMode
         -> IORef [(Node, HydrationEnv (EffTimeline Global), DelayedHydration (EffTimeline Global))]
         -> Eff es a)
  -> JSM (a, NativeFireCommand)
attachHydrationWidgetWithFailure onFailure switchoverAction widget = do
  ctx <- askJSM
  liftIO $ do
    hydrationMode <- newIORef HydrationMode_Hydrating
    rootNodesRef  <- newIORef ([] :: [(Node, HydrationEnv (EffTimeline Global), DelayedHydration (EffTimeline Global))])
    eventsChan    <- newChan
    nhe           <- newNativeHostEnv
    doneVar       <- newTVarIO False
    chanRef       <- newIORef Nothing
    -- Allocate the sync-event trigger INSIDE the interpreter scope via
    -- 'newTriggerEvent', matching upstream's mdo shape
    -- (@Reflex/Dom/Main.hs@ lines 123–129). The @fireSync@ callback
    -- escapes the widget scope via IORef so the outer bootstrap can
    -- fire it synchronously after postBuild, matching upstream line
    -- 129 (@liftIO $ fireSync ()@).
    fireSyncRef <- newIORef (\() -> pure () :: IO ())
    (result, fireCmd) <-
      runFloatingInterpreterStack ctx nhe chanRef $ do
        (syncEv, fireSync) <- newTriggerEvent @(EffTimeline Global) @_ @()
        liftIO $ writeIORef fireSyncRef fireSync
        a <- widget syncEv hydrationMode rootNodesRef
        -- Post-widget: read the root nodes the guests queued, and
        -- schedule the delayed hydration action so it fires at
        -- switchover. Matches upstream lines 125 + 130–136.
        rootNodes <- liftIO (readIORef rootNodesRef)
        let delayedAction = do
              forM_ (reverse rootNodes) $ \(rootNode, hEnv, runner) ->
                -- Install the subwidget's own HydrationEnv so the
                -- runner scope's 'Reader (HydrationEnv t)' resolves to
                -- the env the subwidget was BUILT under (not some
                -- ambient bootstrap env — there is none at this
                -- scope). This matches upstream where the runner is
                -- driven with the same @m@ the widget was built in.
                runReader hEnv $
                  runHydrationRunnerWithFailure @(EffTimeline Global)
                    runner onFailure Nothing rootNode
              liftIO $ writeIORef hydrationMode HydrationMode_Immediate
              -- Bridge the caller-supplied JSM switchoverAction into
              -- the effectful world via the ambient runJSM'
              -- interpreter — same ctx the widget scope uses.
              liftIO $ runJSM switchoverAction ctx
        _ <- runWithReplaceWidget @(EffTimeline Global)
               (pure ()) (delayedAction <$ syncEv)
        pure a
    -- Fire postBuild first (matches upstream line 128).
    _ <- runNativeFireCommand fireCmd
           [SomeTriggerFire (nheTrigger nhe) ()]
           (pure ())
    -- Then fire syncEvent (matches upstream line 129). Uses the
    -- @fireSync@ callback the widget scope stashed via IORef —
    -- 'newTriggerEvent' fires synchronously through 'Prim.fireEvents'
    -- (see 'Reflex.Class'), so
    -- this call has the same synchronous semantics as upstream's
    -- @liftIO $ fireSync ()@.
    fireSync <- readIORef fireSyncRef
    fireSync ()
    -- Fork the drain loop for downstream async triggers.
    _ <- forkIO (drainUntilDone chanRef doneVar)
    pure (result, fireCmd)

-- | Interpreter stack for the FloatingWidget scope — everything a
-- hydration widget scope needs EXCEPT the per-subwidget
-- 'Reader (HydrationEnv t)' / 'DomBuilder' / 'HasDocument' (those get
-- installed inside 'hydrateDom' when the caller descends into a
-- specific parent Node).
--
-- Layered outside-in (innermost first) — same order as
-- 'runInterpreterStack' for immediate mode, minus the two DomBuilder-
-- shaped interpreters:
--
--   * 'runPerformEvent'          — peels PerformEvent, returns FireCmd
--   * 'runHold' / 'runSample'    — FRP read primitives
--   * 'runNotReady'              — no-op (upstream mount-stall)
--   * 'runAdjustable'            — replacement children (used by the
--                                  post-widget @runWithReplaceWidget@
--                                  that schedules the delayed action)
--   * 'runTriggerEventWithChan'  — trigger enqueue → chanRef
--   * 'runPostBuild'             — one-shot postBuild trigger
--   * 'runDomRenderHook'         — schedules JSM on Event fire
--   * 'runJSM''                  — jsaddle ctx bridge
--   * 'runTimeline'              — Spider-side timeline glue
--   * 'runReader nhe'            — NativeHostEnv (postBuild trigger)
--   * 'runEff'                   — IOE base
runFloatingInterpreterStack
  :: forall a. ()
  => JSContextRef
  -> NativeHostEnv
  -> IORef (Maybe TriggerFiringChan)
  -> (forall es. FloatingWidgetE (EffTimeline Global) es => Eff es a)
  -> IO (a, NativeFireCommand)
runFloatingInterpreterStack ctx nhe chanRef guest =
    runEff
  . runReader nhe
  . runTimeline @(EffTimeline Global)
  . runJSM' ctx
  . runPostBuild
  . runTriggerEventWithChan chanRef
  . runDomRenderHook
  . runAdjustable
  . runNotReady @(EffTimeline Global)
  . runSample
  . runHold
  $ runPerformEvent guest

-- ---------------------------------------------------------------------
-- Hydration bootstrap: two-fragment head/body widget assembly
-- ---------------------------------------------------------------------

-- | Convenience wrapper: 'runHydrationWidgetWithHeadAndBodyWithFailure'
-- with the default no-op onFailure hook. Port of upstream lines
-- 146–154.
runHydrationWidgetWithHeadAndBody
  :: JSM ()
  -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> (forall esF
        . FloatingWidgetE (EffTimeline Global) esF
        =>    -- appendHead
              (forall esH. HydrationWidgetE (EffTimeline Global) esH
                    => Eff esH ()) -> Eff esF ()
        ->    -- appendBody
              ((forall esH. HydrationWidgetE (EffTimeline Global) esH
                    => Eff esH ()) -> Eff esF ())
        -> Eff esF ())
  -- ^ App callback: given @(appendHead, appendBody)@ dispatchers,
  -- assemble a floating widget that dispatches head/body content
  -- through the two dispatchers.
  -> JSM ()
runHydrationWidgetWithHeadAndBody =
  runHydrationWidgetWithHeadAndBodyWithFailure (pure ())

-- | Full head+body hydration bootstrap. Port of upstream lines
-- 156–192 (@runHydrationWidgetWithHeadAndBodyWithFailure@).
--
--   1. Look up the document's @<head>@ and @<body>@ elements.
--   2. Bootstrap the floating widget via
--      'attachHydrationWidgetWithFailure'; inside the widget lambda —
--      where the shared hydrationMode + rootNodesRef + syncEvent are
--      in scope — build the head/body dispatchers as closures that
--      install a fresh 'HydrationEnv' per subwidget and push
--      @(node, delayedRunner)@ into the shared list.
--   3. Invoke the caller's 'app' callback with the two closures.
--
-- Shape divergence from upstream: upstream's 'app' callback is
-- @(forall c. HydrationWidget () c -> FloatingWidget () c)@ — each
-- dispatcher is polymorphic in the widget result @c@ per call. The
-- port monomorphises to @()@ result at each dispatch call. This
-- matches how the family is actually used in
-- 'mainHydrationWidgetWithHead\'' + friends, which invoke each
-- dispatcher exactly once on a @()@-returning widget. Callers who
-- need to thread a non-@()@ result across dispatch can lift it via
-- 'IORef' capture inside the dispatcher's scope.
runHydrationWidgetWithHeadAndBodyWithFailure
  :: IO ()
  -- ^ 'onFailure' — fires when hydration diverges from prerendered DOM.
  -> JSM ()
  -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> (forall esF
        . FloatingWidgetE (EffTimeline Global) esF
        =>    -- appendHead
              (forall esH. HydrationWidgetE (EffTimeline Global) esH
                    => Eff esH ()) -> Eff esF ()
        ->    -- appendBody
              ((forall esH. HydrationWidgetE (EffTimeline Global) esH
                    => Eff esH ()) -> Eff esF ())
        -> Eff esF ())
  -> JSM ()
runHydrationWidgetWithHeadAndBodyWithFailure onFailure switchoverAction app = do
  globalDoc   <- currentDocumentUnchecked
  headElement <- getHeadUnchecked globalDoc
  bodyElement <- getBodyUnchecked globalDoc
  _ <- attachHydrationWidgetWithFailure onFailure switchoverAction $
         \syncEv hydrationMode rootNodesRef -> do
           let hydrateAt
                 :: forall esF
                  . FloatingWidgetE (EffTimeline Global) esF
                 => Node
                 -> (forall esH. HydrationWidgetE (EffTimeline Global) esH => Eff esH ())
                 -> Eff esF ()
               hydrateAt parent guest =
                 hydrateSubwidget globalDoc hydrationMode syncEv
                   rootNodesRef parent guest
           app (hydrateAt (toNode headElement))
               (hydrateAt (toNode bodyElement))
  pure ()

-- | Per-subwidget hydration: install a fresh 'HydrationEnv' pointing
-- at a specific parent 'Node', run the guest widget inside it via
-- 'runDomBuilderHydration' + 'runHasDocumentHydration', and push
-- @(parent, delayedAction)@ into the shared root-nodes ref so the
-- outer bootstrap can drive the delayed 'HydrationRunner' at
-- switchover.
--
-- Port of upstream lines 171–189
-- (@hydrateDom :: DOM.Node -> HydrationWidget () c -> FloatingWidget () c@).
hydrateSubwidget
  :: forall es
   . FloatingWidgetE (EffTimeline Global) es
  => DOM.Document
  -> IORef HydrationMode
  -> R.Event (EffTimeline Global) ()
  -> IORef [(Node, HydrationEnv (EffTimeline Global), DelayedHydration (EffTimeline Global))]
  -> Node
  -> (forall esH. HydrationWidgetE (EffTimeline Global) esH => Eff esH ())
  -> Eff es ()
hydrateSubwidget globalDoc hydrationMode syncEv rootNodesRef parent guest = do
  delayed         <- liftIO $ newIORef
    (RunnerAction (pure ()) :: DelayedHydration (EffTimeline Global))
  unreadyChildren <- liftIO $ newIORef 0
  -- Grab the ambient chan so the guest's TriggerEvent registrations
  -- push into the same chan the outer runtime drains.
  (chan, _)       <- newTriggerEventChan @(EffTimeline Global)
  let hEnv :: HydrationEnv (EffTimeline Global)
      hEnv = HydrationEnv
        { hydrationEnv_document        = globalDoc
        , hydrationEnv_parent          = Left parent
        , hydrationEnv_unreadyChildren = unreadyChildren
        , hydrationEnv_commitAction    = pure ()
        , hydrationEnv_hydrationMode   = hydrationMode
        , hydrationEnv_switchover      = syncEv
        , hydrationEnv_delayed         = delayed
        , hydrationEnv_events          = chan
        }
  runReader hEnv
    . runHasDocumentHydration
    . runDomBuilderHydration @(EffTimeline Global)
    $ guest
  -- After the guest builds, snapshot the queued delayed action and
  -- push (parent, delayed) into the shared root list. Matches
  -- upstream lines 186–189.
  runner <- liftIO $ readIORef delayed
  -- Push (parent, hEnv, runner) — the hEnv the subwidget was built
  -- under travels with the queued runner so the drain site can
  -- re-install it (the runner's rank-N body needs
  -- 'Reader (HydrationEnv t)' at specialise time).
  liftIO $ modifyIORef' rootNodesRef ((parent, hEnv, runner) :)


-- ---------------------------------------------------------------------
-- Hydration bootstrap: user-facing entry points
-- ---------------------------------------------------------------------

-- | Bootstrap a hydration app with independent head + body widgets.
-- Port of upstream line 66 (@mainHydrationWidgetWithHead@).
mainHydrationWidgetWithHead
  :: (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> IO ()
mainHydrationWidgetWithHead h b =
  mainHydrationWidgetWithHead' h b

-- | Performance-tweak variant of 'mainHydrationWidgetWithHead' — the
-- widget-result type is monomorphised to @()@ so users can share
-- 'HydrationEnv' state between the head and body if desired. Port of
-- upstream lines 70–72.
mainHydrationWidgetWithHead'
  :: (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> IO ()
mainHydrationWidgetWithHead' =
  mainHydrationWidgetWithSwitchoverAction' (pure ())

-- | Variant that fires an additional 'JSM' action once, at the moment
-- hydration completes (before the widget flips to immediate mode).
-- Port of upstream line 75 (@mainHydrationWidgetWithSwitchoverAction@).
mainHydrationWidgetWithSwitchoverAction
  :: JSM ()
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> IO ()
mainHydrationWidgetWithSwitchoverAction sw h b =
  mainHydrationWidgetWithSwitchoverAction' sw h b

-- | Performance-tweak variant of
-- 'mainHydrationWidgetWithSwitchoverAction'. Port of upstream lines
-- 79–81.
mainHydrationWidgetWithSwitchoverAction'
  :: JSM ()
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> IO ()
mainHydrationWidgetWithSwitchoverAction' =
  mainHydrationWidgetWithSwitchoverActionWithFailure' (pure ())

-- | Full-featured hydration bootstrap: caller supplies both the
-- 'onFailure' IO hook (fired when hydration diverges from the
-- prerendered DOM) and the 'switchoverAction' JSM (fired exactly once
-- at switchover). Port of upstream lines 84–90.
mainHydrationWidgetWithSwitchoverActionWithFailure'
  :: IO ()
  -> JSM ()
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> (forall es. HydrationWidgetE (EffTimeline Global) es => Eff es ())
  -> IO ()
mainHydrationWidgetWithSwitchoverActionWithFailure' onFailure sw h b =
  withDomHost $
    runHydrationWidgetWithHeadAndBodyWithFailure onFailure sw $
      \appendHead appendBody -> do
        -- Upstream calls each dispatcher exactly once with the
        -- caller's head + body widgets. Port shape mirrors that:
        -- appendHead runs the head widget in a HydrationWidget scope
        -- rooted at <head>; appendBody runs the body widget rooted at
        -- <body>. Both dispatchers return () (per this variant's
        -- monomorphic ('()'-result) signature).
        appendHead h
        appendBody b

-- ---------------------------------------------------------------------
-- Immediate-mode two-fragment head/body widget assembly
-- ---------------------------------------------------------------------

-- | Immediate-mode analog of 'runHydrationWidgetWithHeadAndBody': build
-- two 'DocumentFragment's (one for @<head>@ content, one for @<body>@
-- content) inside a single floating-widget scope, then swap them into
-- the real head/body elements. Port of upstream lines 251–284
-- (@runImmediateWidgetWithHeadAndBody@).
--
--   1. Look up the document, @<head>@, @<body>@; create two fresh
--      'DocumentFragment's.
--   2. Bootstrap the floating widget scope; inside, build head/body
--      dispatchers that install a per-subwidget 'HydrationEnv' pointing
--      at the corresponding fragment and run the guest widget under
--      'runDomBuilderImmediate' + 'runHasDocumentGhcjs'.
--   3. Invoke the caller's 'app' with the two dispatchers.
--   4. After the attach, 'replaceElementContents' each fragment into
--      the real head/body element.
--   5. Fire postBuild, fork the drain loop.
--
-- Shape divergence from upstream mirrors the hydration analog: the
-- dispatchers are monomorphised to @()@-result per call (same
-- observation in 'runHydrationWidgetWithHeadAndBody').
runImmediateWidgetWithHeadAndBody
  :: (forall esF
        . FloatingWidgetE (EffTimeline Global) esF
        =>    -- appendHead
              (forall esW c. WidgetE (EffTimeline Global) esW
                    => Eff esW c) -> Eff esF c
        ->    -- appendBody
              ((forall esW c. WidgetE (EffTimeline Global) esW
                    => Eff esW c) -> Eff esF c)
        -> Eff esF ())
  -- ^ App callback: given @(appendHead, appendBody)@ dispatchers,
  -- assemble a floating widget that dispatches head/body content
  -- through the two dispatchers. Each dispatcher preserves the
  -- guest widget's result type (matches upstream's polymorphic
  -- @forall c. Widget () c -> FloatingWidget () c@ shape — needed
  -- so 'mainWidgetWithHead\'' can thread @hOut@ / @bOut@ values
  -- through the mutual-recursion knot).
  -> JSM ()
runImmediateWidgetWithHeadAndBody app = do
  ctx         <- askJSM
  globalDoc   <- currentDocumentUnchecked
  headElement <- getHeadUnchecked globalDoc
  bodyElement <- getBodyUnchecked globalDoc
  headFragment <- createDocumentFragment globalDoc
  bodyFragment <- createDocumentFragment globalDoc
  liftIO $ do
    nhe     <- newNativeHostEnv
    doneVar <- newTVarIO False
    chanRef <- newIORef Nothing
    ((), fireCmd) <-
      runFloatingInterpreterStack ctx nhe chanRef $
        app (immediateSubwidget globalDoc (toNode headFragment))
            (immediateSubwidget globalDoc (toNode bodyFragment))
    _ <- runJSM (attachToRoot headElement headFragment) ctx
    _ <- runJSM (attachToRoot bodyElement bodyFragment) ctx
    _ <- runNativeFireCommand fireCmd
           [SomeTriggerFire (nheTrigger nhe) ()]
           (pure ())
    _ <- forkIO (drainUntilDone chanRef doneVar)
    pure ()

-- | Immediate-mode per-subwidget installer: install a fresh
-- 'HydrationEnv' pointing at the given parent 'Node' (the caller's
-- 'DocumentFragment' cast to 'Node') with @hydrationMode =
-- HydrationMode_Immediate@ and @switchover = never@, then run the
-- guest widget inside it via 'runDomBuilderImmediate' +
-- 'runHasDocumentGhcjs'.
--
-- Immediate-mode analog of 'hydrateSubwidget'. Port of upstream lines
-- 266–279 (the @go df w@ closure inside
-- @runImmediateWidgetWithHeadAndBody@). Semantic difference from
-- 'hydrateSubwidget': no delayed-runner registration (immediate mode
-- never queues a 'HydrationRunner') and no root-nodes ref push.
immediateSubwidget
  :: forall es c
   . FloatingWidgetE (EffTimeline Global) es
  => DOM.Document
  -> Node
  -> (forall esW. WidgetE (EffTimeline Global) esW => Eff esW c)
  -> Eff es c
immediateSubwidget globalDoc parent guest = do
  delayed         <- liftIO $ newIORef
    (RunnerAction (pure ()) :: DelayedHydration (EffTimeline Global))
  unreadyChildren <- liftIO $ newIORef 0
  hydrationMode   <- liftIO $ newIORef HydrationMode_Immediate
  (chan, _)       <- newTriggerEventChan @(EffTimeline Global)
  let hEnv :: HydrationEnv (EffTimeline Global)
      hEnv = HydrationEnv
        { hydrationEnv_document        = globalDoc
        , hydrationEnv_parent          = Left parent
        , hydrationEnv_unreadyChildren = unreadyChildren
        , hydrationEnv_commitAction    = pure ()
        , hydrationEnv_hydrationMode   = hydrationMode
        , hydrationEnv_switchover      = R.never
        , hydrationEnv_delayed         = delayed
        , hydrationEnv_events          = chan
        }
  runReader hEnv
    . runHasDocumentGhcjs
    . runDomBuilderImmediate @(EffTimeline Global)
    $ guest

-- | Interleaved mutual-recursion head/body variant. Port of upstream
-- lines 287–291 (@mainWidgetWithHead\'@).
--
-- Uses @rec@ (via @RecursiveDo@) to mutually bind the head widget's
-- output to the body widget's input and vice versa. Requires
-- 'MonadFix' — supplied by the native 'MonadFix' instance for 'Eff
-- es' from the effectful library, so no additional plumbing is
-- needed beyond enabling the extension.
--
-- Observation semantics match upstream: @h bOut@ observes @b@'s
-- output as a value in the current frame, and @b hOut@ observes
-- @h@'s output — the FRP network ties the knot lazily via 'mfix',
-- exactly like upstream's Spider-backed 'MonadFix'.
mainWidgetWithHead'
  :: forall a b
   . ( a -> (forall es. WidgetE (EffTimeline Global) es => Eff es b)
     , b -> (forall es. WidgetE (EffTimeline Global) es => Eff es a)
     )
  -> IO ()
mainWidgetWithHead' (h, b) = withDomHost $
  runImmediateWidgetWithHeadAndBody $ \appendHead appendBody -> do
    rec hOut <- appendHead (h bOut)
        bOut <- appendBody (b hOut)
    pure ()
