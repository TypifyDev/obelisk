{-# LANGUAGE DerivingStrategies #-}
-- | Kitchen-sink module — framework-bound entries (originally classified as
-- reflex-dom-adjacent). Verbatim aggregation of reflex-dom/effectful_port.hs from
-- the scratch corpus.

module JSDOM.Extras.ReflexDom.EffectfulPort where

import qualified Prelude as P
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Kind (Type)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.IORef (IORef)
import Effectful (Eff, (:>), IOE)
import Effectful.Dispatch.Dynamic (interpose, send)
import Effectful.Reader.Static (Reader)
import Foreign.JavaScript.TH (WithJSContextSingleton(..), runWithJSContextSingleton)
import qualified Reflex.Effectful.DomHost as HH
import Reflex.Effectful.DomHost (DomBuilderSpaceE, DomBuilderTimelineE, DomBuilderE, HasDocumentE, DomRenderHookE, GhcjsDomSpace, WidgetE, HydrationEnv, JSM', hoistTraverseWithKeyWithAdjust, hoistTraverseIntMapWithKeyWithAdjust, applyDomUpdateIntMap, applyDomUpdateDMap, applyDomUpdateDMapWithMove, updateIntMapChildUnreadiness, updateDMapChildUnreadiness, updateDMapWithMoveChildUnreadiness, mapPatchDMap, mapPatchDMapWithMove, runWithReplaceEff)
import qualified Reflex.Dom.Builder.Class as RDBC
import qualified Reflex.PerformEvent.Class as RPE
import qualified Reflex.PostBuild.Class as RPB
import qualified Reflex.TriggerEvent.Class as RTE
import qualified Reflex.Adjustable.Class as RAdj
import qualified Reflex.NotReady.Class as RNR
import qualified Reflex.Host.Class as RH
import qualified Reflex.Class as R
import qualified Reflex.Spider.Internal as SI
import qualified Reflex.Dom.Prerender.Class as RDPR
import qualified Control.Monad.Ref as MRef
import qualified Language.Javascript.JSaddle.Types as JSaddleTypes

-- ----- deps/jenga/lib/reflex-dom-core-effectful/src/Reflex/Effectful/DomHost.hs : resolvePort -----
resolvePort :: IO Int
resolvePort = do
  mPort <- lookupEnv "JSADDLE_WARP_PORT"
  pure $ case mPort P.>>= readMaybe of
    Just p  -> p
    Nothing -> 3911

-- =====================================================================
-- mainWidget composer (immediate mode, Global timeline).
--
-- Interpreter stack layered per variant A's runInterpreterStack shape:
--
--   runEffectfulHost           -- IO base + BaseEffectfulHostRow discharge
--   . runTriggerEvent chan     -- OUTERMOST HeadlessRow: peels TriggerEvent
--   . runPostBuild pbEv        -- peels PostBuild
--   . runPerformEvent respEv   -- peels PerformEvent, returns tuple ((), aggActions)
--   . runAdjustable            -- peels Adjustable (needs PerformEvent :> es)
--   . runReader hEnv           -- DOM: HydrationEnv (used by all DOM interpreters)
--   . runJSM' ctx              -- DOM: JSaddle context
--   . runDomRenderHook         -- DOM: schedules JSM on Event fire
--   . runHasDocumentGhcjs      -- DOM: HasDocumentE
--   . runDomBuilderImmediate   -- DOM: DomBuilderE — innermost around widget
--
-- Design decisions:
--   * Pinned to SpiderTimeline Global (matches Instances.hs Global-
--     specific factories). To use a fresh timeline via
--     'withSpiderTimeline', a caller would need to write their own
--     composer against 'HH.runEffectfulHostFor' with a locally-
--     parameterised widget.
--   * The postBuild trigger is fired in the same 'HH.runEffectfulHost'
--     scope, right after the widget finishes building — see
--     'attachImmediateWidget'.
--   * 'hydrationEnv_events' and the TriggerEvent interpreter chan are
--     the SAME chan: 'attachImmediateWidget' + 'attachWidget'' /
--     'attachHydrationWidgetWithFailure' / 'runHydrationWidget-
--     WithHeadAndBodyWithFailure' allocate ONE chan and thread it
--     both into 'HH.runTriggerEvent' (so 'newTriggerEvent' fires
--     enqueue into it) AND into 'HydrationEnv' (so element event
--     handlers push to the same chan). 'processAsyncEvents' drains
--     both.
--
-- Every send in the widget lands on the SAME Global-tagged Spider
-- scheduler, so subscriptions from within the widget scope are
-- coherent with any outer subscribers set up in the same session.
-- =====================================================================

-- | Effect row satisfied by rows an 'ImmediateWidget' can run in.
-- Polymorphic in the timeline @t@; the entry points ('mainWidget' etc.)
-- pin @t = 'R.SI.SpiderTimeline' 'R.SI.Global'@, but subroutines that
-- carry this constraint may be written timeline-generic.
--
-- @'HH.PerformEvent' ('SpiderTag' t)@ and @'HH.HostFrame'' ('SpiderTag' t)@
-- and @'RPE.Performable' ('Eff' es) ~ 'R.SI.SpiderHostFrame' ('SpiderTag' t)@
-- recover the spider host tag @x@ from @t = 'R.SI.SpiderTimeline' x@; the
-- 'SpiderTag' type family stays stuck if @t@ is not a Spider timeline,
-- so the constraint refuses to solve for non-Spider timelines — matching
-- variant B's implicit @DomTimeline = Spider@ assumption.
type ImmediateWidgetE (t :: Type) es =
  ( MonadFix (Eff es)
  , DomBuilderSpaceE es ~ GhcjsDomSpace
  , DomBuilderTimelineE es ~ t
  , DomBuilderE t GhcjsDomSpace :> es
  , HasDocumentE GhcjsDomSpace :> es
  , DomRenderHookE t :> es
  , Reader (HydrationEnv t) :> es
  , HH.PostBuild t :> es
  , HH.TriggerEvent t :> es
  , HH.Adjustable t :> es
  , HH.PerformEventEff '[IOE] t :> es
    -- Task #273: PerformEventM in row for MTL bridge to pin Performable to
    -- HostFrame t (upstream Prerender requires MonadJSM/MonadSample on Performable).
    -- Discharged via 'interpretPerformEventMAsRequester' at composer sites.
  , HH.PerformEventH t :> es
  , HH.CreateTrigger t :> es
  , HH.NotReady t :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , JSM' :> es
  , IOE :> es
  -- OLD-PERFORMEVENT-COMMENTED-OUT: RPE.Performable equality deleted per task #268 Fork C
  )

-- | Widget monad — newtype wrapper around
-- @WithJSContextSingleton x (Eff es)@. Just a nicer name; carries the
-- same load-bearing @WithJSContextSingleton x@ layer that makes
-- @MonadJSM (Performable m)@ resolve for upstream @Prerender t m@.
--
-- All MTL bridge instances (DomBuilder / HasDocument / DomRenderHook /
-- MountableDomBuilder / Prerender / MonadJSM / MonadHold / MonadSample
-- / PostBuild / TriggerEvent / Adjustable / PerformEvent) are derived
-- via GeneralizedNewtypeDeriving from the wrapped monad.
newtype WidgetEff (x :: Type) (es :: [Effect]) (a :: Type) = WidgetEff
  { unWidgetEff :: WithJSContextSingleton x (Eff es) a }
  deriving newtype (Functor, P.Applicative, P.Monad)

deriving newtype instance IOE :> es => MonadIO (WidgetEff t x es)
deriving newtype instance IOE :> es => MonadFix (WidgetEff t x es)

-- ---------------------------------------------------------------------
-- MTL bridge instances — all one-liners via GeneralizedNewtypeDeriving.
-- Each lifts the corresponding instance on
-- @WithJSContextSingleton x (Eff es)@ (which either exists via
-- the port's own bridges or via jsaddle / MonadTransControl lifting).
-- ---------------------------------------------------------------------

deriving newtype instance
  (JSM' :> es, IOE :> es) => JSaddleTypes.MonadJSM (WidgetEff t x es)

-- ---------------------------------------------------------------------
-- NotReady — trivial delegation.
-- ---------------------------------------------------------------------
instance
  RNR.NotReady t (WithJSContextSingleton x (Eff es))
  => RNR.NotReady t (WidgetEff t x es) where
  notReadyUntil ev = WidgetEff (RNR.notReadyUntil ev)
  notReady         = WidgetEff RNR.notReady

-- ---------------------------------------------------------------------
-- Adjustable — 'Event t (m b)' arg types force us to hoist the
-- 'WidgetEff' newtype in the event via 'fmap unWidgetEff'.
-- ---------------------------------------------------------------------
-- Adjustable for 'WidgetEff t x es'. 'runWithReplace' dispatches through
-- the NEW 'AdjustableInRequesterT' effect (via 'runWithReplaceRequesterTE')
-- so that per-cohort fresh-Requester scoping engages
-- ('runAdjustableInRequesterT' interpreter installs 'interpose'-based
-- fresh Requester handlers per replacement widget). This is the wire
-- that closes the 24 failing hydration browser tests. Traverse variants
-- still delegate through the underlying Adjustable instance for now
-- (see runner docstring at eff-reflex/HeadlessEffectful.hs for
-- follow-up on proper traverse-in-Requester-scope implementation).
instance
  ( RAdj.Adjustable t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  , t ~ R.SI.SpiderTimeline x
  , R.SI.HasSpiderTimeline x
  , WidgetE t (DomBuilderSpaceE es) es
  , RH.ReflexHost t
  , MonadFix (Eff es)
  ) => RAdj.Adjustable t (WidgetEff t x es) where
  runWithReplace a0 a' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    let unwrapChild :: WidgetEff t x es b -> Eff es b
        unwrapChild w = runWithJSContextSingleton (unWidgetEff w) jsSing
    -- Delegate to 'runWithReplaceEff' — the port's fully-hydration-aware
    -- runWithReplace implementation. Under upstream reflex-dom-core, the
    -- MTL Adjustable(HydrationDomBuilderT) instance carries hydration
    -- cohort setup (skipToReplaceStart/End, per-child fresh delayed IORef,
    -- commit ordering, walker step). We keep the same design here rather
    -- than moving into a 'withAdjustableHydration' interpose, because:
    --   1. HydrationEnv fields ('parent', 'commitAction') are plain values
    --      in upstream — using IORef wrapping to survive Effectful's
    --      SeqForkUnlift fork boundary would be an unnecessary divergence.
    --   2. Effectful 'Reader.Static.local' modifications do NOT survive
    --      the fork boundary an interpose's 'localUnliftIO' introduces.
    --   3. The MTL-dispatch path has no fork between the localEnv swap
    --      and the child's execution, so upstream's Reader-based design
    --      works correctly here.
    -- Requester + Query interposers are safe because their per-cohort
    -- state is IORef-based (matching upstream Requester's RequesterState),
    -- and IORef survives fork by reference.
    in runWithReplaceEff @t @(DomBuilderSpaceE es)
         (unwrapChild a0)
         (unwrapChild <$> a')
  -- Per-element PostBuild interpose: each traverse element gets its own
  -- fresh postBuild event (fired after the element widget is built).
  -- Mirrors upstream reflex-dom-core's HydrationDomBuilderT which gives
  -- each per-child scope its own PostBuild via 'PostBuildT'. Without this,
  -- 'getPostBuild' inside a newly-added element would return the
  -- long-since-fired app-level pb, breaking widgets like
  --   listWithKey $ \\k v -> do pb <- getPostBuild; performEvent_ (act <$ pb)
  --
  -- Implementation: wrap each 'f k v' with an 'interpose' on
  -- 'HH.PostBuild t' that returns a fresh trigger event; fire the
  -- trigger via the events chan after the element widget completes.
  -- Traverse methods route through the hoistTraverse* wrappers, which
  -- own the DMap/IntMap DOM lifecycle (placeholder management, applyDomUpdate
  -- for insert/delete/move, child-readiness tracking). Without this
  -- routing the Adjustable interpreter runs the FRP layer correctly but
  -- no DOM operations happen — that's why 8 DMap-family tests were
  -- failing pre-wire-up. Each traverse method also wraps the user's
  -- 'f k v' with 'interpose'+'firePb' to give the element its own
  -- per-invocation PostBuild event (mirrors upstream reflex-dom-core's
  -- HydrationDomBuilderT.PostBuildT-per-scope semantics).
  -- Per-element PostBuild interpose: each traverse element gets its own
  -- fresh postBuild event (fired after the element widget is built).
  -- Mirrors upstream reflex-dom-core's HydrationDomBuilderT which gives
  -- each per-child scope its own PostBuild via 'PostBuildT'. Traverse
  -- methods route through the 'hoistTraverse*' wrappers, which own the
  -- DMap/IntMap DOM lifecycle (placeholder management, applyDomUpdate for
  -- insert/delete/move, child-readiness tracking).
  traverseIntMapWithKeyWithAdjust f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    let fWithPb k v = do
          (freshPb, firePb) <- HH.newTriggerEventE @t
          result <- interpose @(HH.PostBuild t)
                      (\_ HH.GetPostBuild -> pure freshPb)
                      (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
          liftIO (firePb ())
          pure result
    in hoistTraverseIntMapWithKeyWithAdjust @t
         (\ff im0 im' -> send (HH.TraverseIntMapWithKeyWithAdjustOp ff im0 im'))
         updateIntMapChildUnreadiness
         applyDomUpdateIntMap
         fWithPb
         dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    -- Inline the per-element PostBuild interpose lambda directly (not
    -- let-bound) so its polymorphism over the existentially-quantified
    -- 'a' is preserved at the call site.
    hoistTraverseWithKeyWithAdjust @t
      (\ff dm0' dm'' -> send (HH.TraverseDMapWithKeyWithAdjustOp ff dm0' dm''))
      mapPatchDMap
      updateDMapChildUnreadiness
      applyDomUpdateDMap
      (\k v -> do
        (freshPb, firePb) <- HH.newTriggerEventE @t
        result <- interpose @(HH.PostBuild t)
                    (\_ HH.GetPostBuild -> pure freshPb)
                    (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
        liftIO (firePb ())
        pure result)
      dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    hoistTraverseWithKeyWithAdjust @t
      (\ff dm0' dm'' -> send (HH.TraverseDMapWithKeyWithAdjustWithMoveOp ff dm0' dm''))
      mapPatchDMapWithMove
      updateDMapWithMoveChildUnreadiness
      applyDomUpdateDMapWithMove
      (\k v -> do
        (freshPb, firePb) <- HH.newTriggerEventE @t
        result <- interpose @(HH.PostBuild t)
                    (\_ HH.GetPostBuild -> pure freshPb)
                    (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
        liftIO (firePb ())
        pure result)
      dm0 dm'

-- ---------------------------------------------------------------------
-- DomBuilder — pins 'DomBuilderSpace' to the underlying, delegates
-- every method by unwrapping the widget body and re-wrapping the
-- result.
-- ---------------------------------------------------------------------
instance
  ( RDBC.DomBuilder t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  , DomSpace (DomBuilderSpaceE es)
  , RNR.NotReady t (WidgetEff t x es)
  , RAdj.Adjustable t (WidgetEff t x es)
  , RDBC.DomBuilderSpace (WithJSContextSingleton x (Eff es)) ~ DomBuilderSpaceE es
  ) => RDBC.DomBuilder t (WidgetEff t x es) where
  type DomBuilderSpace (WidgetEff t x es) = DomBuilderSpaceE es
  textNode        cfg          = WidgetEff (RDBC.textNode cfg)
  commentNode     cfg          = WidgetEff (RDBC.commentNode cfg)
  element         tag cfg body = WidgetEff (RDBC.element tag cfg (unWidgetEff body))
  inputElement    cfg          = WidgetEff (RDBC.inputElement cfg)
  textAreaElement cfg          = WidgetEff (RDBC.textAreaElement cfg)
  selectElement   cfg body     = WidgetEff (RDBC.selectElement cfg (unWidgetEff body))
  placeRawElement raw          = WidgetEff (RDBC.placeRawElement raw)
  wrapRawElement  raw cfg      = WidgetEff (RDBC.wrapRawElement raw cfg)

-- ---------------------------------------------------------------------
-- HasDocument — dispatch directly through the port's HasDocumentE
-- effect via 'send'. Bypasses 'WithJSContextSingleton', which
-- reflex-dom-core does not ship a lifting instance for.
-- ---------------------------------------------------------------------
instance
  ( HasDocumentE (DomBuilderSpaceE es) :> es
  , RDBC.DomBuilder t (WidgetEff t x es)
  , DomBuilderSpaceE es ~ RDBC.DomBuilderSpace (WidgetEff t x es)
  ) => RDBC.HasDocument (WidgetEff t x es) where
  askDocument = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(HasDocumentE (DomBuilderSpaceE es)) AskDocument)))

-- ---------------------------------------------------------------------
-- DomRenderHook — bridge directly to the port's @DomRenderHook t (Eff es)@
-- instance (line 1372) rather than routing through
-- @WithJSContextSingleton x (Eff es)@. Reflex-dom-core does not ship
-- a ReaderT-lifting instance for 'DomRenderHook', so an
-- intermediate WithJSContextSingleton hop cannot be satisfied.
-- ---------------------------------------------------------------------
instance
  ( R.Reflex t
  , t ~ DomBuilderTimelineE es
  , DomRenderHookE t :> es
  ) => RDBC.DomRenderHook t (WidgetEff t x es) where
  -- Dispatch directly through the effect layer via 'send' —
  -- matches the 'DomRenderHook t (Eff es)' instance body above and
  -- bypasses the DomRenderHookTimelineE fundep gymnastics.
  withRenderHook hook body = WidgetEff (WithJSContextSingleton (ReaderT (\ctx ->
    send @(DomRenderHookE t) (WithRenderHook hook (runWithJSContextSingleton (unWidgetEff body) ctx)))))
  requestDomAction  ev     = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(DomRenderHookE t) (RequestDomAction  ev))))
  requestDomAction_ ev     = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(DomRenderHookE t) (RequestDomAction_ (P.fmap (P.>> pure ()) ev)))))

-- ---------------------------------------------------------------------
-- MountableDomBuilder — pins 'DomFragment' to the underlying.
-- ---------------------------------------------------------------------
instance
  ( RDBC.MountableDomBuilder t (WithJSContextSingleton x (Eff es))
  , RDBC.DomBuilder t (WidgetEff t x es)
  , RDBC.DomFragment (WithJSContextSingleton x (Eff es)) ~ DomFragment (DomBuilderSpaceE es)
  ) => RDBC.MountableDomBuilder t (WidgetEff t x es) where
  type DomFragment (WidgetEff t x es) = DomFragment (DomBuilderSpaceE es)
  buildDomFragment body    = WidgetEff (RDBC.buildDomFragment (unWidgetEff body))
  mountDomFragment df dfEv = WidgetEff (RDBC.mountDomFragment df dfEv)

-- ---------------------------------------------------------------------
-- PerformEvent — pins 'Performable' to the underlying wrapped monad's
-- Performable (which is 'WithJSContextSingleton x (Performable (Eff es))').
-- ---------------------------------------------------------------------
instance
  ( RPE.PerformEvent t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
    -- Task #273: dropped the @Performable ~ SpiderHostFrame@ pin. Under the
    -- new MTL bridge (task #274), @Performable (Eff es) = Eff es@ and Performable
    -- lifts through WithJSContextSingleton naturally.
  ) => RPE.PerformEvent t (WidgetEff t x es) where
  type Performable (WidgetEff t x es) = RPE.Performable (WithJSContextSingleton x (Eff es))
  performEvent  ev = WidgetEff (RPE.performEvent  ev)
  performEvent_ ev = WidgetEff (RPE.performEvent_ ev)

-- ---------------------------------------------------------------------
-- PostBuild — trivial delegation.
-- ---------------------------------------------------------------------
instance
  ( RPB.PostBuild t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => RPB.PostBuild t (WidgetEff t x es) where
  getPostBuild = WidgetEff RPB.getPostBuild

-- ---------------------------------------------------------------------
-- TriggerEvent — has an 'IO () -> IO ()' arg in one method that must be
-- coerced (safe: no 'm' involvement).
-- ---------------------------------------------------------------------
instance
  RTE.TriggerEvent t (WithJSContextSingleton x (Eff es))
  => RTE.TriggerEvent t (WidgetEff t x es) where
  newTriggerEvent               = WidgetEff RTE.newTriggerEvent
  newTriggerEventWithOnComplete = WidgetEff RTE.newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f
                                = WidgetEff (RTE.newEventWithLazyTriggerWithOnComplete f)

-- ---------------------------------------------------------------------
-- MonadSample.
-- ---------------------------------------------------------------------
instance
  ( R.MonadSample t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => R.MonadSample t (WidgetEff t x es) where
  sample b = WidgetEff (R.sample b)

-- ---------------------------------------------------------------------
-- MonadHold.
-- ---------------------------------------------------------------------
instance
  ( R.MonadHold t (WithJSContextSingleton x (Eff es))
  , R.MonadSample t (WidgetEff t x es)
  , R.Reflex t
  ) => R.MonadHold t (WidgetEff t x es) where
  hold a0 ev             = WidgetEff (R.hold a0 ev)
  holdDyn a0 ev          = WidgetEff (R.holdDyn a0 ev)
  holdIncremental p0 ev  = WidgetEff (R.holdIncremental p0 ev)
  buildDynamic pm ev     = WidgetEff (R.buildDynamic pm ev)
  headE ev               = WidgetEff (R.headE ev)
  now                    = WidgetEff R.now

-- ---------------------------------------------------------------------
-- MonadReflexCreateTrigger.
-- ---------------------------------------------------------------------
instance
  ( RH.MonadReflexCreateTrigger t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => RH.MonadReflexCreateTrigger t (WidgetEff t x es) where
  newEventWithTrigger f    = WidgetEff (RH.newEventWithTrigger f)
  newFanEventWithTrigger f = WidgetEff (RH.newFanEventWithTrigger f)

-- ---------------------------------------------------------------------
-- MonadRef — Ref (WidgetEff t x es) = IORef, delegated to the underlying
-- Eff / WithJSContextSingleton pair. Required by upstream's
-- 'PrerenderBaseConstraints' and by any widget helper carrying
-- @MonadRef m@ / @Ref m ~ IORef@ constraints (e.g. the 'TestWidget'
-- bundle used by upstream reflex-dom-core's hydration test suite).
-- ---------------------------------------------------------------------
instance IOE :> es => MRef.MonadRef (WidgetEff t x es) where
  type Ref (WidgetEff t x es) = IORef
  newRef       = WidgetEff . MRef.newRef
  readRef      = WidgetEff . MRef.readRef
  writeRef ref = WidgetEff . MRef.writeRef ref
  {-# INLINABLE newRef   #-}
  {-# INLINABLE readRef  #-}
  {-# INLINABLE writeRef #-}

-- ---------------------------------------------------------------------
-- EventWriter / DynamicWriter / BehaviorWriter — NOT provided.
--
-- Attempted implementation removed: the reflex writer classes have a
-- functional dependency 'm -> t w' (the monad type uniquely determines
-- both the timeline 't' and the write type 'w'). A bare instance on
-- 'Eff es' can't satisfy the liberal coverage condition because 'es'
-- doesn't encode 't' or 'w' anywhere in its type structure — GHC
-- rejects with [GHC-21572] "Illegal instance declaration ... Reason:
-- lhs type 'Eff es' does not determine rhs types 't', 'w'".
--
-- Path to close this properly: define per-timeline-and-writeshape
-- newtype wrappers (e.g. 'newtype EventWriterEff t w a = EventWriterEff
-- (Eff (State ... : es) a)') so the FunDep is satisfied structurally.
-- Task #122 follow-up. Not exercised by the 14 failing HydrationBrowser
-- tests (they use 'performEvent_', not 'tellEvent').
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Prerender — mirrors the RDPR.Prerender bridge instance for
-- WithJSContextSingleton x (Eff serverEs) further down in this file,
-- but for WidgetEff t x es. Client type is a WidgetEff of the client row.
-- ---------------------------------------------------------------------
instance
  ( t ~ DomBuilderTimelineE es
  , DomBuilderTimelineE (ClientEs t (DomBuilderSpaceE es) es) ~ t
  , HasPrerender t (DomBuilderSpaceE es) es
  , PrerenderClientConstraintE t (ClientEs t (DomBuilderSpaceE es) es)
  , RDPR.PrerenderClientConstraint t
      (WidgetEff x (ClientEs t (DomBuilderSpaceE es) es))
  ) => RDPR.Prerender t (WidgetEff t x es) where
  type Client (WidgetEff t x es) =
    WidgetEff x (ClientEs (DomBuilderTimelineE es)
                          (DomBuilderSpaceE es)
                          es)
  prerender server client = WidgetEff (WithJSContextSingleton (ReaderT (\ctx ->
    let serverEff = runWidgetEff server ctx
        clientEff = runWidgetEff client ctx
    in prerender @(DomBuilderTimelineE es) @(DomBuilderSpaceE es) serverEff clientEff)))

-- | Widget type used by 'mainWidget' + 'attachWidgetImmediate'.
-- Timeline is 'DomTimeline' — the single-source-of-truth alias
-- (mirrors upstream MTL reflex-dom's 'Reflex.Dom.Main.DomTimeline').
-- Parameterized on the JSaddle-context tag @x@ and effect row @es@.
type DomTimeline = R.SI.SpiderTimeline R.SI.Global

type ImmediateWidget (x :: Type) (es :: [Effect]) =
  ImmediateWidgetE DomTimeline es
  => WidgetEff t x es ()

-- | Run a 'WidgetEff' against a supplied 'JSContextSingleton'.

