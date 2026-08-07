{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Effectful.Internal.WidgetLoop
  ( processLoop
  , WidgetConstraints
  , WidgetLoopResult(..)
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.IO.Class   (MonadIO, liftIO)

import qualified Reflex
import qualified Reflex.Dom.Core          as RD
import qualified Reflex.Dom.Xhr           as Xhr
import           GHCJS.DOM.Types          (MonadJSM, liftJSM)

import           Reflex.Effectful.Internal.Channel
                   ( WidgetOp(..), WidgetReq(..)
                   , ChildHandle(..), ChildHandleF(..)
                   )

-- | The value produced when a 'processLoop' invocation terminates.
--
-- 'processLoop' can only ever terminate three ways:
--
--   * a matching 'WODone' arrives (top-level user code returns) — 'WLRDone';
--   * a 'WOElementClose' arrives while we are the inner action of an
--     enclosing 'RD.element' call — 'WLRElementClose' carries back the
--     reply 'MVar' so the enclosing dispatch arm can fill it with the
--     element handle produced by 'RD.element';
--   * a 'WOSelectElementClose' arrives, symmetrical for 'RD.selectElement'.
--
-- Which constructor arrives is fully determined by the enclosing dispatch
-- arm: the interpreter never emits a 'WOElementClose' outside a matching
-- 'WOElementOpen' \/ 'RD.element' scope. The caller pattern-matches on the
-- specific constructor it expects; any other constructor at that position
-- is a DSL-construction invariant violation and would deadlock (parent
-- never fills the reply 'MVar'), which is a loud, visible failure in dev.
data WidgetLoopResult t
  = WLRDone
  | WLRElementClose
      (MVar (RD.Element RD.EventResult RD.GhcjsDomSpace t))
  | WLRSelectElementClose
      (MVar (RD.SelectElement RD.EventResult RD.GhcjsDomSpace t))

type WidgetConstraints t m =
  ( Reflex.Reflex t
  , Reflex.MonadSample t m
  , Reflex.MonadHold t m
  , RD.PostBuild t m
  , RD.TriggerEvent t m
  , RD.PerformEvent t m
  , MonadIO (RD.Performable m)
  , RD.DomBuilder t m
  , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
  , RD.HasDocument m
  , RD.DomRenderHook t m
  , RD.NotReady t m
  , RD.Adjustable t m
  , MonadJSM m
  , MonadJSM (RD.Performable m)
  , MonadFix m
  , MonadIO m
  )

processLoop
  :: forall t m. WidgetConstraints t m
  => MVar (WidgetReq t)
  -> m (WidgetLoopResult t)
processLoop chanVar = do
  WidgetReq op respVar <- liftIO $ takeMVar chanVar
  dispatch chanVar op respVar

dispatch
  :: forall t m a. WidgetConstraints t m
  => MVar (WidgetReq t)
  -> WidgetOp t a
  -> MVar a
  -> m (WidgetLoopResult t)
dispatch chanVar op respVar = case op of
  WODone            -> liftIO (putMVar respVar ()) >> return WLRDone
  -- Both close ops hand the enclosing 'respVar' back to the parent's element
  -- handler, which then fills it with the actual element handle. Nothing is
  -- put into 'respVar' here because it is the vehicle used to deliver the
  -- element handle back across the child/parent boundary; typing the return
  -- via 'WidgetLoopResult t' preserves the per-close reply-MVar type.
  WOElementClose    -> return (WLRElementClose respVar)
  WOSelectElementClose -> return (WLRSelectElementClose respVar)

  WOSample b        -> do r <- Reflex.sample b; liftIO $ putMVar respVar r; processLoop chanVar
  WOHold a ev       -> do r <- Reflex.hold a ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOHoldDyn a ev    -> do r <- Reflex.holdDyn a ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOHoldIncremental v0 ev -> do r <- Reflex.holdIncremental v0 ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOBuildDynamic getV0 ev -> do r <- Reflex.buildDynamic getV0 ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOHeadE ev        -> do r <- Reflex.headE ev; liftIO $ putMVar respVar r; processLoop chanVar
  WONow             -> do r <- Reflex.now; liftIO $ putMVar respVar r; processLoop chanVar
  WOFoldDyn f z ev  -> do r <- Reflex.foldDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOCount ev        -> do r <- Reflex.count ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOToggle z ev     -> do r <- Reflex.toggle z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOAccumDyn f z ev -> do r <- Reflex.accumDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOAccumMaybeDyn f z ev -> do r <- Reflex.accumMaybeDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOMapAccumMaybeDyn f z ev -> do r <- Reflex.mapAccumMaybeDyn f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOFoldDynMaybe f z ev -> do r <- Reflex.foldDynMaybe f z ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOGetPostBuild    -> do r <- RD.getPostBuild; liftIO $ putMVar respVar r; processLoop chanVar
  WONewTriggerEvent -> do r <- RD.newTriggerEvent; liftIO $ putMVar respVar r; processLoop chanVar
  WONewTriggerEventWithOnComplete -> do r <- RD.newTriggerEventWithOnComplete; liftIO $ putMVar respVar r; processLoop chanVar
  WOPerformEvent ev -> do r <- RD.performEvent (liftJSM <$> ev); liftIO $ putMVar respVar r; processLoop chanVar
  WOPerformEvent_ ev -> do RD.performEvent_ (liftJSM <$> ev); liftIO $ putMVar respVar (); processLoop chanVar
  WOText t          -> do RD.text t; liftIO $ putMVar respVar (); processLoop chanVar
  WODynText d       -> do RD.dynText d; liftIO $ putMVar respVar (); processLoop chanVar

  WOElementOpen tag cfg -> do
    liftIO $ putMVar respVar ()
    (el, wlr) <- RD.element tag cfg (processLoop chanVar)
    -- 'wlr' is the terminating 'WidgetLoopResult' of the inner processLoop;
    -- by construction (matching WOElementOpen/WOElementClose in interpretDom)
    -- it MUST be 'WLRElementClose closeResp'. 'closeResp' is the reply MVar
    -- for the matching 'WOElementClose'; the close op is typed
    -- 'WidgetOp t (Element ...)', so we deliver the element handle here so
    -- the caller's 'sendReq WOElementClose' completes with it. Any other
    -- constructor at this position is a DSL invariant violation and would
    -- hang the widget (parent blocked on unfilled MVar) — visible failure.
    case wlr of
      WLRElementClose closeResp -> liftIO $ putMVar closeResp el
      _                         -> pure ()
    processLoop chanVar

  WOInputElement cfg -> do r <- RD.inputElement cfg; liftIO $ putMVar respVar r; processLoop chanVar
  WOTextAreaElement cfg -> do r <- RD.textAreaElement cfg; liftIO $ putMVar respVar r; processLoop chanVar

  WOSelectElementOpen cfg -> do
    liftIO $ putMVar respVar ()
    (sel, wlr) <- RD.selectElement cfg (processLoop chanVar)
    -- Symmetrical to 'WOElementOpen': the terminating 'WidgetLoopResult'
    -- MUST be 'WLRSelectElementClose closeResp' by construction. See the
    -- 'WOElementOpen' comment for the invariant discussion.
    case wlr of
      WLRSelectElementClose closeResp -> liftIO $ putMVar closeResp sel
      _                               -> pure ()
    processLoop chanVar

  WOTextNode cfg    -> do r <- RD.textNode cfg; liftIO $ putMVar respVar r; processLoop chanVar
  WOCommentNode cfg -> do r <- RD.commentNode cfg; liftIO $ putMVar respVar r; processLoop chanVar

  WOPlaceRawElement rawEl -> do RD.placeRawElement rawEl; liftIO $ putMVar respVar (); processLoop chanVar
  WOWrapRawElement rawEl cfg -> do r <- RD.wrapRawElement rawEl cfg; liftIO $ putMVar respVar r; processLoop chanVar

  WORunWithReplace initialHandle replaceActions -> do
    let runChild (ChildHandle childChan resultVar) = do
          _ <- processLoop childChan
          liftIO $ takeMVar resultVar
    (a, bEv) <- RD.runWithReplace
      (runChild initialHandle)
      ((\startAction -> liftIO startAction >>= runChild) <$> replaceActions)
    liftIO $ putMVar respVar (a, bEv)
    processLoop chanVar

  WOTraverseIntMapWithAdjust initialHandles patchActions -> do
    let runChild (ChildHandle childChan resultVar) = do
          _ <- processLoop childChan
          liftIO $ takeMVar resultVar
    -- 'patchActions :: Event t (IO (PatchIntMap (ChildHandle t v')))';
    -- 'traverseIntMapWithKeyWithAdjust' wants 'Event t (PatchIntMap ...)'.
    -- 'performEvent' executes the enclosed 'IO' (running 'startChild' /
    -- 'forkIO' for each new child) and hands back the resolved patch event.
    patchEv <- RD.performEvent (liftIO <$> patchActions)
    (initialResults, patchResults) <- RD.traverseIntMapWithKeyWithAdjust
      (\_ h -> runChild h)
      initialHandles
      patchEv
    liftIO $ putMVar respVar (initialResults, patchResults)
    processLoop chanVar

  WOTraverseDMapWithAdjust initialHandles patchActions -> do
    -- See 'WOTraverseIntMapWithAdjust': lift IO into the ambient monad and
    -- execute via 'performEvent' before handing to the adjustable primitive.
    patchEv <- RD.performEvent (liftIO <$> patchActions)
    (initialResults, patchResults) <- RD.traverseDMapWithKeyWithAdjust
      (\_ (ChildHandleF (ChildHandle childChan resultVar)) -> do
          _ <- processLoop childChan
          liftIO $ takeMVar resultVar)
      initialHandles
      patchEv
    liftIO $ putMVar respVar (initialResults, patchResults)
    processLoop chanVar

  WOTraverseDMapWithAdjustWithMove initialHandles patchActions -> do
    -- See 'WOTraverseIntMapWithAdjust': same lift-then-perform pattern for
    -- the with-move variant.
    patchEv <- RD.performEvent (liftIO <$> patchActions)
    (initialResults, patchResults) <- RD.traverseDMapWithKeyWithAdjustWithMove
      (\_ (ChildHandleF (ChildHandle childChan resultVar)) -> do
          _ <- processLoop childChan
          liftIO $ takeMVar resultVar)
      initialHandles
      patchEv
    liftIO $ putMVar respVar (initialResults, patchResults)
    processLoop chanVar

  WOLiftJSM jsm     -> do r <- liftJSM jsm; liftIO $ putMVar respVar r; processLoop chanVar
  WOAskDocument      -> do r <- RD.askDocument; liftIO $ putMVar respVar r; processLoop chanVar
  WORequestDomAction ev -> do r <- RD.requestDomAction ev; liftIO $ putMVar respVar r; processLoop chanVar
  WORequestDomAction_ ev -> do RD.requestDomAction_ ev; liftIO $ putMVar respVar (); processLoop chanVar
  WONotReadyUntil ev -> do RD.notReadyUntil ev; liftIO $ putMVar respVar (); processLoop chanVar
  WONotReady         -> do RD.notReady; liftIO $ putMVar respVar (); processLoop chanVar

  WODelay dt ev      -> do r <- RD.delay dt ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOTickLossyFrom ev -> do r <- RD.tickLossyFrom' ev; liftIO $ putMVar respVar r; processLoop chanVar
  WOPerformRequestAsync ev -> do r <- Xhr.performRequestAsync ev; liftIO $ putMVar respVar r; processLoop chanVar

  WORequesting -> do
    -- Allocate the per-scope response bridge. 'runRequesterEff'
    -- fires the trigger with each per-tag decoded response;
    -- everything else — tag counter, routing table, per-call
    -- response taps — lives in 'runRequesterEff' (Effectful side).
    --
    -- The existentially-hidden 'rsp' in 'WORequesting' is inferred
    -- by GHC from the response 'MVar' 'respVar''s type; 'newTriggerEvent'
    -- unifies its @a@ parameter with @RequesterData rsp@.
    r <- RD.newTriggerEvent
    liftIO $ putMVar respVar r
    processLoop chanVar

  WOTellEvent{}      -> error "reflex-dom-effectful: tellEvent requires EventWriterT in Widget stack"
  WOTellDyn{}        -> error "reflex-dom-effectful: tellDyn requires DynamicWriterT in Widget stack"
  WOTellBehavior{}   -> error "reflex-dom-effectful: tellBehavior requires BehaviorWriterT in Widget stack"
