{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Interpose-cascade composer for reflex-dom-core-effectful.
--
-- Wraps 'Reflex.Effectful.DomHost.hostPerformEventT' with an additional
-- 'Reflex.Host.AdjustableInterpose.withAdjustableRequester' interpose layer
-- on the shared 'HH.Adjustable t' effect. Widget code (MTL 'Adjustable'
-- calls via WidgetEff's instance) still sends to 'HH.Adjustable t' — but
-- dispatch now hits the interposer FIRST, which:
--
--   1. Allocates a fresh cohort id;
--   2. Wraps child/replacements with 'requesterCohortWrap' (fresh
--      Requester scope per cohort);
--   3. Re-sends 'RunWithReplace' to the next Adjustable handler
--      (which is the fused 'runPerformEventAndAdjustable' at base).
--
-- This gives per-cohort Requester isolation on TOP of the existing
-- fused-runner's per-cohort machinery — redundant but non-conflicting.
-- Purpose: prove the cascade shape composes end-to-end with the real
-- hydration composer, without disturbing the fused-runner path that the
-- 103 hydration tests already exercise.
--
-- No touches to 'Reflex.Effectful.DomHost' or 'Reflex.Host.HeadlessEffectful'.
module Reflex.Effectful.DomHostInterpose
  ( -- OLD-PERFORMEVENT-COMMENTED-OUT: hostPerformEventTInterpose (task #268 — depends on gone fused runner)
  ) where

import qualified Control.Monad                   as M
import qualified Data.Functor.Identity           as Identity
import           Data.IORef                      (IORef)

import qualified Control.Monad.IO.Class          as MIO

import qualified Effectful                       as E
import           Effectful                       (Eff, (:>))

import qualified Reflex                          as R
import qualified Reflex.Spider.Internal          as R.SI

import qualified Reflex.Host.HeadlessEffectful   as HH
import qualified Reflex.Host.AdjustableInterpose as AI

import qualified Reflex.Effectful.DomHost        as DH


{- BEGIN OLD-PERFORMEVENT-COMMENTED-OUT hostPerformEventTInterpose (task #268).
Depends on 'HH.runPerformEventAndAdjustable' (the fused Adjustable+PerformEvent+Requester
runner) which is commented out in eff-reflex/HeadlessEffectful.hs per task #268. Restoring
this function requires either (a) an Eff-based fused runner over 'PerformEventEff', or
(b) rewriting as compositional peels (runAdjustable + runPerformEventEff + runRequester)
mirroring the new 'DH.hostPerformEventT'. Deferred to a follow-up wave; interpose-cascade
parallel is not on the critical path for task #268 green build.
Preserved verbatim below.

-- | Interpose-cascade parallel to 'DH.hostPerformEventT'. Same input row,
-- same 'HE.EffectfulHost' output shape. Only difference: an additional
-- 'AI.withAdjustableRequester' interpose is layered on the widget's
-- 'HH.Adjustable' send path.
hostPerformEventTInterpose
  :: forall a
   . Eff ( HH.Adjustable (R.SI.SpiderTimeline R.SI.Global)
         ': HH.PerformEvent R.SI.Global
         ': HH.Requester
             (R.SI.SpiderTimeline R.SI.Global)
             (R.SI.SpiderHostFrame R.SI.Global)
             Identity.Identity
         ': HH.BaseEffectfulHostRow R.SI.Global
         )
        a
  -> HH.EffectfulHost R.SI.Global (a, DH.FireCommand)
hostPerformEventTInterpose act = HH.EffectfulHost $ do
  (responseEv, respTrigRef) <- MIO.liftIO DH.newEventWithTriggerRef
  (result, aggActions) <-
      HH.runPerformEventAndAdjustable @R.SI.Global responseEv
    . AI.withAdjustableRequester
        @R.SI.Global
        @(R.SI.SpiderHostFrame R.SI.Global)
        @Identity.Identity
    $ act
  actionsHandle  <- HH.subscribeEventE @(R.SI.SpiderTimeline R.SI.Global) aggActions
  shutdownHandle <- HH.subscribeEventE @(R.SI.SpiderTimeline R.SI.Global)
                      (R.never :: R.Event (R.SI.SpiderTimeline R.SI.Global) ())
  let fc = DH.FireCommand $ \fires ->
        M.void $ HH.fireAndDrain @R.SI.Global
          fires actionsHandle shutdownHandle respTrigRef
  pure (result, fc)

END OLD-PERFORMEVENT-COMMENTED-OUT hostPerformEventTInterpose -}
