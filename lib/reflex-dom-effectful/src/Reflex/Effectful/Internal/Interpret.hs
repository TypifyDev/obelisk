{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Reflex.Effectful.Internal.Interpret
  ( interpretSample, interpretHold, interpretPostBuild
  , interpretTriggerEvent, interpretPerformEvent, interpretDom
  , interpretAdjustable, interpretPrerender
  , interpretJSM, interpretHasDocument, interpretDomRenderHook
  , interpretNotReady
  , interpretEventWriter, interpretDynamicWriter, interpretBehaviorWriter
  , interpretAll
  ) where

import qualified Data.IntMap.Strict         as IntMap
import qualified Data.Dependent.Map         as DMap
import           Data.Kind                   (Type)
import           Data.Patch.IntMap           (PatchIntMap(..))
import qualified Data.Patch.DMap             as PatchDMap
import qualified Data.Patch.DMapWithMove     as PatchDMapWithMove
import           Effectful
import           Effectful.Dispatch.Dynamic
import qualified Reflex

import           Reflex.Effectful.Effect.Sample           (Sample(..))
import           Reflex.Effectful.Effect.Hold             (Hold(..))
import           Reflex.Effectful.Effect.PostBuild        (PostBuild(..))
import           Reflex.Effectful.Effect.TriggerEvent     (TriggerEvent(..))
import           Reflex.Effectful.Effect.PerformEvent     (PerformEvent(..))
import           Reflex.Effectful.Effect.Dom              (Dom(..))
import           Reflex.Effectful.Effect.Adjustable       (Adjustable(..))
import           Reflex.Effectful.Effect.Prerender        (Prerender(..))
import           Reflex.Effectful.Effect.JSM              (JSM'(..))
import           Reflex.Effectful.Effect.HasDocument      (HasDocument(..))
import           Reflex.Effectful.Effect.DomRenderHook    (DomRenderHook(..))
import           Reflex.Effectful.Effect.NotReady         (NotReady(..))
import           Reflex.Effectful.Effect.EventWriter      (EventWriter(..))
import           Reflex.Effectful.Effect.DynamicWriter    (DynamicWriter(..))
import           Reflex.Effectful.Effect.BehaviorWriter   (BehaviorWriter(..))
import           Reflex.Effectful.Internal.Channel

-- Helper to disambiguate t for timeline-independent WidgetOp constructors
sr :: forall t r es. WidgetChannel t :> es => WidgetOp t r -> Eff es r
sr = sendReq

interpretSample :: forall t es a. WidgetChannel t :> es => Eff (Sample t : es) a -> Eff es a
interpretSample = interpret_ $ \case
  Sample b -> sendReq (WOSample b)

interpretHold :: forall t es a. WidgetChannel t :> es => Eff (Hold t : es) a -> Eff es a
interpretHold = interpret_ $ \case
  Hold a ev           -> sendReq (WOHold a ev)
  HoldDyn a ev        -> sendReq (WOHoldDyn a ev)
  HoldIncremental v0 ev -> sendReq (WOHoldIncremental v0 ev)
  BuildDynamic getV0 ev -> sendReq (WOBuildDynamic getV0 ev)
  HeadE ev            -> sendReq (WOHeadE ev)
  Now                 -> sr @t WONow
  FoldDyn f z ev      -> sendReq (WOFoldDyn f z ev)
  Count ev            -> sendReq (WOCount ev)
  Toggle z ev         -> sendReq (WOToggle z ev)
  AccumDyn f z ev     -> sendReq (WOAccumDyn f z ev)
  AccumMaybeDyn f z ev -> sendReq (WOAccumMaybeDyn f z ev)
  MapAccumMaybeDyn f z ev -> sendReq (WOMapAccumMaybeDyn f z ev)
  FoldDynMaybe f z ev -> sendReq (WOFoldDynMaybe f z ev)

interpretPostBuild :: forall t es a. WidgetChannel t :> es => Eff (PostBuild t : es) a -> Eff es a
interpretPostBuild = interpret_ $ \case
  GetPostBuild -> sr @t WOGetPostBuild

interpretTriggerEvent :: forall t es a. WidgetChannel t :> es => Eff (TriggerEvent t : es) a -> Eff es a
interpretTriggerEvent = interpret_ $ \case
  NewTriggerEvent               -> sr @t WONewTriggerEvent
  NewTriggerEventWithOnComplete -> sr @t WONewTriggerEventWithOnComplete
  Delay dt ev                   -> sendReq (WODelay dt ev)
  TickLossyFrom ev              -> sendReq (WOTickLossyFrom ev)

interpretPerformEvent :: forall t es a. WidgetChannel t :> es => Eff (PerformEvent t : es) a -> Eff es a
interpretPerformEvent = interpret_ $ \case
  PerformEvent ev  -> sendReq (WOPerformEvent ev)
  PerformEvent_ ev -> sendReq (WOPerformEvent_ ev)
  PerformRequestAsync ev -> sendReq (WOPerformRequestAsync ev)

interpretDom :: forall t es a. (WidgetChannel t :> es, IOE :> es) => Eff (Dom t : es) a -> Eff es a
interpretDom = interpret $ \env -> \case
  Element_ tag cfg child -> do
    sendReq (WOElementOpen tag cfg)
    a  <- localSeqUnlift env $ \unlift -> unlift child
    el <- sendReq @t WOElementClose
    return (el, a)
  Text_ t'     -> sr @t (WOText t')
  DynText_ d   -> sendReq (WODynText d)
  InputElement_ cfg    -> sendReq (WOInputElement cfg)
  TextAreaElement_ cfg -> sendReq (WOTextAreaElement cfg)
  SelectElement_ cfg child -> do
    sendReq (WOSelectElementOpen cfg)
    a   <- localSeqUnlift env $ \unlift -> unlift child
    sel <- sendReq @t WOSelectElementClose
    return (sel, a)
  TextNode_ cfg     -> sendReq (WOTextNode cfg)
  CommentNode_ cfg  -> sendReq (WOCommentNode cfg)
  PlaceRawElement_ rawEl    -> sendReq @t (WOPlaceRawElement rawEl)
  WrapRawElement_ rawEl cfg -> sendReq @t (WOWrapRawElement rawEl cfg)

interpretAdjustable :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t) => Eff (Adjustable t : es) a -> Eff es a
interpretAdjustable = interpret $ \env -> \case
  RunWithReplace initial replaceEv -> do
    reg <- getRegistry @t
    localUnliftIO env (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
      initialHandle <- startChild reg (unlift initial)
      let replaceActions = fmap (\eff -> startChild reg (unlift eff)) replaceEv
      sendReqIO reg (WORunWithReplace initialHandle replaceActions)
  TraverseIntMapWithKeyWithAdjust f initialMap patchEv -> do
    reg <- getRegistry @t
    localUnliftIO env (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
      initialHandles <- IntMap.traverseWithKey
        (\k v -> startChild reg (unlift (f k v)))
        initialMap
      let patchActions = fmap
            (\(PatchIntMap patch) -> fmap PatchIntMap $
                IntMap.traverseWithKey
                  (\k mv -> case mv of
                     Nothing -> pure Nothing
                     Just v  -> Just <$> startChild reg (unlift (f k v)))
                  patch)
            patchEv
      sendReqIO reg (WOTraverseIntMapWithAdjust initialHandles patchActions)
  TraverseDMapWithKeyWithAdjust
    (f0 :: forall x. (kk :: Type -> Type) x -> vv x -> _ (vv' x))
    initialMap patchEv -> do
    -- Explicit kind sig on 'kk' (pinned to 'Type -> Type') works around
    -- GHC-25897: the GADT constructor pins 'k :: Type -> Type', but GHC
    -- opens the existential with a fresh polykinded skolem and forgets
    -- the pin; the pattern signature re-binds the existential at
    -- 'Type -> Type' so downstream 'WOTraverseDMapWithAdjust' unifies
    -- cleanly. Mirrors the 'k :: Type -> Type' annotation on the
    -- Channel.hs WOTraverse* constructors. Monad slot is a wildcard
    -- because 'Eff localEs' isn't in scope at the pattern site. Fresh
    -- names 'f0', 'kk', 'vv', 'vv'' avoid shadowing the value-level
    -- 'k' / 'v' pattern bindings below.
    reg <- getRegistry @t
    localUnliftIO env (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
      initialHandles <- DMap.traverseWithKey
        (\k v -> ChildHandleF <$> startChild reg (unlift (f0 k v)))
        initialMap
      let patchActions = fmap
            (\patch -> PatchDMap.traversePatchDMapWithKey
                         (\k v -> ChildHandleF <$> startChild reg (unlift (f0 k v)))
                         patch)
            patchEv
      sendReqIO reg (WOTraverseDMapWithAdjust initialHandles patchActions)
  TraverseDMapWithKeyWithAdjustWithMove
    (f0 :: forall x. (kk :: Type -> Type) x -> vv x -> _ (vv' x))
    initialMap patchEv -> do
    -- Same GHC-25897 workaround as TraverseDMapWithKeyWithAdjust above:
    -- explicit 'kk :: Type -> Type' at the pattern re-pins the kind
    -- that the constructor originally promised but the existential
    -- opening discards.
    reg <- getRegistry @t
    localUnliftIO env (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
      initialHandles <- DMap.traverseWithKey
        (\k v -> ChildHandleF <$> startChild reg (unlift (f0 k v)))
        initialMap
      let patchActions = fmap
            (\patch -> PatchDMapWithMove.traversePatchDMapWithMoveWithKey
                         (\k v -> ChildHandleF <$> startChild reg (unlift (f0 k v)))
                         patch)
            patchEv
      sendReqIO reg (WOTraverseDMapWithAdjustWithMove initialHandles patchActions)

interpretPrerender :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t) => Eff (Prerender t : es) a -> Eff es a
interpretPrerender = interpret $ \env -> \case
  Prerender _server client -> do
    a <- localSeqUnlift env $ \unlift -> unlift client
    sendReq (WOHoldDyn a Reflex.never)

interpretJSM :: forall t es a. WidgetChannel t :> es => Eff (JSM' : es) a -> Eff es a
interpretJSM = interpret_ $ \case
  LiftJSM jsm -> sr @t (WOLiftJSM jsm)

interpretHasDocument :: forall t es a. WidgetChannel t :> es => Eff (HasDocument : es) a -> Eff es a
interpretHasDocument = interpret_ $ \case
  AskDocument -> sr @t WOAskDocument

interpretDomRenderHook :: forall t es a. WidgetChannel t :> es => Eff (DomRenderHook t : es) a -> Eff es a
interpretDomRenderHook = interpret $ \env -> \case
  WithRenderHook _hook child -> localSeqUnlift env $ \unlift -> unlift child
  RequestDomAction ev  -> sendReq (WORequestDomAction ev)
  RequestDomAction_ ev -> sendReq (WORequestDomAction_ ev)

interpretNotReady :: forall t es a. WidgetChannel t :> es => Eff (NotReady t : es) a -> Eff es a
interpretNotReady = interpret_ $ \case
  NotReadyUntil ev -> sendReq (WONotReadyUntil ev)
  NotReady_        -> sr @t WONotReady

interpretEventWriter :: forall t w es a. WidgetChannel t :> es => Eff (EventWriter t w : es) a -> Eff es a
interpretEventWriter = interpret_ $ \(TellEvent ev) -> sendReq (WOTellEvent ev)

interpretDynamicWriter :: forall t w es a. WidgetChannel t :> es => Eff (DynamicWriter t w : es) a -> Eff es a
interpretDynamicWriter = interpret_ $ \(TellDyn d) -> sendReq (WOTellDyn d)

interpretBehaviorWriter :: forall t w es a. WidgetChannel t :> es => Eff (BehaviorWriter t w : es) a -> Eff es a
interpretBehaviorWriter = interpret_ $ \(TellBehavior b) -> sendReq (WOTellBehavior b)

-- 'interpretRequester' lives with 'runRequesterEff' in
-- "Reflex.Effectful.Effect.Requester" — its aggregator body needs
-- 'Reflex.Requester.Base.Internal' primitives that would create a
-- circular import if it lived here.

interpretAll
  :: forall t es a. (WidgetChannel t :> es, IOE :> es, Reflex.Reflex t)
  => Eff ( Dom t : Hold t : Sample t : PostBuild t
         : PerformEvent t : TriggerEvent t
         : Adjustable t : Prerender t
         : JSM' : HasDocument : DomRenderHook t
         : NotReady t : es) a
  -> Eff es a
interpretAll
  = interpretNotReady @t
  . interpretDomRenderHook @t
  . interpretHasDocument @t
  . interpretJSM @t
  . interpretPrerender @t
  . interpretAdjustable @t
  . interpretTriggerEvent @t
  . interpretPerformEvent @t
  . interpretPostBuild @t
  . interpretSample @t
  . interpretHold @t
  . interpretDom @t
