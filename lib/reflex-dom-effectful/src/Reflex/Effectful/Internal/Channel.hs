{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Effectful.Internal.Channel
  ( WidgetChannel
  , sendReq
  , sendReqIO
  , runWidgetChannel
  , WidgetOp(..)
  , WidgetReq(..)
  , ChildHandle(..)
  , ChildHandleF(..)
  , ChannelRegistry(..)
  , startChild
  , getRegistry
  ) where

import           Control.Concurrent       (forkIO, myThreadId, ThreadId)
import           Control.Concurrent.MVar
import           Data.IntMap              (IntMap)
import           Data.IORef
import           Data.Kind                (Type)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import           Data.Dependent.Map       (DMap)
import           Data.GADT.Compare        (GCompare)

import           Effectful
import           Effectful.Dispatch.Static

import           Data.Time.Clock           (NominalDiffTime, UTCTime)
import           Reflex                   (Event, Patch, PatchTarget, TickInfo)
import qualified Reflex
import           GHCJS.DOM.Types          (JSM)
import qualified Reflex.Dom.Builder.Class as DBC
import qualified Reflex.Dom.Xhr           as Xhr
import           Reflex.Requester.Base.Internal (RequesterData)

import           Reflex.Effectful.Types

data ChannelRegistry t = ChannelRegistry
  { _cr_channels :: IORef (Map.Map ThreadId (MVar (WidgetReq t)))
  }

data WidgetOp t result where
  WOSample          :: Behavior t a -> WidgetOp t a
  WOHold            :: a -> Event t a -> WidgetOp t (Behavior t a)
  WOHoldDyn         :: a -> Event t a -> WidgetOp t (Dynamic t a)
  WOHoldIncremental :: Patch p => PatchTarget p -> Event t p -> WidgetOp t (Incremental t p)
  WOBuildDynamic    :: PushM t a -> Event t a -> WidgetOp t (Dynamic t a)
  WOHeadE           :: Event t a -> WidgetOp t (Event t a)
  WONow             :: WidgetOp t (Event t ())
  WOFoldDyn         :: (a -> b -> b) -> b -> Event t a -> WidgetOp t (Dynamic t b)
  WOCount           :: Num b => Event t a -> WidgetOp t (Dynamic t b)
  WOToggle          :: Bool -> Event t a -> WidgetOp t (Dynamic t Bool)
  WOAccumDyn        :: (a -> b -> a) -> a -> Event t b -> WidgetOp t (Dynamic t a)
  WOAccumMaybeDyn   :: (a -> b -> Maybe a) -> a -> Event t b -> WidgetOp t (Dynamic t a)
  WOMapAccumMaybeDyn :: (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> WidgetOp t (Dynamic t a, Event t c)
  WOFoldDynMaybe    :: (a -> b -> Maybe b) -> b -> Event t a -> WidgetOp t (Dynamic t b)
  WOGetPostBuild    :: WidgetOp t (Event t ())
  WONewTriggerEvent :: WidgetOp t (Event t a, a -> IO ())
  WONewTriggerEventWithOnComplete :: WidgetOp t (Event t a, a -> IO () -> IO ())
  WOPerformEvent    :: Event t (JSM a) -> WidgetOp t (Event t a)
  WOPerformEvent_   :: Event t (JSM ()) -> WidgetOp t ()
  WOText            :: Text -> WidgetOp t ()
  WODynText         :: Dynamic t Text -> WidgetOp t ()
  -- | Open an element tag. The reply @()@ signals \"child may proceed\";
  -- the actual 'Element' handle is delivered by the matching 'WOElementClose'
  -- reply once the child block completes. Delivering the handle via the
  -- close reply (instead of a side-channel 'IORef') makes it structurally
  -- impossible for the handle to be observed before the close point.
  WOElementOpen     :: Text -> ElementConfig EventResult t GhcjsDomSpace -> WidgetOp t ()
  WOElementClose    :: WidgetOp t (Element EventResult GhcjsDomSpace t)
  WOInputElement    :: InputElementConfig EventResult t GhcjsDomSpace -> WidgetOp t (InputElement EventResult GhcjsDomSpace t)
  WOTextAreaElement :: TextAreaElementConfig EventResult t GhcjsDomSpace -> WidgetOp t (TextAreaElement EventResult GhcjsDomSpace t)
  -- | Same open/close discipline as 'WOElementOpen'; handle delivered on close.
  WOSelectElementOpen :: SelectElementConfig EventResult t GhcjsDomSpace -> WidgetOp t ()
  WOSelectElementClose :: WidgetOp t (SelectElement EventResult GhcjsDomSpace t)
  WOTextNode        :: TextNodeConfig t -> WidgetOp t (TextNode GhcjsDomSpace t)
  WOCommentNode     :: CommentNodeConfig t -> WidgetOp t (CommentNode GhcjsDomSpace t)
  WOPlaceRawElement  :: DBC.RawElement GhcjsDomSpace -> WidgetOp t ()
  WOWrapRawElement   :: DBC.RawElement GhcjsDomSpace -> RawElementConfig EventResult t GhcjsDomSpace -> WidgetOp t (Element EventResult GhcjsDomSpace t)
  WORunWithReplace   :: ChildHandle t a -> Event t (IO (ChildHandle t b)) -> WidgetOp t (a, Event t b)
  WOTraverseIntMapWithAdjust
    :: IntMap (ChildHandle t v')
    -> Event t (IO (Reflex.PatchIntMap (ChildHandle t v')))
    -> WidgetOp t (IntMap v', Event t (Reflex.PatchIntMap v'))
  -- 'k' is pinned to kind 'Type -> Type' (rather than the poly-kinded
  -- 'k1 -> Type' GHC would otherwise infer) because
  -- 'traverseDMapWithKeyWithAdjust' from reflex-dom takes a per-key action
  -- @forall a. k a -> v a -> m (v' a)@ with @a :: Type@, so the pattern
  -- match downstream (in 'WidgetLoop.dispatch') otherwise fails to unify
  -- the poly-kinded 'k' with reflex-dom's expected '* -> *' shape.
  WOTraverseDMapWithAdjust
    :: forall (k :: Type -> Type) t v'
     . GCompare k
    => DMap k (ChildHandleF t v')
    -> Event t (IO (Reflex.PatchDMap k (ChildHandleF t v')))
    -> WidgetOp t (DMap k v', Event t (Reflex.PatchDMap k v'))
  WOTraverseDMapWithAdjustWithMove
    :: forall (k :: Type -> Type) t v'
     . GCompare k
    => DMap k (ChildHandleF t v')
    -> Event t (IO (Reflex.PatchDMapWithMove k (ChildHandleF t v')))
    -> WidgetOp t (DMap k v', Event t (Reflex.PatchDMapWithMove k v'))
  WOLiftJSM         :: JSM a -> WidgetOp t a
  WOAskDocument     :: WidgetOp t (DBC.RawDocument GhcjsDomSpace)
  WORequestDomAction  :: Event t (JSM a) -> WidgetOp t (Event t a)
  WORequestDomAction_ :: Event t (JSM a) -> WidgetOp t ()
  WONotReadyUntil   :: Event t a -> WidgetOp t ()
  WONotReady        :: WidgetOp t ()
  WOTellEvent       :: Event t a -> WidgetOp t ()
  WOTellDyn         :: Dynamic t a -> WidgetOp t ()
  WOTellBehavior    :: Behavior t a -> WidgetOp t ()
  -- Timer/event primitives
  WODelay           :: NominalDiffTime -> Event t a -> WidgetOp t (Event t a)
  WOTickLossyFrom   :: Event t (NominalDiffTime, UTCTime) -> WidgetOp t (Event t TickInfo)
  -- XHR
  WOPerformRequestAsync :: Event t (Xhr.XhrRequest ()) -> WidgetOp t (Event t Xhr.XhrResponse)
  -- | Requester per-scope response-bridge allocation.
  --
  -- Runs ONCE per 'runRequesterEff' scope. The widget-loop allocates
  -- a 'newTriggerEvent' pair — the fired event carries per-tag
  -- decoded responses; the trigger callback is invoked by
  -- 'runRequesterEff' as it demuxes the raw response stream from
  -- 'matchResponsesWithRequests'.
  --
  -- Distinct from 'WONewTriggerEvent' only in that the payload type
  -- is fixed to @RequesterData rsp@ (whose type is existentially
  -- hidden inside 'WidgetOp'; the caller of 'WORequesting' owns
  -- 'rsp' at the type-erasure boundary). Making it a named op
  -- documents where in the WidgetOp GADT the requester
  -- aggregation ties in.
  --
  -- The aggregator itself (tag counter, routing table, per-call
  -- response taps) lives entirely in 'runRequesterEff' — see
  -- "Reflex.Effectful.Effect.Requester" for the aggregator body.
  WORequesting
    :: forall t rsp.
       WidgetOp t (Event t (RequesterData rsp), RequesterData rsp -> IO ())
       -- ^ Per-scope response-bridge: fire the trigger with a
       -- per-tag decoded response; the event fans through the
       -- Effectful-side aggregator.
  WODone            :: WidgetOp t ()

data WidgetReq t where
  WidgetReq :: WidgetOp t a -> MVar a -> WidgetReq t

data ChildHandle t a = ChildHandle
  { _ch_channel :: MVar (WidgetReq t)
  , _ch_result  :: MVar a
  }

-- | Newtype wrapper letting 'ChildHandle' inhabit a DMap indexed by a
-- type-indexed key. For a key @k :: Type -> Type@, we store one
-- @ChildHandle t (v' a)@ per @k a@ so the child's per-index result type
-- lines up with the reflex-dom side of the adjustable operation.
newtype ChildHandleF t v' a = ChildHandleF { unChildHandleF :: ChildHandle t (v' a) }

data WidgetChannel t :: Effect
type instance DispatchOf (WidgetChannel t) = 'Static 'WithSideEffects
newtype instance StaticRep (WidgetChannel t) = WidgetChannelRep (ChannelRegistry t)

-- | Explicit 'forall' orders 't' first so callers can pin the timeline via
-- 'sendReq @t' — needed for ops whose return type doesn't mention 't'
-- (e.g. 'WOPlaceRawElement', 'WOAskDocument', 'WONotReady').
sendReq :: forall t a es. WidgetChannel t :> es => WidgetOp t a -> Eff es a
sendReq op = do
  WidgetChannelRep reg <- getStaticRep
  unsafeEff_ $ sendReqIO reg op

sendReqIO :: ChannelRegistry t -> WidgetOp t a -> IO a
sendReqIO reg op = do
  tid <- myThreadId
  chanMap <- readIORef (_cr_channels reg)
  case Map.lookup tid chanMap of
    Nothing -> error "reflex-dom-effectful: no channel registered for this thread"
    Just chanVar -> do
      respVar <- newEmptyMVar
      putMVar chanVar (WidgetReq op respVar)
      takeMVar respVar

runWidgetChannel :: IOE :> es => MVar (WidgetReq t) -> Eff (WidgetChannel t : es) a -> Eff es a
runWidgetChannel chanVar eff = do
  reg <- unsafeEff_ $ do
    tid <- myThreadId
    mapRef <- newIORef (Map.singleton tid chanVar)
    return (ChannelRegistry mapRef)
  evalStaticRep (WidgetChannelRep reg) eff

getRegistry :: WidgetChannel t :> es => Eff es (ChannelRegistry t)
getRegistry = do
  WidgetChannelRep reg <- getStaticRep
  return reg

startChild :: ChannelRegistry t -> IO a -> IO (ChildHandle t a)
startChild reg childIO = do
  childChan <- newEmptyMVar
  resultVar <- newEmptyMVar
  _ <- forkIO $ do
    tid <- myThreadId
    atomicModifyIORef' (_cr_channels reg) (\m -> (Map.insert tid childChan m, ()))
    a <- childIO
    putMVar resultVar a
    doneResp <- newEmptyMVar
    putMVar childChan (WidgetReq WODone doneResp)
    takeMVar doneResp
    atomicModifyIORef' (_cr_channels reg) (\m -> (Map.delete tid m, ()))
  return (ChildHandle childChan resultVar)

-- | Inner-channel op for the per-scope MTL 'RequesterT' opened by
-- 'WORequesting'. Sent by 'interpretRequester' (Effectful side),
-- consumed by 'processLoopRequester' (WidgetLoop side).
--
-- 'ROCall' \/ 'ROCall_' carry the batched request event with
-- key type 'k' existentially hidden; 'req' \/ 'rsp' are pinned
-- by the enclosing 'runRequesterEff' scope which owns both endpoints.
data RequesterOp t result where
  ROCall  :: GCompare k
          => Reflex.Event t (DMap k req)
          -> RequesterOp t (Reflex.Event t (DMap k rsp))
  ROCall_ :: GCompare k
          => Reflex.Event t (DMap k req)
          -> RequesterOp t ()
  RODone  :: RequesterOp t ()

-- | Existentially-wrapped request paired with a reply 'MVar', mirroring
-- 'WidgetReq'. The inner requester channel is 'MVar (RequesterReq t)'.
data RequesterReq t where
  RequesterReq :: RequesterOp t a -> MVar a -> RequesterReq t

