module Reflex.Effectful.Effect.Adjustable
  ( Adjustable(..)
  , runWithReplace, traverseIntMapWithKeyWithAdjust
  , traverseDMapWithKeyWithAdjust, traverseDMapWithKeyWithAdjustWithMove
  , widgetHold, widgetHold_, dyn, dyn_
  , simpleList, list, listWithKey, listHoldWithKey
  ) where

import           Data.Dependent.Map       (DMap)
import qualified Data.IntMap              as IntMap
import           Data.GADT.Compare        (GCompare)
import           Data.IntMap              (IntMap)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)

import           Reflex                   (Event, Dynamic, PatchDMap, PatchDMapWithMove, PatchIntMap, Reflex)
import qualified Reflex                   as R
import           Reflex.Effectful.Effect.Dom (Dom, blank)
import           Reflex.Effectful.Effect.Hold (Hold, holdDyn, foldDyn)
import           Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)

data Adjustable t :: Effect where
  RunWithReplace
    :: m a -> Event t (m b) -> Adjustable t m (a, Event t b)
  TraverseIntMapWithKeyWithAdjust
    :: (IntMap.Key -> v -> m v') -> IntMap v -> Event t (PatchIntMap v) -> Adjustable t m (IntMap v', Event t (PatchIntMap v'))
  TraverseDMapWithKeyWithAdjust
    :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> Adjustable t m (DMap k v', Event t (PatchDMap k v'))
  TraverseDMapWithKeyWithAdjustWithMove
    :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> Adjustable t m (DMap k v', Event t (PatchDMapWithMove k v'))

type instance DispatchOf (Adjustable t) = 'Dynamic

runWithReplace :: (KnownTimeline es t, Adjustable t :> es) => Eff es a -> Event t (Eff es b) -> Eff es (a, Event t b)
runWithReplace initial ev = send (RunWithReplace initial ev)

traverseIntMapWithKeyWithAdjust :: (KnownTimeline es t, Adjustable t :> es) => (IntMap.Key -> v -> Eff es v') -> IntMap v -> Event t (PatchIntMap v) -> Eff es (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjust f im ev = send (TraverseIntMapWithKeyWithAdjust f im ev)

traverseDMapWithKeyWithAdjust :: (KnownTimeline es t, Adjustable t :> es, GCompare k) => (forall a. k a -> v a -> Eff es (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> Eff es (DMap k v', Event t (PatchDMap k v'))
traverseDMapWithKeyWithAdjust f dm ev = send (TraverseDMapWithKeyWithAdjust f dm ev)

traverseDMapWithKeyWithAdjustWithMove :: (KnownTimeline es t, Adjustable t :> es, GCompare k) => (forall a. k a -> v a -> Eff es (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> Eff es (DMap k v', Event t (PatchDMapWithMove k v'))
traverseDMapWithKeyWithAdjustWithMove f dm ev = send (TraverseDMapWithKeyWithAdjustWithMove f dm ev)

widgetHold :: (KnownTimeline es t, Adjustable t :> es, Hold t :> es) => Eff es a -> Event t (Eff es a) -> Eff es (Dynamic t a)
widgetHold initial ev = do { (a0, a') <- runWithReplace initial ev; holdDyn a0 a' }

widgetHold_ :: (KnownTimeline es t, Adjustable t :> es) => Eff es () -> Event t (Eff es ()) -> Eff es ()
widgetHold_ initial ev = do { _ <- runWithReplace initial ev; return () }

dyn :: (KnownTimeline es t, Adjustable t :> es, Hold t :> es, Dom t :> es, Reflex t) => Dynamic t (Eff es a) -> Eff es (Event t a)
dyn d = do { (_, ev) <- runWithReplace blank (R.updated d); return ev }

dyn_ :: (KnownTimeline es t, Adjustable t :> es, Dom t :> es, Reflex t) => Dynamic t (Eff es ()) -> Eff es ()
dyn_ d = do { _ <- runWithReplace blank (R.updated d); return () }

-- | Effectful wrapper for 'Reflex.Dom.list' — reruns child widget whenever
-- the input map's key set changes. Built on top of 'listWithKey'.
list
  :: ( KnownTimeline es t
     , Adjustable t :> es, Hold t :> es, PostBuild t :> es
     , Ord k, Reflex t
     )
  => Dynamic t (Map k v)
  -> (Dynamic t v -> Eff es b)
  -> Eff es (Dynamic t (Map k b))
list dm mkChild = listWithKey dm (\_ dv -> mkChild dv)

-- | Effectful wrapper for 'Reflex.Dom.simpleList'. Built on 'listWithKey'
-- keyed by integer index; result is the 'Map.elems' projection.
simpleList
  :: forall es t v b.
     ( KnownTimeline es t
     , Adjustable t :> es, Hold t :> es, PostBuild t :> es
     , Reflex t
     )
  => Dynamic t [v]
  -> (Dynamic t v -> Eff es b)
  -> Eff es (Dynamic t [b])
simpleList xs mkChild = do
  let byKey :: Dynamic t (Map Int v)
      byKey = Map.fromList . zip [(0 :: Int)..] <$> xs
  fmap Map.elems <$> listWithKey byKey (\_ dv -> mkChild dv)

-- | Effectful wrapper for 'Reflex.Dom.listWithKey'.
--
-- Implementation: rebuild the entire child map every time the input
-- 'Dynamic' fires (or on 'PostBuild' for the initial map). The child widget
-- receives a 'constDyn' of its value — so per-key value updates arrive via
-- a rebuild rather than propagating through the child's 'Dynamic'.
--
-- Reflex-dom's canonical 'listWithKey' does per-key diffing via
-- 'traverseDMapWithKeyWithAdjust' which is more efficient AND gives each
-- child a stable per-key 'Dynamic'. That optimisation isn't wired into this
-- effectful wrapper yet; consumers that depend on per-key stability should
-- either fold 'runWithReplace' + 'traverseDMapWithKeyWithAdjust' themselves
-- or wait for an efficient port.
listWithKey
  :: forall es t k v b.
     ( KnownTimeline es t
     , Adjustable t :> es, Hold t :> es, PostBuild t :> es
     , Ord k, Reflex t
     )
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> Eff es b)
  -> Eff es (Dynamic t (Map k b))
listWithKey dm mkChild = do
  pb <- getPostBuild
  let rebuild :: Map k v -> Eff es (Map k b)
      rebuild = Map.traverseWithKey (\k v -> mkChild k (R.constDyn v))
      initialFire = R.tag (R.current dm) pb
      changes     = R.leftmost [initialFire, R.updated dm]
  widgetHold (pure Map.empty) (rebuild <$> changes)

-- | Effectful wrapper for 'Reflex.Dom.listHoldWithKey'. Tracks an evolving
-- @Map k v@ by folding patches over 'initial'; rebuilds the child map each
-- time the accumulated map changes.
listHoldWithKey
  :: forall es t k v b.
     ( KnownTimeline es t
     , Adjustable t :> es, Hold t :> es, PostBuild t :> es
     , Ord k, Reflex t
     )
  => Map k v
  -> Event t (Map k (Maybe v))
  -> (k -> v -> Eff es b)
  -> Eff es (Dynamic t (Map k b))
listHoldWithKey initial patches mkChild = do
  pb <- getPostBuild
  let applyPatch :: Map k (Maybe v) -> Map k v -> Map k v
      applyPatch p m0 = Map.foldrWithKey step m0 p
        where step k Nothing  = Map.delete k
              step k (Just v) = Map.insert k v
      rebuild :: Map k v -> Eff es (Map k b)
      rebuild = Map.traverseWithKey mkChild
  currentMap <- foldDyn applyPatch initial patches
  let initialFire = R.tag (R.current currentMap) pb
      changes     = R.leftmost [initialFire, R.updated currentMap]
  widgetHold (pure Map.empty) (rebuild <$> changes)
