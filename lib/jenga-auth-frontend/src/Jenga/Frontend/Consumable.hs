module Jenga.Frontend.Consumable where

import Reflex.Class (Reflex, Event, Dynamic, MonadHold, holdDyn)
import Reflex.Dynamic (foldDyn)
import Control.Monad.Fix (MonadFix)

type Consumable a = Maybe [a]

justNull :: Consumable a -> Bool
justNull = \case
  Just [] -> True
  _ -> False

depleted :: Consumable a -> Bool
depleted = justNull

holdConsumable :: (Reflex t, MonadHold t m) => Event t [a] -> m (Dynamic t (Consumable a))
holdConsumable e = holdDyn Nothing $ Just <$> e

foldConsumable :: (Reflex t, MonadHold t m, MonadFix m) => Event t [a] -> m (Dynamic t (Consumable a))
foldConsumable e = foldDyn f Nothing e
  where
    f new_ = \case
      Nothing -> Just new_
      Just old -> Just $ old <> new_

dropDyn :: (Reflex t, MonadHold t m, MonadFix m) => Event t () -> [a] -> m (Dynamic t [a])
dropDyn e xs = foldDyn (\() xs' -> drop 1 xs') xs e

consumeWhen :: (Reflex t, MonadHold t m, MonadFix m) => Event t () -> [a] -> m (Dynamic t (Maybe a))
consumeWhen e xs = (fmap . fmap) f $ dropDyn e xs
  where
    f = \case
      [] -> Nothing
      (x_:_) -> Just x_
