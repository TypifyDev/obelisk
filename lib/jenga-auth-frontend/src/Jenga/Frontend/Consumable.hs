module Jenga.Frontend.Consumable where

import Reflex (Reflex, Event, Dynamic)
import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, holdDyn, foldDyn)

type Consumable a = Maybe [a]

justNull :: Consumable a -> Bool
justNull = \case
  Just [] -> True
  _ -> False

depleted :: Consumable a -> Bool
depleted = justNull

holdConsumable :: (Reflex t, Hold t :> es, KnownTimeline es t) => Event t [a] -> Eff es (Dynamic t (Consumable a))
holdConsumable e = holdDyn Nothing $ Just <$> e

foldConsumable :: (Reflex t, Hold t :> es, KnownTimeline es t) => Event t [a] -> Eff es (Dynamic t (Consumable a))
foldConsumable e = foldDyn f Nothing e
  where
    f new_ = \case
      Nothing -> Just new_
      Just old -> Just $ old <> new_

dropDyn :: (Reflex t, Hold t :> es, KnownTimeline es t) => Event t () -> [a] -> Eff es (Dynamic t [a])
dropDyn e xs = foldDyn (\() xs' -> drop 1 xs') xs e

consumeWhen :: (Reflex t, Hold t :> es, KnownTimeline es t) => Event t () -> [a] -> Eff es (Dynamic t (Maybe a))
consumeWhen e xs = (fmap . fmap) f $ dropDyn e xs
  where
    f = \case
      [] -> Nothing
      (x_:_) -> Just x_
