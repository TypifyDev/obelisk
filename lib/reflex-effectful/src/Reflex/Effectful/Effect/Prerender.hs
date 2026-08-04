module Reflex.Effectful.Effect.Prerender (Prerender(..), prerender, prerender_) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Dynamic)

data Prerender t :: Effect where
  Prerender :: m a -> m a -> Prerender t m (Dynamic t a)

type instance DispatchOf (Prerender t) = 'Dynamic

prerender :: (KnownTimeline es t, Prerender t :> es) => Eff es a -> Eff es a -> Eff es (Dynamic t a)
prerender server client = send (Prerender server client)

-- | Discard the resulting 'Dynamic'. Mirrors 'Reflex.Dom.prerender_'.
prerender_ :: (KnownTimeline es t, Prerender t :> es) => Eff es () -> Eff es () -> Eff es ()
prerender_ server client = do
  _ <- prerender server client
  pure ()
