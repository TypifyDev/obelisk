{-# LANGUAGE AllowAmbiguousTypes #-}

module Reflex.Effectful.Effect.DomRenderHook (DomRenderHook(..), withRenderHook, requestDomAction, requestDomAction_) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import           Reflex.Effectful.Types   (KnownTimeline)
import           Reflex                   (Event)
import           GHCJS.DOM.Types          (JSM)

data DomRenderHook t :: Effect where
  WithRenderHook   :: (forall x. JSM x -> JSM x) -> m a -> DomRenderHook t m a
  RequestDomAction :: Event t (JSM a) -> DomRenderHook t m (Event t a)
  RequestDomAction_ :: Event t (JSM a) -> DomRenderHook t m ()

type instance DispatchOf (DomRenderHook t) = 'Dynamic

withRenderHook :: forall t es a. DomRenderHook t :> es => (forall x. JSM x -> JSM x) -> Eff es a -> Eff es a
withRenderHook hook child = send @(DomRenderHook t) (WithRenderHook hook child)

requestDomAction :: (KnownTimeline es t, DomRenderHook t :> es) => Event t (JSM a) -> Eff es (Event t a)
requestDomAction = send . RequestDomAction

requestDomAction_ :: (KnownTimeline es t, DomRenderHook t :> es) => Event t (JSM a) -> Eff es ()
requestDomAction_ = send . RequestDomAction_
