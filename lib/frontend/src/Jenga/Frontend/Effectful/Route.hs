-- | Jenga's routing effects (previously 'Reflex.Effectful.Effect.Jenga.Route').
-- Effect wrappers around 'Routed', 'SetRoute', 'RouteToUrl' from
-- 'Reflex.Dom.Contrib.Router' / 'Obelisk.Router.Frontend'.
module Jenga.Frontend.Effectful.Route
  ( -- * Routed effect
    Routed(..)
  , askRoute

    -- * SetRoute effect
  , SetRoute(..)
  , setRoute
  , modifyRoute

    -- * RouteToUrl effect
  , RouteToUrl(..)
  , askRouteToUrl
  ) where

import           Data.Text                (Text)

import qualified Effectful                as Eff
import           Effectful                ((:>))
import qualified Effectful.Dispatch.Dynamic as Eff
import qualified Reflex                   as Rflx

-- ─── Routed ────────────────────────────────────────────────────

data Routed t r :: Eff.Effect where
  AskRoute :: Routed t r m (Rflx.Dynamic t r)

type instance Eff.DispatchOf (Routed t r) = 'Eff.Dynamic

askRoute :: Routed t r :> es => Eff.Eff es (Rflx.Dynamic t r)
askRoute = Eff.send AskRoute

-- ─── SetRoute ──────────────────────────────────────────────────

data SetRoute t r :: Eff.Effect where
  SetRoute    :: Rflx.Event t r -> SetRoute t r m ()
  ModifyRoute :: Rflx.Event t (r -> r) -> SetRoute t r m ()

type instance Eff.DispatchOf (SetRoute t r) = 'Eff.Dynamic

setRoute :: SetRoute t r :> es => Rflx.Event t r -> Eff.Eff es ()
setRoute = Eff.send . SetRoute

modifyRoute :: SetRoute t r :> es => Rflx.Event t (r -> r) -> Eff.Eff es ()
modifyRoute = Eff.send . ModifyRoute

-- ─── RouteToUrl ────────────────────────────────────────────────

data RouteToUrl r :: Eff.Effect where
  AskRouteToUrl :: RouteToUrl r m (r -> Text)

type instance Eff.DispatchOf (RouteToUrl r) = 'Eff.Dynamic

askRouteToUrl :: RouteToUrl r :> es => Eff.Eff es (r -> Text)
askRouteToUrl = Eff.send AskRouteToUrl
