-- | Jenga's effectful frontend combined constraint (previously
-- 'Reflex.Effectful.Jenga'). Bundles the core Reflex-effectful widget
-- constraints ('WidgetEff'' from @reflex-effectful@) with jenga-specific
-- effects (Configs, CookiesEff, Routed, SetRoute, RouteToUrl).
module Jenga.Frontend.Effectful
  ( -- * Jenga-specific effects
    module Jenga.Frontend.Effectful.Configs
  , module Jenga.Frontend.Effectful.Cookies
  , module Jenga.Frontend.Effectful.Route

    -- * Combined constraint aliases
  , JengaWidget'

    -- * Re-export the library-agnostic reflex-effectful core
  , module Rflx
  ) where

import qualified Effectful                as Eff

import           Reflex.Effectful                     as Rflx
import           Jenga.Frontend.Effectful.Configs
import           Jenga.Frontend.Effectful.Cookies
import           Jenga.Frontend.Effectful.Route

-- | Full Jenga widget constraint — everything a frontend widget needs.
--
-- Replaces the 20+ MTL constraint @JengaWidget t route m@:
--
-- @
-- -- BEFORE (MTL, 20+ constraints):
-- myPage :: JengaWidget t route m => RoutedT t route m ()
-- myPage = subRoute_ $ \\case
--   FrontendRoute_Main -> el "h1" $ text "Home"
--
-- -- AFTER (Effectful, one constraint):
-- myPage :: JengaWidget' es t (R FrontendRoute) => Eff es ()
-- myPage = subRoute_ $ \\case
--   FrontendRoute_Main -> el "h1" $ text "Home"
-- @
type JengaWidget' es t r =
  ( Rflx.WidgetEff' es t
  , Configs         :> es
  , CookiesEff      :> es
  , Routed t r      :> es
  , SetRoute t r    :> es
  , RouteToUrl r    :> es
  , Eff.IOE         :> es
  )
