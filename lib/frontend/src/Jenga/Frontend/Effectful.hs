-- | Jenga's effectful frontend combined constraint (previously
-- 'Reflex.Class'). Bundles the core Reflex-effectful widget
-- constraints with jenga-specific effects (Configs, CookiesEff, Routed,
-- SetRoute, RouteToUrl).
--
-- Historical note: the MTL-era @Reflex.Class'@ was
-- superseded by 'FrontendWidget' in 'Jenga.Frontend', which consolidates
-- the 17-constraint aggregate in one type synonym. 'JengaWidget'' is now
-- an alias for backward compatibility with downstream consumers.
--
-- The re-exports below provide 'Configs', 'CookiesEff', 'Routed',
-- 'SetRoute', 'RouteToUrl' effects and the 'Reflex.Dom.Core' umbrella.
module Jenga.Frontend.Effectful
  ( -- * Jenga-specific effects
    module Jenga.Frontend.Effectful.Configs
  , module Jenga.Frontend.Effectful.Cookies
  , module Jenga.Frontend.Effectful.Route

    -- * Combined constraint aliases
  , JengaWidget'

    -- * Re-export the library-agnostic reflex-dom-effectful core
  , module Rflx
  ) where

import           Reflex.Effectful.DomHost         as Rflx
import           Jenga.Frontend                       (FrontendWidget)
import           Jenga.Frontend.Effectful.Configs
import           Jenga.Frontend.Effectful.Cookies
import           Jenga.Frontend.Effectful.Route

-- | Backward-compatible alias for 'Jenga.Frontend.FrontendWidget'. The
-- MTL-era @WidgetEff'@ bundle was replaced by the 17-constraint
-- 'FrontendWidget' synonym in 'Jenga.Frontend'; this alias keeps the
-- @JengaWidget'@ name available for downstream consumers.
type JengaWidget' es t r = FrontendWidget es t r
