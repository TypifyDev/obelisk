-- | Jenga's routing effects (previously 'Reflex.Class').
-- Effect wrappers around 'Routed', 'SetRoute', 'RouteToUrl'.
--
-- Now a pure re-export of 'Jenga.Route.Effect' — the canonical home for
-- these effects. Previously this module independently declared its own
-- 'Routed' / 'SetRoute' / 'RouteToUrl' GADTs, which were structurally
-- identical to but nominally distinct from those in 'Jenga.Route.Effect',
-- preventing handlers wired here from composing with handlers wired to
-- the canonical types. Kept as a re-export shim for source compatibility
-- with existing downstream imports.
module Jenga.Frontend.Effectful.Route (module Jenga.Route.Effect) where

import Jenga.Route.Effect
