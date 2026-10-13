-- | Source-compatibility re-export.
--
-- The canonical home for the Jenga routing effect surface has moved to
-- 'Jenga.Route.Effect' — it is a jenga-owned module and no longer squats
-- under the @Reflex.Effectful.*@ namespace owned by the
-- reflex-effectful / reflex-dom-core-effectful packages.
--
-- This module is retained purely so downstream call sites that still
-- @import Reflex.Class@ keep compiling. New code
-- should import 'Jenga.Route.Effect' directly.
module Reflex.Effectful.Effect.Jenga.Route
  ( module Jenga.Route.Effect
  ) where

import Jenga.Route.Effect
