{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- | Grid layout with compile-time column budget tracking.
-- QualifiedDo is isolated here so it doesn't affect normal do-notation.
module Frontend.Grid (dashboardGrid) where

import Prelude hiding ((>>=), (>>), return)
import qualified Grid.Effect.Eff as G
import Grid.Effect.Rational
import Effectful (Eff)
import Reflex.Class (WidgetEff')

-- | Dashboard layout: 4 rows, all summing to exactly 12 columns.
-- Compile-time proven — changing any @N x@ to overflow is a type error.
dashboardGrid
  :: WidgetEff' es t
  => Eff es ()   -- ^ sidebar (4 cols)
  -> Eff es ()   -- ^ main panel (8 cols)
  -> Eff es ()   -- ^ card 1 (4 cols)
  -> Eff es ()   -- ^ card 2 (4 cols)
  -> Eff es ()   -- ^ card 3 (4 cols)
  -> Eff es ()   -- ^ info left (6 cols)
  -> Eff es ()   -- ^ info right (6 cols)
  -> Eff es ()   -- ^ footer (12 cols)
  -> G.GridEff (N 12) (N 12) (N 0) es ()
dashboardGrid sidebar main' c1 c2 c3 i1 i2 footer = G.do
  G.consume @(N 4) (G.liftEff sidebar)
    G..>> G.consume @(N 8) (G.liftEff main')

  G.consume @(N 4) (G.liftEff c1)
    G..>> G.consume @(N 4) (G.liftEff c2)
    G..>> G.consume @(N 4) (G.liftEff c3)

  G.consume @(N 6) (G.liftEff i1)
    G..>> G.consume @(N 6) (G.liftEff i2)

  G.consume @(N 12) (G.liftEff footer)
