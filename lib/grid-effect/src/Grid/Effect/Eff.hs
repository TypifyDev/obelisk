{-# LANGUAGE CPP #-}
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- | Effectful integration for type-level grid budgets.
--
-- @GridEff@ is @GridI@ with @Eff es@ as the base monad. This gives you
-- both compile-time budget tracking AND all effectful effects:
--
-- @
-- {-\# LANGUAGE QualifiedDo \#-}
-- import qualified Grid.Effect.Eff as G
--
-- myLayout :: G.GridEff (N 12) (N 12) (N 0) es ()
-- myLayout = G.do
--   _ <- G.consume \@(N 4) sidebar     -- 4 columns, can use Dom, Hold, etc.
--   _ <- G.consume \@(N 8) mainPanel   -- 8 columns, same effects available
--   G.return ()
--
-- -- This won't compile:
-- bad = G.do
--   _ <- G.consume \@(N 7) left
--   _ <- G.consume \@(N 6) right    -- ERROR: 7 + 6 = 13 > 12
--   G.return ()
-- @
module Grid.Effect.Eff
  ( -- * The combined type
    GridEff

    -- * Running
  , runGridEff
  , runGridEff_exact

    -- * Operations
  , consume
  , consumeWith
  , row
  , liftEff

    -- * Horizontal sequencing
  , (.>>)

    -- * QualifiedDo
  , (>>=), (>>), return

    -- * Re-exports
  , module Grid.Effect.Rational
  ) where

import Prelude hiding ((>>=), (>>), return)

import Effectful (Eff)
import Grid.Effect.Rational
import Grid.Effect.Indexed (GridI(..))
import qualified Grid.Effect.Indexed as Ix

-- | @GridI@ over @Eff es@.
--
-- Tracks grid budget at the type level while giving access to all
-- effectful effects in @es@ (Dom, Hold, PerformEvent, Reader, etc.)
type GridEff total rem_in rem_out es = GridI total rem_in rem_out (Eff es)

-- | Run the grid, discarding leftover budget.
runGridEff :: GridEff total total rem_out es a -> Eff es a
runGridEff = Ix.runGridI

-- | Run the grid, requiring exact consumption.
runGridEff_exact :: (RatNum rem_out ~ 0) => GridEff total total rem_out es a -> Eff es a
runGridEff_exact = Ix.runGridI_exact

-- | Consume @n@ from the budget. Child widget runs in a scope of @n@.
--
-- @
-- consume \@(N 6) $ do
--   el "div" $ text "I have 6 columns"  -- uses Dom effect from es
-- @
consume :: (RatLeq n rem, Monad (Eff es))
        => GridEff n n child_rem es a
        -> GridEff t rem (RatSub rem n) es a
consume = Ix.consume

-- | Consume @n@ and run a plain @Eff es@ action (no inner grid tracking).
--
-- @
-- consumeWith \@(N 4) $ el "div" $ text "sidebar"
-- @
consumeWith :: forall n rem t es a. (RatLeq n rem, Monad (Eff es))
            => Eff es a
            -> GridEff t rem (RatSub rem n) es a
consumeWith action = Ix.consume @n (Ix.liftGridI action :: GridEff n n n es a)

-- | Enter a nested row scope. Budget resets to total.
row :: Monad (Eff es)
    => GridEff t t child_rem es a
    -> GridEff t rem rem es a
row = Ix.runChildren

-- | Lift a plain @Eff es@ action. Budget unchanged.
liftEff :: Eff es a -> GridEff t i i es a
liftEff = Ix.liftGridI

-- | Horizontal sequencing — same row.
(.>>) :: Monad (Eff es)
      => GridEff t i mid es a
      -> GridEff t mid j es b
      -> GridEff t i j es b
(.>>) = (Ix..>>)

-- QualifiedDo: row-break bind
(>>=) :: Monad (Eff es)
      => GridEff t i x es a
      -> (a -> GridEff t t j es b)
      -> GridEff t i j es b
(>>=) = Ix.gridBind

(>>) :: Monad (Eff es)
     => GridEff t i x es a
     -> GridEff t t j es b
     -> GridEff t i j es b
(>>) = Ix.gridThen

return :: a -> GridEff t i i es a
return = Ix.gridReturn
