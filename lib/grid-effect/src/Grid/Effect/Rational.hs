{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
#endif

-- | Type-level rationals for budget tracking.
--
-- Rationals are encoded as @'(Nat, Nat)@ pairs (numerator, denominator).
-- Arithmetic is done via type families; GHC's type-checker enforces
-- the constraints at compile time.
module Grid.Effect.Rational
  ( -- * Constructors
    N, F, Inf

    -- * Type families
  , RatNum, RatDen
  , RatLeq, RatSub, RatAdd

    -- * Reification
  , KnownRat(..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Kind (Constraint)
import GHC.TypeLits (Nat, KnownNat, natVal)
import GHC.TypeNats (type (<=), type (-), type (*), type (+))

-- | Integer as a rational: @N 12 = '(12, 1)@
type N (n :: Nat) = '(n, 1)

-- | Fraction: @F 1 3 = '(1, 3)@
type F (n :: Nat) (d :: Nat) = '(n, d)

-- | Unbounded budget. @RatLeq n Inf@ holds for any realistic @n@.
type Inf = N 1000000

-- | Numerator
type family RatNum (r :: (Nat, Nat)) :: Nat where
  RatNum '(n, _) = n

-- | Denominator
type family RatDen (r :: (Nat, Nat)) :: Nat where
  RatDen '(_, d) = d

-- | Helper: constraint that @ad <= cb@ (used to avoid nested type family
-- application inside 'Assert' on GHC 9.14+).
type family RatLeqHelper (ad :: Nat) (cb :: Nat) :: Constraint where
  RatLeqHelper ad cb = ad <= cb

-- | @a/b <= c/d@ via cross-multiplication. Overflow = compile error.
type family RatLeq (x :: (Nat, Nat)) (y :: (Nat, Nat)) :: Constraint where
  RatLeq '(a, b) '(c, d) = RatLeqHelper (a * d) (c * b)

-- | Helper: form @'(ad - bc, bd)@ from already-computed products.
type family RatSubHelper (ad :: Nat) (bc :: Nat) (bd :: Nat) :: (Nat, Nat) where
  RatSubHelper ad bc bd = '(ad - bc, bd)

-- | @a/b - c/d = (ad - bc) / bd@. Safe when guarded by 'RatLeq'.
type family RatSub (x :: (Nat, Nat)) (y :: (Nat, Nat)) :: (Nat, Nat) where
  RatSub '(a, b) '(c, d) = RatSubHelper (a * d) (b * c) (b * d)

-- | Helper: form @'(ad + bc, bd)@ from already-computed products.
type family RatAddHelper (ad :: Nat) (bc :: Nat) (bd :: Nat) :: (Nat, Nat) where
  RatAddHelper ad bc bd = '(ad + bc, bd)

-- | @a/b + c/d = (ad + bc) / bd@.
type family RatAdd (x :: (Nat, Nat)) (y :: (Nat, Nat)) :: (Nat, Nat) where
  RatAdd '(a, b) '(c, d) = RatAddHelper (a * d) (b * c) (b * d)

-- | Reify both components at runtime.
class (KnownNat (RatNum r), KnownNat (RatDen r)) => KnownRat (r :: (Nat, Nat)) where
  ratNum :: Proxy r -> Integer
  ratNum _ = natVal (Proxy @(RatNum r))

  ratDen :: Proxy r -> Integer
  ratDen _ = natVal (Proxy @(RatDen r))

  ratToFloat :: Proxy r -> Float
  ratToFloat p = fromIntegral (ratNum p) / fromIntegral (ratDen p)

instance (KnownNat n, KnownNat d) => KnownRat '(n, d)
