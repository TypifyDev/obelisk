{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Jenga.Backend.DB.BeamInstances where

{-
For creating Beam instances for returning larger tuples
-}

--for stupid instances
import Database.Beam.Query (QExprToIdentity)
-- ThreadRewritable / ProjectibleWithPredicate 9-tuple instances now live
-- in beam-core (beam-914-fork) — see comment on the removed instances below.
import Database.Beam.Backend.Types (BeamBackend, Exposed, Nullable)
import Database.Beam.Backend.SQL.Row (FromBackendRow(..), FromBackendRowM)
--import Database.Beam.Backend.SQL.Row as X


import Data.Proxy (Proxy(..))
import GHC.Generics (Rep, R, K1(..), U1(..), M1(..), (:*:)(..), to)
import GHC.Types (Type)
import Data.Functor.Identity (Identity)




-- Base 4.20+ provides Generic (,,,,,,,,) natively; the manual standalone
-- deriving would duplicate it (GHC-59692).
-- deriving instance Generic ((,,,,,,,,) a b c d e f g h i)

-- | I copied this cuz its not exported
-- TODO: upstream this bullshit module
class GFromBackendRow be (exposed :: Type -> Type) rep where
  gFromBackendRow :: Proxy exposed -> FromBackendRowM be (rep ())
  gValuesNeeded :: Proxy be -> Proxy exposed -> Proxy rep -> Int
instance GFromBackendRow be e p => GFromBackendRow be (M1 t f e) (M1 t f p) where
  gFromBackendRow _ = M1 <$> gFromBackendRow (Proxy @e)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @e) (Proxy @p)
instance GFromBackendRow be e U1 where
  gFromBackendRow _ = pure U1
  gValuesNeeded _ _ _ = 0
instance (GFromBackendRow be aExp a, GFromBackendRow be bExp b) => GFromBackendRow be (aExp :*: bExp) (a :*: b) where
  gFromBackendRow _ = (:*:) <$> gFromBackendRow (Proxy @aExp) <*> gFromBackendRow (Proxy @bExp)
  gValuesNeeded be _ _ = gValuesNeeded be (Proxy @aExp) (Proxy @a) + gValuesNeeded be (Proxy @bExp) (Proxy @b)
instance FromBackendRow be x => GFromBackendRow be (K1 R (Exposed x)) (K1 R x) where
  gFromBackendRow _ = K1 <$> fromBackendRow
  gValuesNeeded be _ _ = valuesNeeded be (Proxy @x)
instance FromBackendRow be (t Identity) => GFromBackendRow be (K1 R (t Exposed)) (K1 R (t Identity)) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t Identity))
instance FromBackendRow be (t (Nullable Identity)) => GFromBackendRow be (K1 R (t (Nullable Exposed))) (K1 R (t (Nullable Identity))) where
    gFromBackendRow _ = K1 <$> fromBackendRow
    gValuesNeeded be _ _ = valuesNeeded be (Proxy @(t (Nullable Identity)))
-- instance BeamBackend be => FromBackendRow be () where
--   fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep ()))
--   valuesNeeded _ _ = 0


type instance QExprToIdentity (a, b, c, d, e, f, g, h, i) =
  ( QExprToIdentity a
  , QExprToIdentity b
  , QExprToIdentity c
  , QExprToIdentity d
  , QExprToIdentity e
  , QExprToIdentity f
  , QExprToIdentity g
  , QExprToIdentity h
  , QExprToIdentity i
  )

-- 9-tuple ThreadRewritable and ProjectibleWithPredicate instances are
-- provided by beam-core (beam-914-fork) itself in Database.Beam.Query.Internal
-- (shape-tagged forms: ThreadRewritable ('STuple '[s1..s9]) …,
-- ProjectibleWithPredicate ('STuple '[s1..s9]) …). Dropped from here to
-- avoid instance-arity conflict with the shape-tagged fork.

instance ( BeamBackend be
         , FromBackendRow be a, FromBackendRow be b, FromBackendRow be c
         , FromBackendRow be d, FromBackendRow be e, FromBackendRow be f
         , FromBackendRow be g, FromBackendRow be h, FromBackendRow be i ) =>
  FromBackendRow be (a, b, c, d, e, f, g, h, i) where
  fromBackendRow = to <$> gFromBackendRow (Proxy @(Rep (Exposed a, Exposed b, Exposed c, Exposed d, Exposed e, Exposed f, Exposed g, Exposed h, Exposed i)))
  valuesNeeded be _ = valuesNeeded be (Proxy @a) + valuesNeeded be (Proxy @b) + valuesNeeded be (Proxy @c) + valuesNeeded be (Proxy @d) +
                      valuesNeeded be (Proxy @e) + valuesNeeded be (Proxy @f) + valuesNeeded be (Proxy @g) + valuesNeeded be (Proxy @h) +
                      valuesNeeded be (Proxy @i)
