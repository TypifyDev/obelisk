{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shared route type surface used by both the obelisk-style encoder
-- module ('Jenga.Route') and the servant-style module
-- ('Jenga.Route.Servant').
--
-- These types are declared once here so both encoder implementations
-- name the exact same types (and so instances stay in one place, not
-- orphan-declared in each encoder module).
module Jenga.Route.Core
  ( R
  , pattern (:/)
  , pattern (:.), type (:.)
  , PageName
  , FullRoute (..)
  , _FullRoute_Backend
  , _FullRoute_Frontend
  , JengaRoute (..)
  , _JengaRoute_App
  , _JengaRoute_Resource
  , ResourceRoute (..)
  , JSaddleWarpRoute (..)
  , IndexOnlyRoute (..)
  , hoistR
  ) where

import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare
import Data.GADT.Show
import Data.Kind (Type)
import Data.Map (Map)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.Universe
import Data.Universe.Some
import Control.Lens (makePrisms)

import Jenga.Route.TH

-- | Existentially-quantified route.
type R f = DSum f Identity

pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b
{-# COMPLETE (:/) #-}
infixr 5 :/

infixr 5 :.
type (:.) = (,)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 810
{-# COMPLETE (:.) #-}
#endif
#endif
pattern (:.) :: a -> b -> a :. b
pattern a :. b = (a, b)

hoistR :: (forall x. f x -> g x) -> R f -> R g
hoistR f (x :=> Identity y) = f x :/ y

-- | Path segments + query parameters.
type PageName = ([Text], Map Text (Maybe Text))

-- | Full app route: backend or frontend.
data FullRoute :: (Type -> Type) -> (Type -> Type) -> Type -> Type where
  FullRoute_Backend :: br a -> FullRoute br fr a
  FullRoute_Frontend :: JengaRoute fr a -> FullRoute br fr a

-- | Jenga-side split: app frontend vs jenga-served resource.
data JengaRoute :: (Type -> Type) -> Type -> Type where
  JengaRoute_App :: f a -> JengaRoute f a
  JengaRoute_Resource :: ResourceRoute a -> JengaRoute f a

-- | Jenga's canonical resource routes.
data ResourceRoute :: Type -> Type where
  ResourceRoute_Static :: ResourceRoute [Text]
  ResourceRoute_Ghcjs :: ResourceRoute [Text]
  ResourceRoute_JSaddleWarp :: ResourceRoute (R JSaddleWarpRoute)
  ResourceRoute_Version :: ResourceRoute ()

data JSaddleWarpRoute :: Type -> Type where
  JSaddleWarpRoute_JavaScript :: JSaddleWarpRoute ()
  JSaddleWarpRoute_WebSocket :: JSaddleWarpRoute ()
  JSaddleWarpRoute_Sync :: JSaddleWarpRoute [Text]

data IndexOnlyRoute :: Type -> Type where
  IndexOnlyRoute :: IndexOnlyRoute ()

concat <$> mapM deriveRouteComponent
  [ ''ResourceRoute
  , ''JSaddleWarpRoute
  , ''IndexOnlyRoute
  ]

instance (GShow br, GShow fr) => GShow (FullRoute br fr) where
  gshowsPrec p = \case
    FullRoute_Backend x -> showParen (p > 10) (showString "FullRoute_Backend " . gshowsPrec 11 x)
    FullRoute_Frontend x -> showParen (p > 10) (showString "FullRoute_Frontend " . gshowsPrec 11 x)

instance (GEq br, GEq fr) => GEq (FullRoute br fr) where
  geq (FullRoute_Backend x) (FullRoute_Backend y) = geq x y
  geq (FullRoute_Frontend x) (FullRoute_Frontend y) = geq x y
  geq _ _ = Nothing

instance (GCompare br, GCompare fr) => GCompare (FullRoute br fr) where
  gcompare (FullRoute_Backend _) (FullRoute_Frontend _) = GLT
  gcompare (FullRoute_Frontend _) (FullRoute_Backend _) = GGT
  gcompare (FullRoute_Backend x) (FullRoute_Backend y) = gcompare x y
  gcompare (FullRoute_Frontend x) (FullRoute_Frontend y) = gcompare x y

instance (UniverseSome br, UniverseSome fr) => UniverseSome (FullRoute br fr) where
  universeSome = [Some (FullRoute_Backend x) | Some x <- universeSome]
              ++ [Some (FullRoute_Frontend x) | Some x <- universeSome]

instance UniverseSome f => UniverseSome (JengaRoute f) where
  universeSome = concat
    [ (\(Some x) -> Some (JengaRoute_App x)) <$> universe
    , (\(Some x) -> Some (JengaRoute_Resource x)) <$> (universe @(Some ResourceRoute))
    ]

instance GEq f => GEq (JengaRoute f) where
  geq (JengaRoute_App x) (JengaRoute_App y) = geq x y
  geq (JengaRoute_Resource x) (JengaRoute_Resource y) = geq x y
  geq _ _ = Nothing

instance GCompare f => GCompare (JengaRoute f) where
  gcompare (JengaRoute_App x) (JengaRoute_App y) = gcompare x y
  gcompare (JengaRoute_Resource x) (JengaRoute_Resource y) = gcompare x y
  gcompare (JengaRoute_App _) (JengaRoute_Resource _) = GLT
  gcompare (JengaRoute_Resource _) (JengaRoute_App _) = GGT

instance GShow appRoute => GShow (JengaRoute appRoute) where
  gshowsPrec prec = \case
    JengaRoute_App appRoute -> showParen (prec > 10) $
      showString "JengaRoute_App " . gshowsPrec 11 appRoute
    JengaRoute_Resource appRoute -> showParen (prec > 10) $
      showString "JengaRoute_Resource " . gshowsPrec 11 appRoute

makePrisms ''JengaRoute
makePrisms ''FullRoute
