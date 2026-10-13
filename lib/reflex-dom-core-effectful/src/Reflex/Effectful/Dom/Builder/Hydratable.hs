{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Effectful port of @Reflex.Dom.Builder.Hydratable@.
--
-- Upstream (@Reflex/Dom/Builder/Hydratable.hs@ lines 31, 89–102) ships
-- @newtype HydratableT m a = HydratableT { runHydratableT :: m a }@ with
-- a full @DomBuilder@ instance that intercepts @element@ /
-- @inputElement@ / @textAreaElement@ / @selectElement@ and rewrites
-- every config through 'makeHydratable' before delegating to the inner
-- builder. Every other MTL class (@MonadFix@, @MonadIO@, @MonadRef@,
-- @MonadHold@, @MonadSample@, @MonadReflexCreateTrigger@, @NotReady@,
-- @PerformEvent@, @PostBuild@, @TriggerEvent@, @Adjustable@,
-- @HasDocument@, @DomRenderHook@, @PrimMonad@, @MonadTrans@,
-- @MonadTransControl@) is a straight @lift@-forward.
--
-- The port expresses the same interception as a middleware on the
-- 'DomBuilder' effect via 'interpose' (from @effectful@). The middleware
-- rewrites the four config-carrying constructors and forwards every
-- other constructor unchanged; the surrounding row keeps whatever
-- 'MonadFix' \/ 'IOE' \/ 'Hold' \/ etc. it already had — no wrapper
-- forwarding needed because effect handling already threads them.
module Reflex.Effectful.Dom.Builder.Hydratable
  ( makeHydratable
  , runHydratable
  ) where

import qualified Data.Map as Map

import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpose, send)

import Reflex.Class (Reflex)
import Reflex.Dom.Core
  ( DomBuilder (..)
  , ElementConfig (..)
  , InputElementConfig (..)
  , SelectElementConfig (..)
  , TextAreaElementConfig (..)
  )
import Reflex.Dom.Core (hydratableAttribute)

-- | Rewrite an 'ElementConfig' so the produced element carries the
-- hydration marker attribute in its initial attribute set, and so any
-- caller-driven modifyAttributes stream cannot remove that marker.
-- Byte-identical to upstream 'Reflex.Dom.Builder.Hydratable.makeHydratable'.
makeHydratable :: Reflex t => ElementConfig er t space -> ElementConfig er t space
makeHydratable cfg = cfg
  { _elementConfig_initialAttributes = Map.insert hydratableAttribute "" $ _elementConfig_initialAttributes cfg
  , _elementConfig_modifyAttributes = fmap (Map.delete hydratableAttribute) <$> _elementConfig_modifyAttributes cfg
  }

-- | Effect-middleware port of upstream's @HydratableT@ transformer.
-- Wraps a widget so every 'element' \/ 'inputElement' \/
-- 'textAreaElement' \/ 'selectElement' call inside its scope has its
-- config rewritten through 'makeHydratable' before reaching the
-- underlying 'DomBuilder' effect.
--
-- The upstream 'DomBuilder' instance for @HydratableT m@ (lines 89–102)
-- had four config-rewriting arms and forwarded 'textNode' \/
-- 'commentNode' \/ 'placeRawElement' \/ 'wrapRawElement' unchanged;
-- 'interpose' + a case with the same four arms + a catch-all @send@
-- reproduces that shape exactly. The remaining ~13 upstream MTL
-- instances (@MonadFix@, @MonadIO@, @MonadRef@, @MonadHold@,
-- @MonadSample@, @MonadReflexCreateTrigger@, @NotReady@,
-- @PerformEvent@, @PostBuild@, @TriggerEvent@, @Adjustable@,
-- @HasDocument@, @DomRenderHook@, @PrimMonad@) were pure @lift@
-- forwarders — in the effect-shape port they are no-ops because
-- 'interpose' doesn't touch effects other than 'DomBuilder', so every
-- other effect the caller uses is threaded through untouched.
runHydratable
  :: forall t space es a
   . (DomBuilder t space :> es, Reflex t)
  => Eff es a
  -> Eff es a
runHydratable = interpose $ \_ e -> case e of
  DB_Element elementTag cfg body ->
    send @(DomBuilder t space)
      (DB_Element elementTag (makeHydratable cfg) body)
  DB_InputElement cfg ->
    send @(DomBuilder t space)
      (DB_InputElement cfg
        { _inputElementConfig_elementConfig =
            makeHydratable (_inputElementConfig_elementConfig cfg)
        })
  DB_TextAreaElement cfg ->
    send @(DomBuilder t space)
      (DB_TextAreaElement cfg
        { _textAreaElementConfig_elementConfig =
            makeHydratable (_textAreaElementConfig_elementConfig cfg)
        })
  DB_SelectElement cfg child ->
    send @(DomBuilder t space)
      (DB_SelectElement cfg
        { _selectElementConfig_elementConfig =
            makeHydratable (_selectElementConfig_elementConfig cfg)
        }
        child)
  other -> send other
