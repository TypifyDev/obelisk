{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Effectful port of @Reflex.Dom.Builder.InputDisabled@.
--
-- Upstream (@Reflex/Dom/Builder/InputDisabled.hs@ lines 30, 88–100)
-- ships @newtype InputDisabledT m a = InputDisabledT { runInputDisabledT
-- :: m a }@ with a @DomBuilder@ instance that intercepts
-- @inputElement@ / @textAreaElement@ / @selectElement@ (NOT @element@ —
-- note the asymmetry vs @HydratableT@) and rewrites every config
-- through 'disableElementConfig' before delegating to the inner
-- builder. Every other MTL class (@MonadFix@, @MonadIO@, @MonadRef@,
-- @MonadHold@, @MonadSample@, @MonadReflexCreateTrigger@, @NotReady@,
-- @PerformEvent@, @PostBuild@, @TriggerEvent@, @Adjustable@,
-- @HasDocument@, @DomRenderHook@, @PrimMonad@, @MonadTrans@,
-- @MonadTransControl@) is a straight @lift@-forward.
--
-- The port expresses the same interception as a middleware on the
-- 'DomBuilder' effect via 'interpose'. The middleware rewrites three
-- config-carrying constructors and forwards every other constructor
-- (including 'DB_Element' — same as upstream) unchanged.
module Reflex.Effectful.Dom.Builder.InputDisabled
  ( disableElementConfig
  , runInputDisabled
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

-- | Rewrite an 'ElementConfig' so the produced element carries the
-- HTML @disabled@ attribute in its initial attribute set, and so any
-- caller-driven modifyAttributes stream cannot remove that attribute.
-- Byte-identical to upstream 'Reflex.Dom.Builder.InputDisabled.disableElementConfig'.
disableElementConfig :: Reflex t => ElementConfig er t space -> ElementConfig er t space
disableElementConfig cfg = cfg
  { _elementConfig_initialAttributes = Map.insert "disabled" "disabled" $ _elementConfig_initialAttributes cfg
  , _elementConfig_modifyAttributes = fmap (Map.delete "disabled") <$> _elementConfig_modifyAttributes cfg
  }

-- | Effect-middleware port of upstream's @InputDisabledT@ transformer.
-- Wraps a widget so every 'inputElement' \/ 'textAreaElement' \/
-- 'selectElement' call inside its scope has its config rewritten
-- through 'disableElementConfig' before reaching the underlying
-- 'DomBuilder' effect. 'element' passes through unchanged — matching
-- upstream's asymmetry between 'HydratableT' (which rewrites 'element'
-- too) and 'InputDisabledT' (which does not).
runInputDisabled
  :: forall t space es a
   . (DomBuilder t space :> es, Reflex t)
  => Eff es a
  -> Eff es a
runInputDisabled = interpose $ \_ e -> case e of
  DB_InputElement cfg ->
    send @(DomBuilder t space)
      (DB_InputElement cfg
        { _inputElementConfig_elementConfig =
            disableElementConfig (_inputElementConfig_elementConfig cfg)
        })
  DB_TextAreaElement cfg ->
    send @(DomBuilder t space)
      (DB_TextAreaElement cfg
        { _textAreaElementConfig_elementConfig =
            disableElementConfig (_textAreaElementConfig_elementConfig cfg)
        })
  DB_SelectElement cfg child ->
    send @(DomBuilder t space)
      (DB_SelectElement cfg
        { _selectElementConfig_elementConfig =
            disableElementConfig (_selectElementConfig_elementConfig cfg)
        }
        child)
  other -> send other
