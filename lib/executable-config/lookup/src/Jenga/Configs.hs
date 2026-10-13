{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Jenga.Configs
  ( HasConfigs
  , getConfigs
  , getConfig
  , getTextConfig
  , runConfigs
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Effectful (Eff, Effect, DispatchOf, Dispatch (Static), (:>))
import Effectful.Dispatch.Static
  ( SideEffects (NoSideEffects)
  , StaticRep
  , evalStaticRep
  , getStaticRep
  )

-- | Effect providing access to the process-wide config map loaded at startup.
data HasConfigs :: Effect

type instance DispatchOf HasConfigs = 'Static 'NoSideEffects
newtype instance StaticRep HasConfigs = HasConfigs (Map Text ByteString)

-- | Read the entire config map.
getConfigs :: HasConfigs :> es => Eff es (Map Text ByteString)
getConfigs = do
  HasConfigs cs <- getStaticRep
  pure cs

-- | Look up a single config value.
getConfig :: HasConfigs :> es => Text -> Eff es (Maybe ByteString)
getConfig k = do
  cs <- getConfigs
  pure $ Map.lookup k cs

-- | Look up a single config value, decoded as UTF-8 text.
getTextConfig :: HasConfigs :> es => Text -> Eff es (Maybe Text)
getTextConfig k = fmap T.decodeUtf8 <$> getConfig k

-- | Run a computation with the given config map in scope.
runConfigs
  :: Map Text ByteString
  -> Eff (HasConfigs ': es) a
  -> Eff es a
runConfigs cs = evalStaticRep (HasConfigs cs)
