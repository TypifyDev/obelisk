{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hot-reload-list config for @jenga run@.
--
-- The config is Dhall. Default location is @configs/jenga-run.dhall@
-- relative to the invocation cwd (which, for a Jenga project, is the
-- repo root). If the file is absent, 'defaultConfig' is used — a
-- single-dir discovery ('thunks') plus the standard
-- @lib:backend@ / @exe:frontend@ pane targets.
--
-- See @configs/jenga-run.dhall@ in this repo for the schema and
-- comments on each field.
module Jenga.Command.Run.Config
  ( JengaRunConfig (..)
  , PaneConfig (..)
  , defaultConfig
  , defaultConfigPath
  , loadConfig
  ) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Dhall (FromDhall, auto, inputFile)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

-- | Top-level config read from the Dhall file.
data JengaRunConfig = JengaRunConfig
  { discoveryDirs :: [FilePath]
    -- ^ Directories to scan for thunk manifests and plugin restart paths.
    -- Cabal's multi-repl loads the modules with their package settings.
    -- Each path is relative to the invocation cwd; @jenga run@
    -- enumerates each directory's immediate children and treats
    -- each child as a candidate unpacked thunk.
  , backendPane :: PaneConfig
  , frontendPane :: PaneConfig
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromDhall)

-- | Shared configuration schema. The foreground multi-repl launcher uses
-- backendPane.onReload; pane targets remain available to other launchers.
data PaneConfig = PaneConfig
  { target :: Text
    -- ^ cabal repl target (e.g. @"lib:backend"@, @"exe:frontend"@,
    -- @"lib:common"@).
  , onReload :: Maybe Text
    -- ^ Optional GHCi expression to run after each successful reload
    -- (ghcid @--run=@). @Just ":main"@ to launch the server after
    -- each green reload.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromDhall)

-- | Config file path when the user didn't override.
defaultConfigPath :: FilePath
defaultConfigPath = "configs/jenga-run.dhall"

-- | Hard-coded fallback used when 'defaultConfigPath' is absent.
-- Chosen to match the shape 'Jenga.Command.Run' had before the config
-- system existed: single @thunks@ discovery dir, backend + frontend
-- cabal repl targets with @:main@ on reload.
defaultConfig :: JengaRunConfig
defaultConfig = JengaRunConfig
  { discoveryDirs = ["thunks"]
  , backendPane  = PaneConfig { target = "lib:backend",  onReload = Just "DevMain.devMain" }
  , frontendPane = PaneConfig { target = "exe:frontend", onReload = Just ":main" }
  }

-- | Read the config, falling back to 'defaultConfig' when the file
-- doesn't exist. Any OTHER error (parse error, type error) is
-- re-thrown — a corrupt config is a bug the user needs to see, not
-- silently paper over.
loadConfig :: FilePath -> IO JengaRunConfig
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure defaultConfig
    else do
      result <- try @IOException (inputFile auto path)
      case result of
        Right cfg -> pure cfg
        Left ioErr -> ioError ioErr
