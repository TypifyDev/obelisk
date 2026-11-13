-- | Share Nix-resolved package sources with interactive Cabal commands.
module Jenga.Command.Cabal (withCabalProject) where

import Control.Monad (filterM)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)

-- | Keep the project's own configuration live across ghcid reloads. The
-- generated wrapper is temporary; user freeze/local files remain untouched.
withCabalProject :: ([String] -> IO a) -> IO a
withCabalProject action = do
  sources <- lookupEnv "JENGA_CABAL_SOURCES"
  case sources of
    Nothing -> action []
    Just path -> withProjectSources path action

withProjectSources :: FilePath -> ([String] -> IO a) -> IO a
withProjectSources sources action = do
  root <- getCurrentDirectory
  let project = root </> "cabal.project"
  overrides <- filterM doesFileExist [project <> ".freeze", project <> ".local"]
  withProjectFile (project : sources : overrides) $ \path ->
    action ["--project-dir=" <> root, "--project-file=" <> path]

withProjectFile :: [FilePath] -> (FilePath -> IO a) -> IO a
withProjectFile imports action =
  withSystemTempFile "jenga-cabal.project" $ \path handle -> do
    hPutStr handle $ unlines $ map ("import: " <>) imports
    hClose handle
    action path
