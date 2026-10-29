module Jenga.Command.Init
  ( InitOpts (..)
  , initParser
  , run
  ) where

import Control.Monad (forM_, unless, when)
import Options.Applicative hiding (info)
import System.Directory (copyFile, createDirectoryIfMissing, doesPathExist,
                         getPermissions, listDirectory, setOwnerWritable,
                         setPermissions)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink, getSymbolicLinkStatus,
                           isDirectory, isSymbolicLink, readSymbolicLink
                           )
import System.Which (staticWhich)

import Paths_jenga_command (getDataDir)

import Jenga.Command.Utils (die, info, runProcess, strip)

gitPath :: FilePath
gitPath = $(staticWhich "git")

data InitOpts = InitOpts
  { initProjectName :: String
  }

initParser :: Parser InitOpts
initParser = InitOpts
  <$> strArgument (metavar "PROJECT-NAME" <> help "Name of the new project directory")

run :: InitOpts -> IO ()
run opts = do
  let name = initProjectName opts

  exists <- doesPathExist name
  when exists $ die $ name <> " already exists"

  dataDir <- getDataDir
  let skeletonDir = dataDir </> "skeleton"

  info $ "Creating project: " <> name

  -- Read the Jenga root path (baked in at nix build time).
  let jengaRootFile = dataDir </> "jenga-root"
  jengaRootExists <- doesPathExist jengaRootFile
  jengaRoot <- if jengaRootExists
    then strip <$> readFile jengaRootFile
    else die "jenga-root not found in data directory"

  -- Copy skeleton, preserving symlinks as symlinks (avoids cycles and nix store issues).
  copyDirectoryRecursive skeletonDir name

  -- Make everything writable (skeleton may come from nix store).
  makeWritableRecursive name

  -- Create thunks/jenga symlink to the live Jenga checkout.
  -- This ensures nix evaluation uses the same derivation hashes as the developer's builds.
  createDirectoryIfMissing True (name </> "thunks")
  createSymbolicLink jengaRoot (name </> "thunks" </> "jenga")

  -- Initialize git.
  runProcess gitPath ["-C", name, "init", "-q"]
  runProcess gitPath ["-C", name, "add", "-A"]
  runProcess gitPath ["-C", name, "commit", "-q", "-m", "Initialize project"]

  info $ "Project ready: " <> name
  putStrLn ""
  putStrLn $ "  cd " <> name
  putStrLn   "  nix-shell --run 'jenga run'"

-- | Recursively copy a directory, preserving symlinks as symlinks.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  forM_ entries $ \entry -> do
    let srcPath = src </> entry
        dstPath = dst </> entry
    status <- getSymbolicLinkStatus srcPath
    if isSymbolicLink status
      then do
        target <- readSymbolicLink srcPath
        createSymbolicLink target dstPath
      else if isDirectory status
        then copyDirectoryRecursive srcPath dstPath
        else copyFile srcPath dstPath

-- | Recursively set owner-writable on all files and directories.
makeWritableRecursive :: FilePath -> IO ()
makeWritableRecursive path = do
  status <- getSymbolicLinkStatus path
  unless (isSymbolicLink status) $ do
    perms <- getPermissions path
    setPermissions path (setOwnerWritable True perms)
    when (isDirectory status) $ do
      entries <- listDirectory path
      forM_ entries $ \entry ->
        makeWritableRecursive (path </> entry)
