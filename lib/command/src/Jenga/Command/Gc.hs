{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Explicit per-derivation garbage collection for Jenga projects.
--
-- Design: NEVER run a blanket @nix-collect-garbage@. The user names
-- which store paths die. 'jenga gc list' enumerates candidate GC roots
-- (defaulting to roots that reference the current project tree so
-- unrelated projects' build state is never touched); 'jenga gc delete'
-- takes explicit store paths and removes them one by one.
--
-- Hard refusals:
--  * Any path referenced by @/nix/var/nix/profiles/system*@ or
--    @/run/current-system@ (NixOS system profile — never touched).
--  * Non-existent paths (fails loud rather than silently skipping).
module Jenga.Command.Gc
  ( GcCommand (..)
  , ListScope (..)
  , gcParser
  , run
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Options.Applicative
import System.Directory
  ( canonicalizePath
  , doesPathExist
  , getCurrentDirectory
  , getSymbolicLinkTarget
  , listDirectory
  , pathIsSymbolicLink
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Which (staticWhich)

import qualified Jenga.Command.Utils as Utils
import Jenga.Command.Utils (strip)

-- | Resolved path to the @nix-store@ binary. staticWhich guarantees
-- the binary existed at compile time; nix provides it in shell.nix.
nixStorePath :: FilePath
nixStorePath = $(staticWhich "nix-store")

-- | Directory where @nix-build@ / @nix-shell@ drop automatic GC roots.
autoGcrootsDir :: FilePath
autoGcrootsDir = "/nix/var/nix/gcroots/auto"

-- | Any symlink chain terminating in a path with one of these
-- prefixes is OFF-LIMITS — this is the NixOS system profile and
-- deleting its referents can brick a boot.
systemProfilePrefixes :: [String]
systemProfilePrefixes =
  [ "/nix/var/nix/profiles/system"
  , "/run/current-system"
  , "/run/booted-system"
  ]

data GcCommand
  = GcList ListScope
  | GcDelete DeleteOpts

data ListScope
  = ScopeCwd    -- ^ only gcroots whose target lives under the current working directory
  | ScopeHome   -- ^ gcroots under @$HOME@, excluding system refs

data DeleteOpts = DeleteOpts
  { deleteYes :: Bool
  , deletePaths :: [FilePath]
  }

gcParser :: Parser GcCommand
gcParser = subparser
  (  command "list"
       (info (helper <*> listP)
         (progDesc "List GC-root candidates (default scope: this project's cwd)"))
  <> command "delete"
       (info (helper <*> deleteP)
         (progDesc "Delete named store paths explicitly. No implicit sweeping."))
  )
  where
    listP = GcList <$> flag ScopeCwd ScopeHome
      ( long "home"
      <> help "List all gcroots under $HOME (default: only roots pointing into cwd)" )
    deleteP = fmap GcDelete $ DeleteOpts
      <$> switch (long "yes" <> short 'y' <> help "Skip the confirm prompt")
      <*> some
            (argument str
              ( metavar "STORE_PATH..."
              <> help "/nix/store/<hash>-<name> paths to delete. Each is named by you." ))

-- | Candidate GC root: the symlink file plus what it points at.
data Candidate = Candidate
  { candRoot :: FilePath   -- ^ absolute path to the symlink in @gcroots/auto/@
  , candTarget :: FilePath -- ^ the symlink's immediate target (may itself be a project 'result' symlink)
  }

run :: GcCommand -> IO ()
run (GcList scope) = runList scope
run (GcDelete opts) = runDelete opts

-- ---------------------------------------------------------------------------
-- list

runList :: ListScope -> IO ()
runList scope = do
  cands <- listCandidates scope
  if null cands
    then Utils.info "No GC-root candidates matched the scope."
    else do
      Utils.info $ show (length cands) <> " GC-root candidate(s):"
      forM_ (sortOn (\(Candidate _ t) -> t) cands) $ \(Candidate r t) ->
        putStrLn $ "  " <> r <> "  ->  " <> t
      Utils.info "To delete a derivation, resolve the store path and run:"
      Utils.info "  jenga gc delete /nix/store/<hash>-<name>"

listCandidates :: ListScope -> IO [Candidate]
listCandidates scope = do
  cwdCanon <- getCurrentDirectory >>= canonicalizePath
  mHome <- lookupEnv "HOME"
  entries <- listDirectory autoGcrootsDir
  cands <- forM entries $ \name -> do
    let rootPath = autoGcrootsDir </> name
    isLink <- pathIsSymbolicLink rootPath
    if not isLink
      then pure Nothing
      else do
        tgt <- getSymbolicLinkTarget rootPath
        pure (Just (Candidate rootPath tgt))
  pure $ filter (keep cwdCanon mHome) (catMaybes cands)
  where
    keep cwdCanon mHome (Candidate _ tgt)
      | isSystemReferenced tgt = False
      | otherwise = case scope of
          ScopeCwd ->
            cwdCanon `isPrefixOf` tgt || cwdCanon `isInfixOf` tgt
          ScopeHome -> case mHome of
            Just home -> home `isPrefixOf` tgt
            Nothing   -> False

isSystemReferenced :: FilePath -> Bool
isSystemReferenced p = any (`isPrefixOf` p) systemProfilePrefixes

-- ---------------------------------------------------------------------------
-- delete

runDelete :: DeleteOpts -> IO ()
runDelete (DeleteOpts yes rawPaths) = do
  paths <- mapM validateStorePath rawPaths
  reports <- mapM describe paths
  Utils.info "The following store paths will be deleted:"
  forM_ reports $ \(p, refs, roots) -> do
    putStrLn $ "  " <> p
    putStrLn $ "    referrers: " <> show (length refs)
    putStrLn $ "    gcroots:   " <> show (length roots)
  confirmed <-
    if yes
      then pure True
      else do
        putStr "Proceed? [y/N] "
        hFlush stdout
        answer <- getLine
        pure (answer `elem` ["y", "Y", "yes", "YES"])
  unless confirmed $ do
    Utils.info "Aborted — no paths deleted."
    exitFailure
  forM_ reports $ \(p, _refs, roots) -> do
    -- Remove any auto-gcroot symlinks that pin this path so the delete can proceed.
    forM_ roots $ \rootSymlink -> do
      Utils.info $ "Removing GC root: " <> rootSymlink
      runNixStore ["--delete", rootSymlink]
    Utils.info $ "Deleting store path: " <> p
    runNixStore ["--delete", p]
  Utils.info "Done."

-- | Expand a user-supplied path argument into a canonical store path.
-- Dies on: missing path, non-store path, system-referenced path.
validateStorePath :: FilePath -> IO FilePath
validateStorePath raw = do
  canon <- canonicalizePath raw
  unless ("/nix/store/" `isPrefixOf` canon) $
    Utils.die $ "not a /nix/store path: " <> raw <> " (canonical: " <> canon <> ")"
  exists <- doesPathExist canon
  unless exists $
    Utils.die $ "store path does not exist: " <> canon
  when (isSystemReferenced canon) $
    Utils.die $ "refusing to touch system-referenced path: " <> canon
  -- Also refuse if any system profile links to it (checked via roots query below).
  roots <- queryRoots canon
  case filter isSystemReferenced roots of
    (sysRoot : _) ->
      Utils.die $ "refusing — path is pinned by a system profile root: " <> sysRoot
    [] -> pure ()
  pure canon

-- | For reporting: what referrers and gcroots does this path have.
describe :: FilePath -> IO (FilePath, [FilePath], [FilePath])
describe p = do
  refs <- queryReferrers p
  roots <- queryRoots p
  -- Only remove roots that live under auto/ (user's own build-side roots).
  -- System roots are already excluded by validateStorePath.
  let autoRoots = filter (autoGcrootsDir `isPrefixOf`) roots
  pure (p, refs, autoRoots)

queryReferrers :: FilePath -> IO [FilePath]
queryReferrers p = linesOf <$> nixStoreCapture ["--query", "--referrers", p]

queryRoots :: FilePath -> IO [FilePath]
queryRoots p = mapMaybe takeRootPath . lines <$> nixStoreCapture ["--query", "--roots", p]
  where
    -- @nix-store --query --roots@ formats as "/path/to/root -> /nix/store/...".
    takeRootPath line = case words line of
      (root : _) -> Just (strip root)
      []         -> Nothing

linesOf :: String -> [FilePath]
linesOf = filter (not . null) . map strip . lines

nixStoreCapture :: [String] -> IO String
nixStoreCapture args = do
  (ec, out, err) <- readCreateProcessWithExitCode (proc nixStorePath args) ""
  case ec of
    ExitSuccess -> pure out
    ExitFailure c ->
      Utils.die $ "nix-store " <> unwords args <> " exited " <> show c
         <> if null err then "" else ": " <> err

runNixStore :: [String] -> IO ()
runNixStore args = do
  (ec, _out, err) <- readCreateProcessWithExitCode (proc nixStorePath args) ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure c ->
      Utils.die $ "nix-store " <> unwords args <> " exited " <> show c
         <> if null err then "" else ": " <> err
