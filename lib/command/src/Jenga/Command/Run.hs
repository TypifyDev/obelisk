-- | Run one foreground ghcid session over the selected local libraries.
-- Unpacked thunk sources reload as interpreted modules; plugin-dependent
-- thunks trigger a restart. Diagnostics and console logs are written under
-- /tmp. The user's GHCi configuration is restored on exit.
module Jenga.Command.Run
  ( RunOpts (..)
  , runParser
  , run
  ) where

import Control.Exception (bracket_)
import Control.Monad (filterM, when)
import Data.List (isInfixOf)
import System.Posix.Files (setFileMode)
import Options.Applicative hiding (info)
import System.Directory
  ( doesDirectoryExist, doesFileExist, doesPathExist
  , getHomeDirectory, listDirectory
  , renamePath
  )
import System.FilePath ((</>), takeExtension)
import System.Which (staticWhich)

import qualified Data.Text as T
import Jenga.Command.Run.Config
  ( JengaRunConfig (..), PaneConfig (..)
  , defaultConfigPath, loadConfig
  )
import Jenga.Command.Utils
  ( devGhcOptions
  , execProcess, info
  )

ghcidPath, cabalPath :: FilePath
ghcidPath = $(staticWhich "ghcid")
cabalPath = $(staticWhich "cabal")

data RunOpts = RunOpts
  { runReplOnly  :: Bool
  , runTarget    :: Maybe String
  , runExtraArgs :: [String]
  }

runParser :: Parser RunOpts
runParser = RunOpts
  <$> switch
      (  long "repl-only"
      <> help "Compatibility flag; the loop always starts directly in ghcid."
      )
  <*> optional (strOption
      (  long "target"
      <> metavar "TARGET"
      <> help "cabal repl target (default: all:libs)."
      ))
  <*> many (strArgument
      (  metavar "CABAL_ARGS..."
      <> help "Extra arguments passed to cabal repl."
      ))

-- ---------------------------------------------------------------------------
-- Top-level run
-- ---------------------------------------------------------------------------

run :: RunOpts -> IO ()
run opts = do
  cfg <- loadConfig defaultConfigPath
  info $ "Loaded config from " <> defaultConfigPath
         <> " (discovery dirs: " <> show (discoveryDirs cfg) <> ")"
  -- Discovery phase: enumerate unpacked thunks across every configured
  -- discovery dir, then classify by plugin use.
  thunks       <- concat <$> mapM discoverUnpackedThunks (discoveryDirs cfg)
  pluginThunks <- filterM thunkUsesPlugins thunks
  let hotThunks     = filter (`notElem` pluginThunks) thunks
      restartThunks = pluginThunks

  info $ "Discovered " <> show (length thunks)
         <> " unpacked thunks ("
         <> show (length hotThunks)     <> " watched by Cabal's multi-repl, "
         <> show (length restartThunks) <> " via --restart due to plugins)."

  -- Start with compiler diagnostics. Browser artifacts are built separately.

  metadata <- concat <$> mapM thunkCabalFiles thunks

  -- Sideline ~/.ghci for the lifetime of the ghcid session.
  bracket_ sidelineUserGhci restoreUserGhci $ do
    scriptPath <- writeRunScript opts cfg metadata restartThunks
    info "Starting ghcid (multi-repl). Output in this terminal AND tee'd to:"
    info "  /tmp/jenga-ghcid.log"
    info "Edit a .hs file under configured discoveryDirs; ghcid reloads on save."
    info "Ctrl-C to exit."
    execProcess "/usr/bin/env" ["nix-shell", "shell.nix", "--run", "bash " <> scriptPath]

-- ---------------------------------------------------------------------------
-- Compiler loop
-- ---------------------------------------------------------------------------

-- | Write the full ghcid-mhu invocation to a bash script at
-- @/tmp/jenga-ghcid.sh@. Having the full invocation in a file
-- eliminates all shell-quoting layering between @jenga run@ and
-- ghcid: @nix-shell shell.nix --run 'bash /tmp/jenga-ghcid.sh'@
-- passes exactly one short arg to nix-shell, and the script's
-- contents are read verbatim by bash with zero re-quoting.
--
-- The script writes GHCi directives to a sidecar file
-- @/tmp/jenga-ghcid-ghci.cmd@ and references it via
-- @:cmd (readFile ...)@. Cabal owns module loading; the sidecar contains
-- only the configured application startup expression.
--
-- Output is tee'd to @/tmp/jenga-ghcid.log@ so anyone can
-- @tail -F@ it out-of-band.
writeRunScript
  :: RunOpts
  -> JengaRunConfig
  -> [FilePath]      -- ^ thunk package manifests
  -> [FilePath]      -- ^ restart-only thunks
  -> IO FilePath
writeRunScript opts cfg metadata restartThunks = do
  let scriptPath = "/tmp/jenga-ghcid.sh"
      ghciCmdPath = "/tmp/jenga-ghcid-ghci.cmd"
      logPath = "/tmp/jenga-ghcid.log"
      -- Multi-repl target: default to all:libs (every local library).
      -- CLI flag --target overrides.
      multiTarget = maybe "all:libs" id (runTarget opts)
      onReloadLine = maybe [] (pure . T.unpack) (onReload (backendPane cfg))
      -- Cabal loads each library in its own unit with its declared extensions.
      -- :add would create a separate interactive unit without those settings.
      ghciDirectives = unlines onReloadLine
      replCmd = unwords $
        [ cabalPath, "repl", "--enable-multi-repl", multiTarget
        , "-O0"
        , "--repl-options=-ignore-dot-ghci"
        ] <> devGhcOptions <> runExtraArgs opts
      restartArgs = unwords $
        [ "--restart=cabal.project"
        , "--restart=backend/backend.cabal"
        , "--restart=common/common.cabal"
        , "--restart=frontend/frontend.cabal"
        , "--restart=landing-page/landing-page.cabal"
        ] <> map ("--restart=" <>) metadata <> concatMap thunkRestartArgs restartThunks
      -- The script itself. All shell-sensitive strings become bash
      -- here-doc / shell-literal content, with ZERO Haskell-side
      -- quote-interpolation.
      script = unlines
        [ "#!/usr/bin/env bash"
        , "set -u"
        , "exec > >(tee -a " <> logPath <> ") 2>&1"
        , "echo '[jenga] ghcid log: " <> logPath <> "'"
        , "echo '[jenga] GHCi directives: " <> ghciCmdPath <> "'"
        , "cat > " <> ghciCmdPath <> " <<'JENGA_GHCI_EOF'"
        , ghciDirectives
        , "JENGA_GHCI_EOF"
        , "exec " <> ghcidPath
            <> " --command=" <> shQuote replCmd
            <> " --warnings --poll"
            <> " --outputfile=/tmp/jenga-ghcid-diagnostics.txt"
            <> " --run=" <> shQuote (":cmd (readFile " <> show ghciCmdPath <> ")")
            <> " " <> restartArgs
        ]
  writeFile scriptPath script
  setFileMode scriptPath 0o755
  pure scriptPath


-- | Package manifests change the REPL plan and therefore require a restart.
thunkCabalFiles :: FilePath -> IO [FilePath]
thunkCabalFiles dir = do
  entries <- listDirectory dir
  pure [dir </> entry | entry <- entries, takeExtension entry == ".cabal"]

-- | For a plugin-thunk, generate @--restart@ entries covering its
-- source and cabal metadata so ghcid auto-restarts on edits.
thunkRestartArgs :: FilePath -> [String]
thunkRestartArgs thunkDir =
  [ "--restart=" <> thunkDir </> "src"
  , "--restart=" <> thunkDir
  ]

-- ---------------------------------------------------------------------------
-- Thunk discovery
-- ---------------------------------------------------------------------------

-- | A thunk is unpacked if its directory contains a .cabal file. The
-- packed form is a bare {github.json, thunk.nix, default.nix} triple.
discoverUnpackedThunks :: FilePath -> IO [FilePath]
discoverUnpackedThunks root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      candidates <- mapM (\e -> pure (root </> e)) entries
      dirs       <- filterM doesDirectoryExist candidates
      filterM isUnpackedThunk dirs

-- | True if @dir@ contains any @.cabal@ file.
isUnpackedThunk :: FilePath -> IO Bool
isUnpackedThunk dir = do
  entries <- listDirectory dir
  pure $ any ((== ".cabal") . takeExtension) entries

-- | True if any @.cabal@ file in the thunk references @-fplugin=@.
-- Rough heuristic: grep the file for the substring.
thunkUsesPlugins :: FilePath -> IO Bool
thunkUsesPlugins dir = do
  entries <- listDirectory dir
  let cabalFiles = filter ((== ".cabal") . takeExtension) entries
  anyM (\f -> fileContains "-fplugin=" (dir </> f)) cabalFiles
  where
    anyM _ [] = pure False
    anyM p (x:xs) = do
      r <- p x
      if r then pure True else anyM p xs

fileContains :: String -> FilePath -> IO Bool
fileContains needle path = do
  exists <- doesFileExist path
  if not exists
    then pure False
    else do
      contents <- readFile path
      pure $ needle `isInfixOf` contents

-- ---------------------------------------------------------------------------
-- ~/.ghci sideline
-- ---------------------------------------------------------------------------

parkedGhciSuffix :: String
parkedGhciSuffix = ".jenga-run-parked"

sidelineUserGhci :: IO ()
sidelineUserGhci = do
  home <- getHomeDirectory
  let ghci   = home </> ".ghci"
      parked = home </> (".ghci" <> parkedGhciSuffix)
  present <- doesPathExist ghci
  when present $ do
    renamePath ghci parked
    info "Sidelined ~/.ghci (prevents cabal repl -package noise)."

restoreUserGhci :: IO ()
restoreUserGhci = do
  home <- getHomeDirectory
  let ghci   = home </> ".ghci"
      parked = home </> (".ghci" <> parkedGhciSuffix)
  present <- doesPathExist parked
  when present $ do
    renamePath parked ghci
    info "Restored ~/.ghci."

-- ---------------------------------------------------------------------------
-- Shell quoting (live — used by writeRunScript)
-- ---------------------------------------------------------------------------

-- | Shell-single-quote a string for safe inclusion in a bash -c payload.
shQuote :: String -> String
shQuote s = "'" <> concatMap escape s <> "'"
  where
    escape '\'' = "'\\''"
    escape c    = [c]
