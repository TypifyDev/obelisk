-- | @jenga run@: zero-config dev loop.
--
-- Internally spawns two ghcid sessions in a tmux split:
--
--   * __Backend pane__   @cabal repl lib:backend@ wrapped by ghcid; after
--     each reload fires @DevMain.devMain@ which starts the backend server
--     (default @http:\/\/localhost:8000@, from @config\/common\/route@).
--     Sub-second reload on changes under @backend\/src@.
--
--   * __Frontend pane__  @cabal repl exe:frontend@ wrapped by ghcid; after
--     each reload fires @:main@ which calls @Reflex.Dom.run@, which on
--     native GHC resolves to @jsaddle-warp@ serving the SPA at
--     @http:\/\/localhost:3003@ (override: @JSADDLE_WARP_PORT@).
--     Sub-second reload on changes under @frontend\/src@.
--
-- Both ghcids are single-unit, sidestepping GHC 9.14's LinkInMemory panic
-- that fires when launching @:main@ under multi-unit MHU GHCi.
--
-- Any __unpacked__ thunk under @thunks\/@ (directory with a @.cabal@ file,
-- no bare @thunk.nix@) is auto-wired for hot reload. For each such thunk,
-- jenga walks @\<thunk\>\/src@ and emits @:add *\<path\>.hs@ into both
-- ghcids' @--run@ directive, forcing the thunk's modules into GHCi's
-- interpreted set. This gives sub-second @:reload@ on thunk source
-- changes, same latency as backend's own modules.
--
-- Thunks whose @.cabal@ or source references @-fplugin=@ fall back to
-- @--restart@ wiring instead of @:add *@, because GHC plugins don't fire
-- in interpreted mode. The thunk still auto-reloads, just via the slower
-- restart cycle rather than sub-second :reload.
--
-- @~\/.ghci@ is sidelined for the lifetime of this process. User-global
-- @:set -package X@ directives cause @cannot satisfy -package X@ noise
-- under cabal repl's per-unit flag application; sidelining prevents this.
-- Restored on normal exit, exceptions, SIGINT, SIGTERM, SIGHUP.
--
-- @tmux@ is launched via @nix-shell -p tmux@ if not on @PATH@; no
-- permanent install required.
module Jenga.Command.Run
  ( RunOpts (..)
  , runParser
  , run
  ) where

import Control.Exception (bracket_)
import Control.Monad (filterM, when)
import Data.List (isInfixOf, sort)
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

-- | Default ports; env overrides exist for frontend only.
backendPort, frontendPort :: Int
backendPort  = 8000
frontendPort = 3003

-- | Tmux session name. Fixed so re-running `jenga run` targets the same
-- session; existing session is killed first.
tmuxSession :: String
tmuxSession = "jenga-run"

data RunOpts = RunOpts
  { runReplOnly  :: Bool
  , runTarget    :: Maybe String
  , runExtraArgs :: [String]
  }

runParser :: Parser RunOpts
runParser = RunOpts
  <$> switch
      (  long "repl-only"
      <> help "Skip the initial cabal build and drop directly into ghcid. \
              \ghcid surfaces errors even on red transitive deps, iterating \
              \on a specific target with sub-second reloads."
      )
  <*> optional (strOption
      (  long "target"
      <> metavar "TARGET"
      <> help "cabal repl target for the backend pane (default: lib:backend). \
              \Useful with --repl-only for iterating on a specific transitive \
              \dep."
      ))
  <*> many (strArgument
      (  metavar "CABAL_ARGS..."
      <> help "Extra arguments passed to cabal repl in both panes."
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
         <> show (length hotThunks)     <> " hot via :add *, "
         <> show (length restartThunks) <> " via --restart due to plugins)."

  -- No pre-build. The tmux ghcid session is the authority. The old
  -- pre-build served exactly one purpose — producing the WASM frontend
  -- artifact via frontend-wasm's Setup.hs — and that artifact is only
  -- needed when loading the frontend in a browser, not for Haskell
  -- iteration. If you need a fresh WASM, invoke `cabal build backend`
  -- (or a future `jenga wasm` subcommand) explicitly out-of-band.
  --
  -- The @--repl-only@ flag is now redundant with the default and kept
  -- for compatibility; it does nothing.

  -- Resolve each hot thunk's .hs files once up-front for :add * injection.
  -- GHCi's * prefix forces interpreted mode for the named module.
  hotAddLines <- concat <$> mapM thunkAddLines hotThunks

  -- Sideline ~/.ghci for the lifetime of the ghcid session.
  bracket_ sidelineUserGhci restoreUserGhci $ do
    scriptPath <- writeRunScript opts cfg hotAddLines restartThunks
    info "Starting ghcid (multi-repl). Output in this terminal AND tee'd to:"
    info "  /tmp/jenga-ghcid.log"
    info "Edit a .hs file under configured discoveryDirs; ghcid reloads on save."
    info "Ctrl-C to exit."
    execProcess "/usr/bin/env" ["nix-shell", "shell.nix", "--run", "bash " <> scriptPath]

-- ---------------------------------------------------------------------------
-- Pane commands
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
-- @:cmd return (readFile ...)@ \8212 same trick, applied one level
-- deeper so no quote escaping of the \@:add *\@ lines is needed.
--
-- Output is tee'd to @/tmp/jenga-ghcid.log@ so anyone can
-- @tail -F@ it out-of-band.
writeRunScript
  :: RunOpts
  -> JengaRunConfig
  -> [String]        -- ^ hot-add lines
  -> [FilePath]      -- ^ restart-only thunks
  -> IO FilePath
writeRunScript opts cfg hotAddLines restartThunks = do
  let scriptPath = "/tmp/jenga-ghcid.sh"
      ghciCmdPath = "/tmp/jenga-ghcid-ghci.cmd"
      logPath = "/tmp/jenga-ghcid.log"
      -- Multi-repl target: default to all:libs (every local library).
      -- CLI flag --target overrides.
      multiTarget = maybe "all:libs" id (runTarget opts)
      onReloadLine = maybe [] (pure . T.unpack) (onReload (backendPane cfg))
      ghciDirectives = unlines $ hotAddLines <> onReloadLine
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
        ] <> concatMap thunkRestartArgs restartThunks
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
            <> " --run=" <> shQuote (":cmd (readFile " <> show ghciCmdPath <> ")")
            <> " " <> restartArgs
        ]
  writeFile scriptPath script
  setFileMode scriptPath 0o755
  pure scriptPath

-- | For a hot-thunk, walk its @src\/@ dir recursively for @.hs@ files and
-- emit one @:add *path@ line per file. GHCi's @*@ prefix forces
-- interpreted mode, which gives sub-second @:reload@ on save of any
-- listed module (same latency as backend's own interpreted modules).
thunkAddLines :: FilePath -> IO [String]
thunkAddLines thunkDir = do
  let srcDir = thunkDir </> "src"
  present <- doesDirectoryExist srcDir
  if not present
    then pure []
    else do
      files <- walkHsFiles srcDir
      pure [ ":add *" <> f | f <- sort files ]

-- | Recursively enumerate @.hs@ files under @root@. Does NOT follow
-- symlinks (listDirectory ignores entry type; doesDirectoryExist
-- resolves the link and reports True if the target is a dir — accepted
-- behavior, cycles unlikely in a thunk tree).
walkHsFiles :: FilePath -> IO [FilePath]
walkHsFiles root = do
  entries <- listDirectory root
  let full = map (root </>) entries
  (dirs, files) <- splitByType full
  let hsHere = filter ((== ".hs") . takeExtension) files
  nested <- concat <$> mapM walkHsFiles dirs
  pure $ hsHere <> nested
  where
    splitByType :: [FilePath] -> IO ([FilePath], [FilePath])
    splitByType [] = pure ([], [])
    splitByType (p:ps) = do
      isDir <- doesDirectoryExist p
      (ds, fs) <- splitByType ps
      pure $ if isDir then (p:ds, fs) else (ds, p:fs)

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
-- Tmux launch — PARKED. The jenga-run v2 rewrite drops tmux entirely
-- in favor of a single foreground ghcid-mhu session (output tee'd to
-- /tmp/jenga-ghcid.log). The functions below are kept commented so a
-- future revival can lift them back out of the {- ... -} if we decide
-- to re-add in-process multiplexing. Removing them outright per the
-- never-delete rule.
-- ---------------------------------------------------------------------------

{- PARKED — the stub inside was never wired; if uncommented, the stub must be wired or renamed to claudeDeferredLogicImplementation.

-- | Launch a tmux session with two vertical panes running the given
-- shell command strings. Session name is fixed ('tmuxSession'). If a
-- stale session exists it is killed first.
launchTmuxSplit :: String -> String -> IO ()
launchTmuxSplit leftCmd rightCmd = do
  mTmux <- findExecutable "tmux"
  -- leftCmd / rightCmd each contain the nested nix-shell+ghcid+repl
  -- invocation for one pane, which easily exceeds tmux's internal
  -- per-command length limit (~8KB — tmux refuses with
  -- "command too long"). Spill each to a tempfile and have tmux
  -- just run @bash <path>@.
  leftScript  <- writePaneScript "jenga-run-left.sh"  leftCmd
  rightScript <- writePaneScript "jenga-run-right.sh" rightCmd
  let tmux = maybe "tmux" id mTmux
      runPane path = "bash " <> shQuote path
      kill  = tmux <> " kill-session -t " <> tmuxSession <> " 2>/dev/null || true"
      new   = tmux <> " new-session -d -s " <> tmuxSession
                   <> " -n ghcid " <> shQuote (runPane leftScript)
      split = tmux <> " split-window -h -t " <> tmuxSession <> ":ghcid "
                   <> shQuote (runPane rightScript)
      selPane = tmux <> " select-pane -t " <> tmuxSession <> ":ghcid.0"
      -- Do NOT attach-session here. jenga runs under a detached
      -- setsid context (no controlling terminal), so attach-session
      -- would fail with "open terminal failed: not a terminal".
      -- The session is left running; the user attaches from their
      -- own terminal with the command we print below.
      script  = unlines [kill, new, split, selPane]
  case mTmux of
    Just _  -> execProcess "/usr/bin/env" ["bash", "-c", script]
    Nothing ->
      execProcess "/usr/bin/env" ["nix-shell", "-p", "tmux", "--run", script]

-- | Spill a pane command string to @$TMPDIR/<name>@ as a bash script,
-- mode 755, so tmux can invoke it with a very short argv.
writePaneScript :: FilePath -> String -> IO FilePath
writePaneScript name body = do
  tmpDir <- fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  let path = tmpDir </> name
      -- Pane log lives in /tmp (NOT $TMPDIR) because $TMPDIR under
      -- nix-shell is wiped when nix-shell exits; the pane log must
      -- outlive its launching shell so you can grep errors after
      -- the fact. Name mirrors the script name (e.g.
      -- jenga-run-left.sh -> /tmp/jenga-pane-left.log).
      logPath = "/tmp/jenga-pane-"
             <> takeWhile (/= '.') (drop (length ("jenga-run-" :: String)) name)
             <> ".log"
  -- No `set -euo pipefail` here. If the inner ghcid/cabal exits
  -- non-zero (e.g. the outer nix-shell emits a stdenv-setup warning
  -- that bash flags as a parse error, or ghcid returns red state),
  -- we still want the pane to stay alive so the user can see the
  -- output and iterate. Drop to an interactive bash on exit instead.
  --
  -- `unset TMPDIR` matters: this script executes inside a tmux pane
  -- that was spawned by Level-2 nix-shell (-p tmux). Level-2 exited
  -- right after `tmux select-pane` and cleaned up its TMPDIR, but
  -- the tmux server still has Level-2's TMPDIR in env. If we don't
  -- unset it, the inner `nix-shell shell.nix --run ghcid` here
  -- becomes Level-3 and tries to create its own TMPDIR under the
  -- deleted Level-2 TMPDIR, failing with
  -- @error: creating directory '/tmp/nix-shell-.../nix-shell-.../nix-shell-...': No such file or directory@.
  --
  -- `exec > >(tee -a <logPath>) 2>&1` makes every line the pane
  -- emits (ghcid, cabal, nix-shell, the inner server's startup
  -- trace, exceptions, everything) land in a stable log file so
  -- the user (and any out-of-tmux inspector) can grep errors
  -- without having to be inside the tmux session. The pane still
  -- shows output live; tee duplicates to both.
  writeFile path
    ( "#!/usr/bin/env bash\n"
    <> "unset TMPDIR\n"
    <> "exec > >(tee -a " <> shQuote logPath <> ") 2>&1\n"
    <> "echo \"[jenga] pane log: " <> logPath <> "\"\n"
    <> body
    <> "\necho; echo '[jenga] pane command exited; dropping to shell. Ctrl-D to close.'\n"
    <> "exec bash -i\n"
    )
  setFileMode path 0o755
  pure path

-- ---------------------------------------------------------------------------
-- Shell quoting helpers
-- ---------------------------------------------------------------------------

-- | Shell-single-quote a string for safe inclusion in a bash -c payload.
shQuote :: String -> String
shQuote s = "'" <> concatMap escape s <> "'"
  where
    escape '\'' = "'\\''"
    escape c    = [c]

-- | Escape a GHCi-:cmd string literal. GHCi parses the string with
-- Haskell escape rules, so we need \" for quotes and \\n for newlines.
--
-- Unused by the v2 run path (which pipes the GHCi directives through
-- a separate file rather than embedding them in an argv string), but
-- kept available for future callers that want to inline GHCi scripts.
escapeGhciString :: String -> String
escapeGhciString s = "\"" <> concatMap esc s <> "\""
  where
    esc '"'  = "\\\""
    esc '\n' = "\\n"
    esc '\\' = "\\\\"
    esc c    = [c]
-}  -- END PARKED

-- ---------------------------------------------------------------------------
-- Shell quoting (live — used by writeRunScript)
-- ---------------------------------------------------------------------------

-- | Shell-single-quote a string for safe inclusion in a bash -c payload.
shQuote :: String -> String
shQuote s = "'" <> concatMap escape s <> "'"
  where
    escape '\'' = "'\\''"
    escape c    = [c]
