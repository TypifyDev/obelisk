module Jenga.Command.Run
  ( RunOpts (..)
  , runParser
  , run
  ) where

import Options.Applicative hiding (info)
import System.Which (staticWhich)

import Jenga.Command.Utils (devCrossCabalArgs, devGhcOptions, execProcess, info, runProcessWithEnv)

ghcidPath, cabalPath :: FilePath
ghcidPath = $(staticWhich "ghcid")
cabalPath = $(staticWhich "cabal")

data RunOpts = RunOpts
  { runReplOnly :: Bool
  , runTarget :: Maybe String
  , runExtraArgs :: [String]
  }

runParser :: Parser RunOpts
runParser = RunOpts
  <$> switch (long "repl-only" <> help "Skip the initial cabal build and drop directly into ghcid — ghcid surfaces errors even on red transitive deps, iterating on a specific target with sub-second reloads")
  <*> optional (strOption (long "target" <> metavar "TARGET" <> help "cabal repl target (default: lib:backend). Useful with --repl-only for iterating on a specific transitive dep."))
  <*> many (strArgument (metavar "CABAL_ARGS..." <> help "Extra arguments passed to cabal repl"))

run :: RunOpts -> IO ()
run opts = do
  -- Step 1: Full cabal build (skipped if --repl-only). Triggers WASM via
  -- frontend-wasm Setup.hs; without it, frontend assets stay stale from the
  -- last successful jenga run.
  if runReplOnly opts
    then info "--repl-only: skipping initial cabal build, going straight to ghcid"
    else do
      info "Building backend (including WASM frontend)..."
      let buildArgs = ["build", "backend", "-O0"] <> devGhcOptions <> runExtraArgs opts
      runProcessWithEnv cabalPath buildArgs [("JENGA_CROSS_CABAL_ARGS", devCrossCabalArgs)]

  -- Step 2: ghcid for sub-second reloads
  -- Uses single-unit repl (lib:backend only) because GHC 9.14's multi-unit
  -- GHCi panics with "LinkInMemory" when using -unit flags.
  -- Backend changes get sub-second interpreted reloads.
  -- Common/frontend/landing-page source changes trigger a full session restart
  -- via --restart (slower, but automatic and correct).
  info "Starting ghcid (sub-second reloads on save)..."
  info "Ctrl+C to stop. Re-run jenga run to rebuild WASM after frontend changes."
  let target = maybe "lib:backend" id (runTarget opts)
      replCmd = unwords $
        [cabalPath, "repl", target
        , "-O0", "-f", "-cross"
        ] <> devGhcOptions <> runExtraArgs opts
  execProcess ghcidPath
    [ "--command=" <> replCmd
    , "--warnings"
    , "--poll"
    , "--run=:cmd return \":add *src/DevMain.hs\\nDevMain.devMain\""
    , "--restart=backend/backend.cabal"
    , "--restart=common/common.cabal"
    , "--restart=frontend/frontend.cabal"
    , "--restart=landing-page/landing-page.cabal"
    , "--restart=cabal.project"
    , "--restart=common/src"
    , "--restart=frontend/src"
    , "--restart=landing-page/src"
    ]
