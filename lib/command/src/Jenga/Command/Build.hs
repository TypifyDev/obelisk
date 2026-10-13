{-# LANGUAGE TemplateHaskell #-}

-- | @jenga build@: explicit on-demand build of a cabal target.
--
-- Thin wrapper around @cabal build \<target\>@ that:
--
--   * Runs inside the project's nix-shell (same env as other jenga
--     subcommands) so @cabal@ + GHC resolve correctly.
--   * Defaults the target to @backend@ when the user doesn't name one.
--   * Threads the project's dev GHC flags (@-O0 -fno-specialise ...@)
--     and @JENGA_CROSS_CABAL_ARGS@ env var — the latter is read by
--     the @Jenga.Setup.Frontend.Wasm@ hook to drive the WASM cross
--     cabal invocation, so @jenga build backend@ produces the WASM
--     artifact in the same way the removed pre-build step did.
--   * Forwards any extra arguments after the target through to cabal.
--
-- Fatal on error: this is the explicit \"please build it now\"
-- lever, so build failure propagates all the way out as an exit
-- code the caller can see.
module Jenga.Command.Build
  ( BuildOpts (..)
  , buildParser
  , run
  ) where

import Options.Applicative
import System.Which (staticWhich)

import qualified Jenga.Command.Utils as Utils
import Jenga.Command.Utils
  ( devCrossCabalArgs, devGhcOptions
  , runProcessWithEnv
  )

cabalPath :: FilePath
cabalPath = $(staticWhich "cabal")

-- | 'buildTarget' is anything cabal accepts as a build target:
-- a package name (@backend@, @common@), a component target
-- (@lib:common@, @exe:frontend@, @test:common-filters-tests@), or
-- the special @all@ (every package in @cabal.project@).
data BuildOpts = BuildOpts
  { buildTarget :: String
  , buildExtraArgs :: [String]
  }

buildParser :: Parser BuildOpts
buildParser = BuildOpts
  <$> strArgument
      (  metavar "TARGET"
      <> value "backend"
      <> showDefault
      <> help "cabal build target. Anything cabal accepts (package name, \
              \component like lib:common / exe:frontend, or 'all')."
      )
  <*> many (strArgument
      (  metavar "CABAL_ARGS..."
      <> help "Extra args forwarded to cabal after the target."
      ))

run :: BuildOpts -> IO ()
run opts = do
  let buildArgs = ["build", buildTarget opts, "-O0"]
                  <> devGhcOptions
                  <> buildExtraArgs opts
      envVars   = [("JENGA_CROSS_CABAL_ARGS", devCrossCabalArgs)]
  Utils.info $ "cabal " <> unwords buildArgs
  runProcessWithEnv cabalPath buildArgs envVars
