module Jenga.Command.Repl
  ( run
  ) where

import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Env (setEnv)
import System.Which (staticWhich)

import Jenga.Command.Cabal (withCabalProject)
import Jenga.Command.Utils (devCrossCabalArgs, devGhcOptions, runProcessWithEnv)

cabalPath :: FilePath
cabalPath = $(staticWhich "cabal")

-- | jenga repl parses its own args manually (targets before --, extra args after).
run :: [String] -> IO ()
run rawArgs
  | "--help" `elem` rawArgs || "-h" `elem` rawArgs = do
      hPutStrLn stderr $ unlines
        [ "Usage: jenga repl [TARGETS...] [-- CABAL_ARGS...]"
        , ""
        , "  Starts 'cabal repl' with optimizations disabled and cross builds skipped."
        , "  TARGETS defaults to 'lib:backend lib:common lib:frontend'. Examples:"
        , "    jenga repl                        # repl for backend, common, and frontend"
        , "    jenga repl lib:common             # repl for common library only"
        , "    jenga repl lib:backend -- -v      # repl for backend with verbose cabal output"
        ]
      exitSuccess
  | otherwise = do
      let (targets, extraArgs) = splitAtDashDash rawArgs
          finalTargets = if null targets
            then ["lib:backend", "lib:common", "lib:frontend"]
            else targets
      setEnv "JENGA_CROSS_CABAL_ARGS" devCrossCabalArgs True
      withCabalProject $ \projectArgs ->
        runProcessWithEnv cabalPath
          (["repl"] <> finalTargets <> ["-O0", "-f", "-cross"]
            <> projectArgs <> devGhcOptions <> extraArgs) []

splitAtDashDash :: [String] -> ([String], [String])
splitAtDashDash = go []
  where
    go acc [] = (reverse acc, [])
    go acc ("--" : rest) = (reverse acc, rest)
    go acc (x : rest) = go (x : acc) rest
