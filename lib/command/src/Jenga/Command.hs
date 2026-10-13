module Jenga.Command (main) where

import Options.Applicative
import System.Environment (getArgs)

import qualified Jenga.Command.Build as Build
import qualified Jenga.Command.Deploy as Deploy
import qualified Jenga.Command.Gc as Gc
import qualified Jenga.Command.Hoogle as Hoogle
import qualified Jenga.Command.Init as Init
import qualified Jenga.Command.Repl as Repl
import qualified Jenga.Command.Run as Run
import qualified Jenga.Command.Thunk as Thunk

data ObCommand
  = Cmd_Init Init.InitOpts
  | Cmd_Run Run.RunOpts
  | Cmd_Repl [String]        -- raw args, parsed manually
  | Cmd_Watch Run.RunOpts
  | Cmd_Hoogle Hoogle.HoogleOpts
  | Cmd_Deploy Deploy.DeployCommand
  | Cmd_Thunk [String]       -- raw args, delegated to nix-thunk
  | Cmd_Gc Gc.GcCommand
  | Cmd_Build Build.BuildOpts

commandParser :: Parser ObCommand
commandParser = subparser
  ( command "init"    (mkInfo "Create a new Jenga project"
      (Cmd_Init <$> Init.initParser))
  <> command "run"    (mkInfo "Development server with sub-second reloads"
      (Cmd_Run <$> Run.runParser))
  <> command "watch"  (mkInfo "Alias for 'jenga run'"
      (Cmd_Watch <$> Run.runParser))
  <> command "repl"   (mkInfo "GHCi REPL with optimizations disabled"
      (pure (Cmd_Repl [])))
  <> command "hoogle" (mkInfo "Local Hoogle documentation server"
      (Cmd_Hoogle <$> Hoogle.hoogleParser))
  <> command "deploy" (mkInfo "Initialize and push NixOS deployments"
      (Cmd_Deploy <$> Deploy.deployParser))
  <> command "thunk"  (mkInfo "Manage nix thunks (delegates to nix-thunk)"
      (pure (Cmd_Thunk [])))
  <> command "gc"     (mkInfo "Explicit per-derivation nix garbage collection"
      (Cmd_Gc <$> Gc.gcParser))
  <> command "build"  (mkInfo "cabal build <target> (default: backend) with dev flags"
      (Cmd_Build <$> Build.buildParser))
  )
  where
    mkInfo desc p = info (helper <*> p) (progDesc desc)

opts :: ParserInfo ObCommand
opts = info (helper <*> commandParser)
  ( fullDesc
  <> header "jenga - Jenga development and deployment CLI"
  <> progDesc "Build, develop, and deploy Jenga applications"
  )

main :: IO ()
main = do
  allArgs <- getArgs
  -- repl and thunk need raw args (repl splits at --, thunk delegates everything).
  -- We parse just enough to identify the subcommand, then pass raw args through.
  case allArgs of
    ("repl" : rest)  -> Repl.run rest
    ("thunk" : rest) -> Thunk.run rest
    _ -> do
      cmd <- execParser opts
      case cmd of
        Cmd_Init o    -> Init.run o
        Cmd_Run o     -> Run.run o
        Cmd_Watch o   -> Run.run o
        Cmd_Hoogle o  -> Hoogle.run o
        Cmd_Deploy c  -> Deploy.run c
        Cmd_Gc c      -> Gc.run c
        Cmd_Build o   -> Build.run o
        Cmd_Repl _    -> Repl.run []    -- fallback if somehow reached
        Cmd_Thunk _   -> Thunk.run []   -- fallback if somehow reached
