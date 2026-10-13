{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Type-level smoke test for 'Reflex.Effectful.DomHost' — verifies the
-- public 'mainWidget' + 'el' + 'text' surface composes and links.
--
-- Not intended to be executed in this test-runner (that requires an
-- open browser + jsaddle-warp roundtrip). Passing = the smoke widget
-- typechecks + links against the current library.
module Main (main) where

import qualified Reflex.Effectful.DomHost as DH
import           System.Environment       (lookupEnv)

-- | The canonical smoke widget: build a @<div>@ containing "hello".
-- 'ImmediateWidget x es' is parameterized on the JS-context tag @x@ and
-- the effect row @es@ and unfolds to @WidgetEff t x es ()@; the forall
-- lives on the value-level signature so this can be passed to
-- 'mainWidget' (which expects rank-N). We use 'DH.el' / 'DH.text' — the
-- DomHost-provided helpers that operate directly on 'WidgetEff t x es'
-- under the 'ImmediateWidget' constraint alias.
smokeWidget :: forall x es. DH.ImmediateWidget x es
smokeWidget = DH.el "div" (DH.text "hello")

-- | Build the widget composition all the way up to but not including
-- 'mainWidget' — this is what 'mainWidget smokeWidget' would run.
-- Referencing @DH.mainWidget smokeWidget@ at the value level proves the
-- rank-N ImmediateWidget → IO () shape composes.
smokeMain :: IO ()
smokeMain = DH.mainWidget smokeWidget

main :: IO ()
main = do
  -- Only actually stand up jsaddle-warp if RUN_SMOKE_WIDGET is set —
  -- otherwise this test just proves link-time correctness (browsers
  -- aren't available in CI, and jsaddle-warp blocks on a port).
  wantRun <- lookupEnv "RUN_SMOKE_WIDGET"
  case wantRun of
    Just _  -> smokeMain
    Nothing -> pure ()
