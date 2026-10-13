{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Type-level smoke test for the AdjustableE-based composers in
-- 'Reflex.Effectful.DomHost' (Phases 2-4). Verifies that
-- 'mainWidgetAE' + 'WidgetRowE' compose and link.
--
-- Not intended to be executed in this test-runner (that requires an
-- open browser + jsaddle-warp roundtrip). Passing = the smoke widget
-- typechecks + links against the current library, which proves:
--
--   * 'GuestEff t x' is a valid effect row
--   * 'WidgetRowE t x' contains the required host scaffolding
--     (Reader HydrationEnv, JSM', AdjustableE (GuestEff t x) t, Base)
--   * 'attachImmediateWidgetAE' — the low-level composer using
--     'hostPerformEventTAE' + 'runAdjustableEStandalone' — links.
--   * 'attachWidgetAE'' — discharges HydrationEnv + JSM' scaffolding
--     and delegates to 'attachImmediateWidgetAE'.
--   * 'mainWidgetAE' — top-level entry, mounts widget in <body>.
--
-- Widget content is trivial ('pure ()') because 'WidgetRowE' does not
-- yet include DomBuilder/HasDocument (deferred to Phase 5 — those
-- interpreters depend on OLD 'WidgetE' bundle including OLD Adjustable,
-- which requires a rework to route via AdjustableE-based row).
module Main (main) where

import           Effectful                (Eff)
import qualified Reflex.Effectful.DomHost as DH
import qualified Reflex.Spider            as RS
import qualified Reflex.Spider.Internal   as RSI
import           System.Environment       (lookupEnv)

-- | The AE-composer smoke widget: does nothing (yet).
-- Widget row is 'WidgetRowE (SpiderTimeline Global) Global':
--   Reader HydrationEnv : JSM' : AdjustableE (GuestEff ...) t : Base
smokeWidgetAE
  :: Eff (DH.WidgetRowE (RS.SpiderTimeline RSI.Global) RSI.Global) ()
smokeWidgetAE = pure ()

-- | Compose all the way up to 'mainWidgetAE' — proves the rank-N shape
-- required by 'mainWidgetAE' is satisfied.
smokeMainAE :: IO ()
smokeMainAE = DH.mainWidgetAE smokeWidgetAE

main :: IO ()
main = do
  -- Only actually stand up jsaddle-warp if RUN_SMOKE_WIDGET_AE is set —
  -- otherwise this test just proves link-time correctness. jsaddle-warp
  -- blocks on a port, incompatible with CI.
  wantRun <- lookupEnv "RUN_SMOKE_WIDGET_AE"
  case wantRun of
    Just _  -> smokeMainAE
    Nothing -> pure ()
