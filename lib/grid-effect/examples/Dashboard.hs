{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Example: Type-safe grid layout for a dashboard using
--  grid-effect + reflex-dom-effectful + Classh.
--
--  CURRENT (no budget tracking — silently overflows):
--
-- @
--  gridCol Col12 $ do
--    col [7] $ sidebar    -- 7 cols
--    col [8] $ mainPanel  -- 8 cols (7+8=15 > 12, no error!)
-- @
--
--  WITH grid-effect (compile-time overflow detection):
--
-- @
--  runGridEff_exact $ G.do
--    G.consume \@(N 7) sidebar
--      G..>> G.consume \@(N 8) mainPanel
--  -- COMPILE ERROR: Couldn't solve: (8 * 1) <= (1 * 5)
--  --   meaning: 8 > 5 remaining columns after consuming 7
-- @
--
--  The fix:
--
-- @
--  runGridEff_exact $ G.do
--    G.consume \@(N 4) sidebar
--      G..>> G.consume \@(N 8) mainPanel  -- 4+8=12 ✓
-- @
module Dashboard where

import Prelude hiding ((>>=), (>>), return)
import qualified Grid.Effect.Eff as G
import Grid.Effect.Rational

import Effectful (Eff, (:>))
import Reflex (Reflex, Event, Dynamic)

-- These would come from reflex-dom-effectful in a real app:
-- import Reflex.Effectful
-- import Reflex.Class (WidgetEff')

-- And these from Classh / reflex-classh:
-- import Classh
-- import Classh.Reflex.Layout (gridCol, col)
-- import Classh.Box.TWSize.Budget () -- orphan ReifiableBudget instance

-- ─── Dashboard layout (12 columns, compile-time tracked) ─────

-- | Full dashboard: header, sidebar+main, footer.
-- Budget: exactly 12 consumed per row. Proven at compile time.
--
-- The @WidgetEff' es t@ constraint gives access to all reflex-dom-effectful
-- effects (Dom, Hold, PerformEvent, etc.) inside each grid cell.
--
-- In a real app with Classh, each @consume@ would emit a
-- @col-span-N@ CSS class via @ReifiableBudget TWSizeOrFraction@.

{-
dashboard :: WidgetEff' es t => G.GridEff (N 12) (N 12) (N 0) es ()
dashboard = G.do
  -- Row 1: full-width header
  G.consume @(N 12) header

  -- Row 2: sidebar (4) + main content (8)
  G.consume @(N 4) sidebar G..>> G.consume @(N 8) mainContent

  -- Row 3: three equal cards
  G.consume @(N 4) card1 G..>> G.consume @(N 4) card2 G..>> G.consume @(N 4) card3

  -- Row 4: full-width footer
  G.consume @(N 12) footer
-}

-- ─── With Classh styling (what the actual code looks like) ───

{-
-- Each cell gets type-safe Tailwind classes from the budget:
dashboardClassh :: WidgetEff' es t => G.GridEff (N 12) (N 12) (N 0) es ()
dashboardClassh = G.do
  -- Row 1: header spanning all 12 columns
  G.consume @(N 12) $ G.liftEff $
    elClass "header" $(classh'
      [ bgColor .~~ solidColor (Gray C900)
      , p .~~ TWSize 4
      , colSpan .~~ Col12
      ]) $ do
      textS $(classh' [text_size .~~ XL2, text_weight .~~ Bold]) "Dashboard"

  -- Row 2: sidebar + main
  G.consume @(N 4) $ G.liftEff $
    elClass "aside" $(classh'
      [ bgColor .~~ solidColor (Gray C800)
      , p .~~ TWSize 4
      , br .~~ R_Lg
      , colSpan .~~ Col4
      ]) $ do
      el "h2" $ text "Navigation"
      el "ul" $ do
        el "li" $ text "Home"
        el "li" $ text "Analytics"
        el "li" $ text "Settings"

    G..>> G.consume @(N 8) $ G.liftEff $
      elClass "main" $(classh'
        [ bgColor .~~ solidColor (Gray C800)
        , p .~~ TWSize 6
        , br .~~ R_Lg
        , colSpan .~~ Col8
        ]) $ do
        el "h1" $ text "Welcome"
        el "p" $ text "Your dashboard content here"

        -- Effectful effects work inside grid cells:
        counterWidget

  -- Row 3: stats cards (nested grid inside each cell)
  G.consume @(N 4) (statsCard "Users" "1,247")
    G..>> G.consume @(N 4) (statsCard "Revenue" "$42K")
    G..>> G.consume @(N 4) (statsCard "Uptime" "99.9%")

  -- Row 4: footer
  G.consume @(N 12) $ G.liftEff $
    elClass "footer" $(classh' [py .~~ TWSize 4]) $
      textS $(classh' [text_color .~~ color (Gray C500)]) "© 2026 Jenga"

-- A card that uses effectful effects inside a budget-tracked cell
statsCard :: WidgetEff' es t => Text -> Text -> G.GridEff (N 4) (N 4) (N 4) es ()
statsCard label value = G.liftEff $ do
  elClass "div" $(classh'
    [ bgColor .~~ solidColor (Gray C800)
    , p .~~ TWSize 4
    , br .~~ R_Lg
    , shadow .~~ Shadow_Md
    ]) $ do
    textS $(classh' [text_color .~~ color (Gray C400), text_size .~~ SM]) label
    el "div" $
      textS $(classh' [text_size .~~ XL3, text_weight .~~ Bold]) value

-- Effectful counter inside a grid cell
counterWidget :: WidgetEff' es t => Eff es ()
counterWidget = do
  el "div" $ do
    (dec, _) <- el' "button" $ text "-"
    (inc, _) <- el' "button" $ text "+"
    val <- foldDyn (+) (0 :: Int) $ leftmost
      [ 1 <$ domEvent Click inc
      , (-1) <$ domEvent Click dec
      ]
    el "span" $ display val
-}

-- ─── Fractional layouts ──────────────────────────────────────

{-
-- Thirds layout — 1/3 + 2/3 = 1, proven at compile time
thirdsLayout :: WidgetEff' es t => G.GridEff (N 1) (N 1) '(0, 9) es ()
thirdsLayout =
  G.consume @(F 1 3) $ G.liftEff $
    -- Classh: colSpan renders as "w-1/3" via ReifiableBudget
    elClass "div" $(classh' [w .~~ TWFraction 1 D3]) $
      text "One third"

  G..>> G.consume @(F 2 3) $ G.liftEff $
    elClass "div" $(classh' [w .~~ TWFraction 2 D3]) $
      text "Two thirds"
-}

-- ─── What overflow looks like ────────────────────────────────

{-
-- UNCOMMENT THIS — it won't compile:
overflow :: G.GridEff (N 12) (N 12) (N 0) es ()
overflow =
  G.consume @(N 7) (G.liftEff $ text "sidebar")
    G..>> G.consume @(N 8) (G.liftEff $ text "main")
-- ERROR:
--   • Couldn't solve: (8 * 1) <= (1 * 5)
--     arising from a use of 'G.consume'
--   Translation: 8 columns requested but only 5 remain (12 - 7 = 5)
-}
