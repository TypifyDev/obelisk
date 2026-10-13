{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Link-only smoke: verify that variant B's MTL-shape widget functions
-- (from @Reflex.Dom.Widget.Basic@, @.Widget.Input@, etc.) can be
-- called directly on @Eff es@ via the MTL instances landed in
-- 'Reflex.Effectful.DomHost' (task #207).
--
-- Passing this test = the win from writing MTL instances actually
-- works: variant B's MTL widget code becomes callable on our Effect
-- rows with ZERO re-implementation.
module Main (main) where

import qualified Reflex.Effectful.DomHost      as DH
import qualified Reflex.Dom.Builder.Class      as RDBC
import qualified Reflex.Dom.Widget.Basic       as RDWB
import qualified Data.Text                     as Text
import qualified Data.Map                      as Data.Map
import           Data.Default                  (def)

-- Call variant B's RDBC.element directly on our ImmediateWidget row.
-- If this typechecks + links, the MTL DomBuilder instance is doing its job.
_smokeElementViaMtl :: forall x es. DH.ImmediateWidget x es
_smokeElementViaMtl = do
  _ <- RDBC.element "div" def (pure ())
  pure ()

-- Variant B's RDWB.text (from Reflex.Dom.Builder.Class).
_smokeTextViaMtl :: forall x es. DH.ImmediateWidget x es
_smokeTextViaMtl = do
  RDWB.text "Hello from variant B's MTL text, running on Eff es!"
  pure ()

-- Variant B's Reflex.Dom.Widget.Basic.el.
_smokeElViaWidgetBasic :: forall x es. DH.ImmediateWidget x es
_smokeElViaWidgetBasic = do
  RDWB.el "p" (RDWB.text "Widget/Basic el called on Eff es")

-- Variant B's Reflex.Dom.Widget.Basic.elAttr.
_smokeElAttrViaWidgetBasic :: forall x es. DH.ImmediateWidget x es
_smokeElAttrViaWidgetBasic = do
  RDWB.elAttr "a"
    (Data.Map.singleton (Text.pack "href") (Text.pack "https://example.com"))
    (RDWB.text "Widget/Basic elAttr from variant B")

-- NOT tested here (blocked by nominal-type mismatch):
-- Widget/Input.textInput, Widget/Resize.resizeDetector, Xhr.*, WebSocket.*,
-- Location.*. All require variant B's GhcjsDomSpace (in
-- Reflex.Dom.Builder.Immediate) but our DomBuilder Effect GADT
-- parameterises on our own inlined GhcjsDomSpace — nominally distinct.
-- Unifying them requires the "extract shared space types" wave
-- (task followup: import GhcjsDomSpace / HydrationDomSpace / StaticDomSpace
-- from variant B's Immediate.hs + Static.hs, delete our inlined versions).

main :: IO ()
main = putStrLn "MTL-bridge smoke: variant B's widgets compile + link against Eff es via MTL instances."
