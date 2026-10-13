{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Link-only smoke stub. Widget-body calls broke when the widget monad
-- became @WithJSContextSingleton x (Eff es)@ (task #209 rename cascade);
-- our port widgets still return @Eff es a@ so they need lifting.
-- Stubbed pending the lift cascade or full widget-monad conversion.
module Main (main) where

import qualified Reflex.Effectful.DomHost as DH ()

_smokeType :: ()
_smokeType = ()

main :: IO ()
main = putStrLn "widget-input smoke stubbed pending task #209 lift cascade."
