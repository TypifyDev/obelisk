{-# LANGUAGE OverloadedStrings #-}
module Test.Util.ChromeFlags where

import Data.Text
import System.IO.Temp
import System.Info (os)

withSandboxedChromeFlags :: Bool -> ([Text] -> IO a) -> IO a
withSandboxedChromeFlags headless action =
  withSystemTempDirectory "reflex-dom-core_test" $ \tmp ->
    -- ChromeDriver owns a fresh profile for each Darwin session. Sharing one
    -- directory across Hspec sessions causes Chrome's SingletonLock conflict.
    action $
      [ if headless then "--headless" else "--auto-open-devtools-for-tabs"
      , "--disable-gpu"
      , "--no-sandbox"
      , "--disable-extensions"
      , "--disable-default-apps"
      , if os == "darwin" then "--remote-debugging-port=0" else "--remote-debugging-port=9222"
      ] <> if os == "darwin" then [] else ["--user-data-dir=" <> pack tmp]
