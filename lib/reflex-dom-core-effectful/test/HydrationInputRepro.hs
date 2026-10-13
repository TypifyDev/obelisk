{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- MINIMAL Effectful widget test that reproduces the failing behavior of
-- HydrationBrowser.hs's "captures user input after switchover" test —
-- but stripped of the full hspec-webdriver / session machinery. Just:
--
--   1. Spin up JSaddle-warp on a body element that has an <input>.
--   2. Call attachHydrationWidgetWithFailure to switchover.
--   3. WebDriver types "hello world" into the <input>.
--   4. WebDriver clicks the "save" <button>.
--   5. Report via putStrLn whether:
--         * the DOM's input.value is what we typed
--         * the FRP graph's _inputElement_input Event fired
--         * the FRP graph's _inputElement_value Dynamic updated
--         * an IORef (written by performEvent_) has the value
--
-- If (a) fires but the FRP-side observations are empty, the bug is on
-- the DOM-to-FRP boundary (Requester_ never propagates post-snapshot,
-- or mergeInt never sees the late add). If ALL are empty, the DOM wire
-- itself is broken. If FRP observes but IORef stays "", the bug is
-- somewhere down the observation chain instead.
--
-- Instrumentation printfs are ADDED to eff-reflex's HeadlessEffectful.hs
-- separately (see printf spec in the accompanying report). Together
-- they let us observe every load-bearing point in one run.
module Main (main) where

import Prelude hiding (fail)
import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Lens.Operators ((.~))
import Control.Monad hiding (fail)
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Function ((&))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Language.Javascript.JSaddle (syncPoint)
import Language.Javascript.JSaddle.Warp
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai
import Network.WebSockets
import Reflex.Dom.Core
import qualified Reflex.Effectful.DomHost as DH
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import System.Environment
import System.IO (stderr, hFlush, stdout)
import System.IO.Silently
import System.Process
import System.Which (staticWhich)
import qualified Test.HUnit as HUnit
import qualified Test.Hspec as H
import Test.Hspec.WebDriver hiding (runWD, click, uploadFile, WD)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver (WD(..))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD
import Data.Maybe (isNothing)

import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork

chromium :: FilePath
chromium = $(staticWhich "chromium")

seleniumPort, jsaddlePort :: PortNumber
seleniumPort = 8000
jsaddlePort = 8001

-- Orphan MonadRef WD instance (matches HydrationBrowser.hs).
instance MonadRef WD where
  type Ref WD = Ref IO
  newRef = WD . newRef
  readRef = WD . readRef
  writeRef r = WD . writeRef r

deriving instance MonadFail WD

chromeConfig :: Text -> [Text] -> WD.WDConfig
chromeConfig fp flags = WD.useBrowser
  (WD.chrome { WD.chromeBinary = Just $ T.unpack fp
             , WD.chromeOptions = T.unpack <$> flags })
  WD.defaultConfig

data HydrationFailedException = HydrationFailedException deriving Show
instance Exception HydrationFailedException

-- ---------------------------------------------------------------------
-- Selenium plumbing (matches HydrationBrowser.hs).
-- ---------------------------------------------------------------------

data Selenium = Selenium
  { _selenium_portNumber :: PortNumber
  , _selenium_stopServer :: IO ()
  }

startSeleniumServer :: PortNumber -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in = NoStream
    , std_out = NoStream
    , std_err = NoStream
    }
  return $ terminateProcess ph

withSeleniumServer :: (Selenium -> IO ()) -> IO ()
withSeleniumServer f = do
  stopServer <- startSeleniumServer seleniumPort
  threadDelay $ 1000 * 1000 * 2
  f $ Selenium
    { _selenium_portNumber = seleniumPort
    , _selenium_stopServer = stopServer
    }

withAsync' :: (MonadIO m, MonadMask m) => IO a -> m b -> m b
withAsync' f g = bracket
  (liftIO $ Async.async f)
  (liftIO . Async.uninterruptibleCancel)
  (const g)

withRetry :: forall a. WD a -> WD a
withRetry a = wait 300
  where wait :: Int -> WD a
        wait 0 = a
        wait n = try a >>= \case
          Left (e :: SomeException) -> do
            liftIO $ putStrLn ("(retrying due to " <> show e <> ")")
                       *> threadDelay 100000
            wait $ n - 1
          Right v -> return v

findElemWithRetry :: WD.Selector -> WD WD.Element
findElemWithRetry = withRetry . WD.findElem

-- ---------------------------------------------------------------------
-- The widget: minimal — one inputElement, one button, three
-- observation points.
-- ---------------------------------------------------------------------

type TestWidget t m =
  ( DomBuilder t m
  , MonadHold t m
  , PostBuild t m
  , Prerender t m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadFix m
  , MonadIO (Performable m)
  , MonadIO m
  )

reproWidget
  :: TestWidget (SpiderTimeline Global) m
  => IORef Text  -- ^ Written by performEvent_ tag on _inputElement_input
  -> IORef Text  -- ^ Written by performEvent_ on updated _inputElement_value
  -> IORef Text  -- ^ Written by performEvent_ on button-click tag
  -> m ()
reproWidget inputEvRef valueDynRef clickTagRef = do
  liftIO $ putStrLn "[widget] BEFORE subscribe input"
  liftIO $ hFlush stdout
  e <- inputElement def
  click <- button "save"
  -- Observation point 1: every fire of _inputElement_input
  performEvent_ $ ffor (_inputElement_input e) $ \t -> liftIO $ do
    putStrLn $ "[widget] _inputElement_input FIRED: " <> show t
    hFlush stdout
    writeIORef inputEvRef t
  -- Observation point 2: every update of the _inputElement_value Dynamic
  performEvent_ $ ffor (updated (_inputElement_value e)) $ \t -> liftIO $ do
    putStrLn $ "[widget] _inputElement_value UPDATED: " <> show t
    hFlush stdout
    writeIORef valueDynRef t
  -- Observation point 3: on button click, tag current value.
  -- Mirrors the failing test's exact wiring.
  performEvent_ $ ffor (tag (current (value e)) click) $ \t -> liftIO $ do
    putStrLn $ "[widget] click captured value: " <> show t
    hFlush stdout
    writeIORef clickTagRef t
  liftIO $ putStrLn "[widget] AFTER subscribe input"
  liftIO $ hFlush stdout

-- ---------------------------------------------------------------------
-- The scaffold: identical shape to testWidgetDebug' but inlined,
-- stripped of hspec, and with printfs at every stage.
-- ---------------------------------------------------------------------

runRepro :: WD.WDConfig -> WD.Capabilities -> IO ()
runRepro wdConfig caps = do
  putStrLn "[main] repro START"
  hFlush stdout
  inputEvRef  <- newIORef ("" :: Text)
  valueDynRef <- newIORef ("" :: Text)
  clickTagRef <- newIORef ("" :: Text)

  let bodyWidget :: TestWidget (SpiderTimeline Global) m => m ()
      bodyWidget = reproWidget inputEvRef valueDynRef clickTagRef
      staticApp = do
        el "head" $ pure ()
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  putStrLn "[main] rendering static"
  hFlush stdout
  ((), html) <- renderStatic $ runHydratableT staticApp
  putStrLn "[main] rendered static"
  hFlush stdout

  waitBeforeJS       <- newEmptyMVar
  waitUntilSwitchover <- newEmptyMVar
  mainTid <- myThreadId
  let onFailure = throwTo mainTid HydrationFailedException
      entryPoint = do
        liftIO $ do
          putStrLn "[jsaddle] taking waitBeforeJS"
          hFlush stdout
          takeMVar waitBeforeJS
        let switchOverAction = do
              liftIO $ do
                putStrLn "[jsaddle] switchover syncPoint"
                hFlush stdout
              syncPoint
              liftIO $ do
                putStrLn "[jsaddle] putting waitUntilSwitchover"
                hFlush stdout
                putMVar waitUntilSwitchover ()
        liftIO $ do
          putStrLn "[jsaddle] running attachHydrationWidgetWithFailure"
          hFlush stdout
        doc <- DOM.currentDocumentUnchecked
        body <- Document.getBodyUnchecked doc
        _ <- DH.attachHydrationWidgetWithFailure @Global onFailure switchOverAction body bodyWidget
        liftIO $ do
          putStrLn "[jsaddle] syncPoint after attach"
          hFlush stdout
        syncPoint

  application <- jsaddleOr defaultConnectionOptions entryPoint $ \_ sendResponse -> do
    putStrLn "[warp] sending response"
    hFlush stdout
    sendResponse $ responseLBS status200 [] $ "<!doctype html>\n" <> LBS.fromStrict html

  waitJSaddle <- newEmptyMVar
  let settings = foldr ($) Warp.defaultSettings
        [ Warp.setPort $ fromIntegral $ toInteger jsaddlePort
        , Warp.setBeforeMainLoop $ do
            putStrLn "[warp] putting waitJSaddle"
            hFlush stdout
            putMVar waitJSaddle ()
        ]
      jsaddleWarp = hSilence [stderr] $ Warp.runSettings settings application

  -- Drive one WebDriver session against a fresh warp.
  let session = WD.runSession wdConfig $ WD.finallyClose $ do
        liftIO $ do
          putStrLn "[wd] taking waitJSaddle"
          hFlush stdout
          takeMVar waitJSaddle
        liftIO $ do
          putStrLn "[wd] opening page"
          hFlush stdout
        WD.openPage $ "http://localhost:" <> show jsaddlePort
        -- Static page is up. Release the JSaddle entrypoint, then wait
        -- for switchover to complete.
        liftIO $ do
          putStrLn "[wd] putting waitBeforeJS"
          hFlush stdout
          putMVar waitBeforeJS ()
          putStrLn "[wd] taking waitUntilSwitchover"
          hFlush stdout
          takeMVar waitUntilSwitchover
          putStrLn "[wd] SWITCHOVER DONE — beginning WD interactions"
          hFlush stdout
        -- Type into the input.
        e <- findElemWithRetry (WD.ByTag "input")
        liftIO $ do
          putStrLn "[wd] sendKeys 'hello world'"
          hFlush stdout
        WD.sendKeys "hello world" e
        -- Read the DOM's value BEFORE clicking, so we can distinguish
        -- DOM-side vs Dynamic-side observations.
        domValMid <- WD.attr e "value"
        liftIO $ do
          putStrLn $ "[wd] DOM input.value after sendKeys: " <> show domValMid
          hFlush stdout
        -- Click the button.
        btn <- findElemWithRetry (WD.ByTag "button")
        WD.click btn
        liftIO $ do
          putStrLn "[wd] button clicked; waiting 500ms for FRP settle"
          hFlush stdout
          threadDelay (500 * 1000)
        -- Read again.
        domValFinal <- WD.attr e "value"
        liftIO $ putStrLn $ "[wd] DOM input.value final: " <> show domValFinal
        pure ()

  withAsync' jsaddleWarp $ do
    session
    inputEvVal  <- readIORef inputEvRef
    valueDynVal <- readIORef valueDynRef
    clickTagVal <- readIORef clickTagRef
    putStrLn ""
    putStrLn "======================================================"
    putStrLn "REPRO OBSERVATIONS"
    putStrLn "======================================================"
    putStrLn $ "  inputEvRef  (last _inputElement_input   fire) = " <> show inputEvVal
    putStrLn $ "  valueDynRef (last _inputElement_value   fire) = " <> show valueDynVal
    putStrLn $ "  clickTagRef (button click tag on current) = " <> show clickTagVal
    putStrLn "======================================================"
    hFlush stdout
    -- Non-zero exit if the FRP graph observed NOTHING. That's the
    -- signal that reproduces the failing test's assertion.
    when (T.null clickTagVal) $ do
      putStrLn "FAIL: click tag captured empty — reproduces failure."
      HUnit.assertFailure "click tag empty"

main :: IO ()
main = do
  putStrLn "[main] HydrationInputRepro START"
  hFlush stdout
  wantUnshare <- (== Nothing) <$> lookupEnv "NO_UNSHARE"
  when wantUnshare unshareNetwork
  isHeadless <- (== Nothing) <$> lookupEnv "NO_HEADLESS"
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ HUnit.assertFailure "No browser found"
      let wdConfig = WD.defaultConfig
            { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
          chromeCaps' = WD.getCaps $ chromeConfig browserPath chromeFlags
      runRepro wdConfig chromeCaps'
        `finally` _selenium_stopServer selenium
