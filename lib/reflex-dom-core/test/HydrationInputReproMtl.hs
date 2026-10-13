{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | HydrationInputReproMtl -- CONTROL group for the port-vs-upstream trace comparison.
--
-- This test builds the SAME minimal `inputElement` + `performEvent_` widget as its
-- port sibling at reflex-dom-core-effectful/test/HydrationInputRepro.hs, but using
-- UPSTREAM MTL reflex-dom-core. The value: an EXPECTED-PASS trace we can diff
-- against the port's suspected-broken trace. Any divergence in the printf stream
-- from vendor/reflex's Reflex.Requester.Base.Internal (the shared-with-port codepath
-- for MTL only) localises the port's break.

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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Language.Javascript.JSaddle (syncPoint)
import Language.Javascript.JSaddle.Warp (jsaddleJs, jsaddleOr)
import Network.HTTP.Types (status200)
import Network.Socket (PortNumber)
import Network.Wai (responseLBS)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom.Core
import System.Environment (lookupEnv)
import System.IO (stderr)
import System.IO.Silently (hSilence)
import System.Process (createProcess, proc, std_in, std_out, std_err, StdStream(NoStream), terminateProcess)
import System.Which (staticWhich)
import qualified Test.HUnit as HUnit
import qualified Test.Hspec as H
import Test.Hspec.WebDriver hiding (runWD, click, uploadFile, WD)
import qualified Test.Hspec.WebDriver as WD
import Test.WebDriver (WD(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai.Handler.Warp as Warp
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD

import Test.Util.ChromeFlags
import Test.Util.UnshareNetwork

chromium :: FilePath
chromium = $(staticWhich "chromium")

seleniumPort, jsaddlePort :: PortNumber
seleniumPort = 8020
jsaddlePort  = 8021

instance MonadRef WD where
  type Ref WD = Ref IO
  newRef       = WD . newRef
  readRef      = WD . readRef
  writeRef r   = WD . writeRef r

deriving instance MonadFail WD

assertEqual :: (MonadIO m, Eq a, Show a) => String -> a -> a -> m ()
assertEqual msg a b = liftIO $ HUnit.assertEqual msg a b

chromeConfig :: Text -> [Text] -> WD.WDConfig
chromeConfig fp flags =
  WD.useBrowser
    (WD.chrome { WD.chromeBinary  = Just $ T.unpack fp
               , WD.chromeOptions = T.unpack <$> flags
               })
    WD.defaultConfig

main :: IO ()
main = do
  unshareNetwork
  isHeadless <- (== Nothing) <$> lookupEnv "NO_HEADLESS"
  withSandboxedChromeFlags isHeadless $ \chromeFlags -> do
    withSeleniumServer $ \selenium -> do
      let browserPath = T.strip $ T.pack chromium
      when (T.null browserPath) $ fail "No browser found"
      withDebugging <- isNothing <$> lookupEnv "NO_DEBUG"
      let wdConfig     = WD.defaultConfig
                           { WD.wdPort = fromIntegral $ _selenium_portNumber selenium }
          chromeCaps'  = WD.getCaps $ chromeConfig browserPath chromeFlags
      hspec (tests withDebugging wdConfig [(chromeCaps', "chrome")] selenium)
        `finally` _selenium_stopServer selenium

------------------------------------------------------------
-- The single control-group test
------------------------------------------------------------

tests :: Bool -> WD.WDConfig -> [(Capabilities, String)] -> Selenium -> Spec
tests withDebugging wdConfig caps _selenium = do
  let session' t = sessionWith wdConfig t . using caps
      runWD m = runWDOptions (WdOptions False) m
  session' "input-repro-mtl (control)" $ do
    it "types 'hello world' and DOM value + input event agree (MTL upstream)" $ runWD $ do
      -- Refs receive prints Haskell-side so we can compare with the port's trace.
      -- MTL upstream: expected PASS. Port sibling: expected to reveal divergence.
      inputRef  <- liftIO $ newIORef ([] :: [Text])
      updateRef <- liftIO $ newIORef ([] :: [Text])
      let widget :: (TestWidget (SpiderTimeline Global) m) => m ()
          widget = do
            e <- inputElement $ def
              & inputElementConfig_initialValue .~ ("" :: Text)
            performEvent_ $
              (\t -> liftIO $ do
                 putStrLn $ "MTL PE_input:  " <> show t
                 modifyIORef' inputRef (t :)
              ) <$> _inputElement_input e
            performEvent_ $
              (\t -> liftIO $ do
                 putStrLn $ "MTL PE_value:  " <> show t
                 modifyIORef' updateRef (t :)
              ) <$> updated (_inputElement_value e)
      let beforeJS = do
            liftIO $ putStrLn "MTL WD: opened page, static input should exist"
            e <- findElemWithRetry $ WD.ByTag "input"
            WD.attr e "value" `shouldBeWithRetryM` Just ""
            pure e
          afterSwitchover e = do
            liftIO $ putStrLn "MTL WD: switchover complete, typing 'hello world'"
            WD.sendKeys "hello world" e
            WD.attr e "value" `shouldBeWithRetryM` Just "hello world"
            liftIO $ putStrLn "MTL WD: DOM value matches, checking Haskell refs"
            inputVals  <- liftIO $ readIORef inputRef
            updateVals <- liftIO $ readIORef updateRef
            liftIO $ putStrLn $ "MTL final _input      log (reverse-chronological): " <> show inputVals
            liftIO $ putStrLn $ "MTL final updated val log (reverse-chronological): " <> show updateVals
            -- The DOM shows "hello world" AND the ref must have been fed at least
            -- one input event containing that final value. If MTL is healthy
            -- (which it is), this holds. If it doesn't hold the control itself
            -- is broken (and we should investigate before comparing to the port).
            assertEqual "input ref must have final value" (Just "hello world")
              (case inputVals of [] -> Nothing; (x:_) -> Just x)
            assertEqual "value ref must have final value" (Just "hello world")
              (case updateVals of [] -> Nothing; (x:_) -> Just x)
      testWidgetDebug' True withDebugging beforeJS afterSwitchover widget

------------------------------------------------------------
-- Selenium wrapper (identical shape to hydration.hs)
------------------------------------------------------------

data Selenium = Selenium
  { _selenium_portNumber :: PortNumber
  , _selenium_stopServer :: IO ()
  }

startSeleniumServer :: PortNumber -> IO (IO ())
startSeleniumServer port = do
  (_,_,_,ph) <- createProcess $ (proc "selenium-server" ["-port", show port])
    { std_in  = NoStream
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

------------------------------------------------------------
-- Small WD helpers (mirror of hydration.hs)
------------------------------------------------------------

findElemWithRetry :: Selector -> WD WD.Element
findElemWithRetry = withRetry . WD.findElem

shouldBeWithRetryM :: (Eq a, Show a) => WD a -> a -> WD ()
shouldBeWithRetryM m expected = withRetry $ do
  got <- m
  got `shouldBe` expected

withRetry :: forall a. WD a -> WD a
withRetry a = wait 300
  where
    wait :: Int -> WD a
    wait 0 = a
    wait n = try a >>= \case
      Left (e :: SomeException) -> do
        liftIO $ putStrLn ("(retrying due to " <> show e <> ")")
                *> threadDelay 100000
        wait $ n - 1
      Right v -> return v

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' r f = do
  x <- readIORef r
  let !x' = f x
  writeIORef r x'

------------------------------------------------------------
-- testWidgetDebug' -- LITERAL COPY from hydration.hs so both test binaries
-- share the same harness path.
------------------------------------------------------------

type TestWidget t m =
  ( DomBuilder t m, MonadHold t m, PostBuild t m, Prerender t m
  , PerformEvent t m, TriggerEvent t m, MonadFix m
  , MonadIO (Performable m), MonadIO m
  )

data HydrationFailedException = HydrationFailedException deriving Show
instance Exception HydrationFailedException

testWidgetDebug'
  :: Bool
  -> Bool
  -> WD a
  -> (a -> WD b)
  -> (forall m. TestWidget (SpiderTimeline Global) m => m ())
  -> WD b
testWidgetDebug' hardFailure withDebugging beforeJS afterSwitchover bodyWidget = do
  let putStrLnDebug :: MonadIO m => Text -> m ()
      putStrLnDebug m = when withDebugging $ liftIO $ putStrLn $ T.unpack m
      staticApp = do
        el "head" $ pure ()
        el "body" $ do
          bodyWidget
          el "script" $ text $ TE.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  putStrLnDebug "rendering static"
  ((), html) <- liftIO $ renderStatic $ runHydratableT staticApp
  putStrLnDebug "rendered static"
  waitBeforeJS <- liftIO newEmptyMVar
  onFailure <-
    if hardFailure
      then (`throwTo` HydrationFailedException) <$> liftIO myThreadId
      else pure $ pure ()
  waitUntilSwitchover <- liftIO newEmptyMVar
  let entryPoint = do
        putStrLnDebug "taking waitBeforeJS"
        liftIO $ takeMVar waitBeforeJS
        let switchOverAction = do
              putStrLnDebug "switchover syncPoint"
              syncPoint
              putStrLnDebug "putting waitUntilSwitchover"
              liftIO $ putMVar waitUntilSwitchover ()
              putStrLnDebug "put waitUntilSwitchover"
        putStrLnDebug "running mainHydrationWidgetWithSwitchoverAction"
        mainHydrationWidgetWithSwitchoverActionWithFailure'
          onFailure switchOverAction blank bodyWidget
        putStrLnDebug "syncPoint after mainHydrationWidgetWithSwitchoverAction"
        syncPoint
  application <- liftIO $ jsaddleOr defaultConnectionOptions entryPoint $
    \_ sendResponse -> do
      putStrLnDebug "sending response"
      r <- sendResponse $ responseLBS status200 [] $
             "<!doctype html>\n" <> LBS.fromStrict html
      putStrLnDebug "sent response"
      return r
  waitJSaddle <- liftIO newEmptyMVar
  let settings = foldr ($) Warp.defaultSettings
        [ Warp.setPort $ fromIntegral $ toInteger jsaddlePort
        , Warp.setBeforeMainLoop $ do
            putStrLnDebug "putting waitJSaddle"
            putMVar waitJSaddle ()
            putStrLnDebug "put waitJSaddle"
        ]
      silenceIfDebug = if withDebugging then id else hSilence [stderr]
      jsaddleWarp = silenceIfDebug $ Warp.runSettings settings application
  withAsync' jsaddleWarp $ do
    putStrLnDebug "taking waitJSaddle"
    liftIO $ takeMVar waitJSaddle
    putStrLnDebug "opening page"
    WD.openPage $ "http://localhost:" <> show jsaddlePort
    putStrLnDebug "running beforeJS"
    a <- beforeJS
    putStrLnDebug "putting waitBeforeJS"
    liftIO $ putMVar waitBeforeJS ()
    putStrLnDebug "taking waitUntilSwitchover"
    liftIO $ takeMVar waitUntilSwitchover
    putStrLnDebug "running afterSwitchover"
    afterSwitchover a

withAsync' :: (MonadIO m, MonadMask m) => IO a -> m b -> m b
withAsync' f g = bracket
  (liftIO $ Async.async f)
  (liftIO . Async.uninterruptibleCancel)
  (\_ -> g)
