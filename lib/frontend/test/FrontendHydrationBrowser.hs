{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, readMVar, threadDelay, tryReadMVar)
import Control.Exception (bracket, catch, throwIO)
import Control.Monad (replicateM_, unless, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.NodeList as NodeList
import qualified GHCJS.DOM.NonElementParentNode as NonElementParentNode
import qualified GHCJS.DOM.ParentNode as ParentNode
import GHCJS.DOM.Types (Element (..), HTMLElement (..), castTo)
import qualified Jenga.Configs.MTL as Config
import Jenga.Frontend
import Jenga.Frontend.MTL (FrontendMTLWidget)
import Jenga.Route.Servant (HasRoute (..))
import Language.Javascript.JSaddle (JSM, askJSM, runJSM, syncPoint)
import Language.Javascript.JSaddle.Warp (jsaddleJs, jsaddleOr)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (ConnectionException (..), defaultConnectionOptions)
import qualified Reflex.Dom.Core as R
import qualified Reflex.Effectful.DomHost as E
import System.Directory (findExecutable)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)

data TestApi

instance HasRoute TestApi () where
  encodeRoute () = "/"
  decodeRoute _ _ = Just ()

main :: IO ()
main = do
  ready <- newEmptyMVar
  clicked <- newEmptyMVar
  completed <- newEmptyMVar
  app <- probeApplication ready clicked completed
  runBrowserCheck app completed
  runDivergenceCheck
  putStrLn "Frontend SSR, supplied configs, prerender hydration, click and counter update passed."

runDivergenceCheck :: IO ()
runDivergenceCheck = do
  failed <- newEmptyMVar
  completed <- newEmptyMVar
  let server = FrontendMTL (\_ -> pure ()) (\_ -> divergenceBody "server")
  html <- renderFrontendHtml Map.empty [] (const "/") () server (pure ()) $
    E.el "script" $ E.text $ Text.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  app <- jsaddleOr defaultConnectionOptions (hydrateDivergence failed completed) $
    \_ respond -> respond $ responseLBS status200 [("Content-Type", "text/html")] $ LBS.fromStrict html
  runBrowserCheck app completed

-- A mismatch followed by siblings must report once without multiplying the
-- diagnostics inherited by each nested element.
divergenceBody :: R.DomBuilder t m => Text.Text -> m ()
divergenceBody value = R.el "main" $ do
  R.el "p" $ R.text value
  replicateM_ 40 $ R.el "span" $ R.text "unchanged"

hydrateDivergence :: MVar () -> MVar () -> JSM ()
hydrateDivergence failed completed =
  E.runHydrationWidgetWithHeadAndBodyWithFailureJSM (putMVar failed ())
    (finishDivergence failed completed) $ \appendHead appendBody -> do
      appendHead $ E.liftEff baseTag
      appendBody $ E.liftEff $ divergenceBody "client"

finishDivergence :: MVar () -> MVar () -> JSM ()
finishDivergence failed completed = liftIO $ do
  observed <- tryReadMVar failed
  unless (observed == Just ()) $ fail "Hydration accepted mismatched server content"
  putMVar completed ()

probeApplication :: MVar () -> MVar () -> MVar () -> IO Application
probeApplication ready clicked completed = do
  -- Client startup state may have advanced since the server rendered the DOM.
  let configs = Map.singleton "frontend/probe" "configured"
      server = probeFrontend "server" (putMVar ready ()) (putMVar clicked ())
      client = probeFrontend "client" (putMVar ready ()) (putMVar clicked ())
  html <- renderFrontendHtml configs [] (const "/") () server (pure ()) $
    E.el "script" $ E.text $ Text.decodeUtf8 $ LBS.toStrict $ jsaddleJs False
  jsaddleOr defaultConnectionOptions (hydrate ready clicked completed configs client) $
    \_ respond -> respond $ responseLBS status200
      [("Content-Type", "text/html")] $ LBS.fromStrict html

runBrowserCheck :: Application -> MVar () -> IO ()
runBrowserCheck app completed =
  (Warp.testWithApplication (pure app) $ \port ->
    withBrowser port $ awaitEvent "Hydrated browser check did not complete" completed)
  `catch` normalBrowserShutdown completed

awaitEvent :: String -> MVar () -> IO ()
awaitEvent failure event = do
  result <- timeout 20000000 $ readMVar event
  unless (result == Just ()) $ fail failure

-- Closing the test browser ends its websocket after the assertion has passed.
-- Earlier disconnects and all other exceptions remain failures.
normalBrowserShutdown :: MVar () -> ConnectionException -> IO ()
normalBrowserShutdown completed failure = case failure of
  CloseRequest 1001 _ -> do
    result <- tryReadMVar completed
    unless (result == Just ()) $ throwIO failure
  _ -> throwIO failure

probeFrontend :: Text.Text -> IO () -> IO () -> Frontend ()
probeFrontend startupPhase ready completed = FrontendMTL
  (\_ -> R.el "title" $ R.text "Hydration probe")
  (probeBody startupPhase ready completed)

probeBody :: Text.Text -> IO () -> IO () -> FrontendMTLWidget ()
probeBody startupPhase ready completed _cookies = do
  probeConfig
  clicked <- counterProbe
  attributeProbe startupPhase clicked
  R.performEvent_ $ liftIO completed <$ clicked
  R.prerender_ (pure ()) $ R.getPostBuild >>= R.performEvent_ . fmap (const $ liftIO ready)

probeConfig :: (R.DomBuilder t m, Config.HasConfigs m) => m ()
probeConfig = do
  config <- Config.getConfig "frontend/probe"
  R.elAttr "div" (Map.singleton "id" "config") $
    R.text $ maybe "missing" Text.decodeUtf8 config

counterProbe
  :: (R.DomBuilder t m, R.PostBuild t m, R.MonadHold t m, MonadFix m)
  => m (R.Event t ())
counterProbe = do
  (button, ()) <- R.elAttr' "button" (Map.singleton "id" "probe") $ R.text "Count"
  let clicked = R.domEvent R.Click button
  count <- R.foldDyn (+) (0 :: Int) (1 <$ clicked)
  R.elAttr "span" (Map.singleton "id" "counter") $ R.dynText $ Text.pack . show <$> count
  pure clicked

attributeProbe
  :: (R.DomBuilder t m, R.PostBuild t m, R.MonadHold t m)
  => Text.Text -> R.Event t () -> m ()
attributeProbe startupPhase clicked = do
  postBuild <- R.getPostBuild
  let base = Map.singleton "id" "attribute-probe"
      initial = base <> Map.singleton "data-old" "remove-me"
      startup = base <> Map.fromList [("data-phase", startupPhase), ("data-startup", "ready")]
      updated = base <> Map.singleton "data-phase" "clicked"
  attrs <- R.holdDyn initial $ R.leftmost [updated <$ clicked, startup <$ postBuild]
  R.elDynAttr "div" attrs $ R.text "Attributes"

hydrate :: MVar () -> MVar () -> MVar () -> Map.Map Text.Text ByteString -> Frontend () -> JSM ()
hydrate ready clicked completed configs frontend = do
  runFrontendWithConfigsAndCurrentRouteJSM
    (FrontendMode True False) configs (Proxy @TestApi) () frontend
  syncPoint
  liftIO $ awaitEvent "Client prerender branch did not initialize" ready
  assertStartupAttributes
  assertUniqueCounterAndClick
  liftIO $ awaitEvent "Hydrated click did not reach the MTL handler" clicked
  assertCounterUpdate
  liftIO $ putMVar completed ()

assertStartupAttributes :: JSM ()
assertStartupAttributes = do
  doc <- DOM.currentDocumentUnchecked
  element <- NonElementParentNode.getElementByIdUnchecked doc ("attribute-probe" :: Text.Text)
  phase <- Element.getAttribute element ("data-phase" :: Text.Text) :: JSM (Maybe Text.Text)
  removed <- Element.getAttribute element ("data-old" :: Text.Text) :: JSM (Maybe Text.Text)
  unless (phase == Just "client" && removed == Nothing) $
    liftIO $ fail "Hydration lost an attribute update that occurred before switchover"

assertUniqueCounterAndClick :: JSM ()
assertUniqueCounterAndClick = do
  syncPoint
  doc <- DOM.currentDocumentUnchecked
  nodes <- ParentNode.querySelectorAll doc ("#counter" :: Text.Text)
  size <- NodeList.getLength nodes
  unless (size == 1) $ liftIO $ fail "Hydration duplicated the SSR counter"
  button <- NonElementParentNode.getElementByIdUnchecked doc ("probe" :: Text.Text)
  htmlButton <- castTo HTMLElement button >>= maybe (liftIO $ fail "Probe is not HTML") pure
  HTMLElement.click htmlButton
  syncPoint

assertCounterUpdate :: JSM ()
assertCounterUpdate = do
  context <- askJSM
  result <- liftIO $ timeout 20000000 $ runJSM waitForCounter context
  unless (result == Just ()) $ liftIO $ fail "Hydrated counter did not render the click"

waitForCounter :: JSM ()
waitForCounter = do
  doc <- DOM.currentDocumentUnchecked
  counter <- NonElementParentNode.getElementByIdUnchecked doc ("counter" :: Text.Text)
  value <- Node.getTextContent counter
  element <- NonElementParentNode.getElementByIdUnchecked doc ("attribute-probe" :: Text.Text)
  phase <- Element.getAttribute element ("data-phase" :: Text.Text) :: JSM (Maybe Text.Text)
  removed <- Element.getAttribute element ("data-startup" :: Text.Text) :: JSM (Maybe Text.Text)
  unless (value == Just ("1" :: Text.Text) && phase == Just "clicked" && removed == Nothing) $
    liftIO (threadDelay 10000) >> waitForCounter

withBrowser :: Int -> IO a -> IO a
withBrowser port action = withSystemTempDirectory "jenga-hydration-browser" $ \profile -> do
  browser <- findExecutable "chromium" >>= maybe (fail "chromium unavailable") pure
  let args = ["--headless", "--disable-gpu", "--disable-extensions", "--disable-default-apps",
              "--no-first-run", "--user-data-dir=" <> profile,
              "http://127.0.0.1:" <> show port]
  bracket (createProcess (proc browser args) { std_out = NoStream, std_err = Inherit })
    (\(_, _, _, process) -> terminateProcess process >> void (waitForProcess process))
    (const action)
