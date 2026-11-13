{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A single browser origin for the Snap backend and native JSaddle client.
module Jenga.Backend.Development (runDevelopmentBackend) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8 as BS
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Jenga.Backend
import Jenga.Frontend (Frontend, FrontendMode (..), runFrontendWithConfigsAndCurrentRoute)
import Jenga.Route
import qualified Jenga.Route.Servant as Route
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.ReverseProxy
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (isWebSocketsReq)
import qualified Snap.Http.Server as Snap
import Snap (Snap)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

runDevelopmentBackend
  :: forall br fr. Route.HasRoute br (R fr)
  => Backend br fr -> Frontend (R fr) -> R fr -> IO ()
runDevelopmentBackend backend frontend fallback = do
  encoder <- either (fail . Text.unpack) pure $ checkEncoder $ _backend_routeEncoder backend
  publicConfig <- Snap.commandLineConfig (Snap.setPort 8000 Snap.defaultConfig :: Snap.Config Snap ())
  nativePort <- readNativePort
  backendPort <- newEmptyMVar
  let internal = Snap.setStartupHook (publishPort backendPort) $
        Snap.setBind "127.0.0.1" $ Snap.setPort 0 Snap.defaultConfig
  concurrently_ (runBackendWithConfig internal backend frontend) $ do
    port <- takeMVar backendPort
    serveDevelopment encoder (fromMaybe 8000 $ Snap.getPort publicConfig) port nativePort $
      runNativeFrontend (Proxy :: Proxy br) frontend fallback

runNativeFrontend :: Route.HasRoute br (R fr) => Proxy br -> Frontend (R fr) -> R fr -> IO ()
runNativeFrontend proxy frontend fallback = do
  configs <- getPublicConfigs
  runFrontendWithConfigsAndCurrentRoute (FrontendMode False False) configs proxy fallback frontend

readNativePort :: IO Int
readNativePort = do
  configured <- lookupEnv "JSADDLE_WARP_PORT"
  case traverse readMaybe configured of
    Just port | let value = fromMaybe 3003 port, value > 0 && value <= 65535 ->
      pure $ fromMaybe 3003 port
    _ -> ioError $ userError "JSADDLE_WARP_PORT must be an integer between 1 and 65535"

publishPort :: MVar Int -> Snap.StartupInfo m a -> IO ()
publishPort target startup = case Snap.getStartupSockets startup of
  [socket] -> do
    address <- Socket.getSocketName socket
    case address of
      Socket.SockAddrInet port _ -> putMVar target $ fromIntegral port
      Socket.SockAddrInet6 port _ _ _ -> putMVar target $ fromIntegral port
      _ -> ioError $ userError "Development backend did not open a TCP listener"
  _ -> ioError $ userError "Development backend requires exactly one internal listener"

serveDevelopment
  :: Encoder Identity Identity (R (FullRoute br fr)) PageName
  -> Int -> Int -> Int -> IO () -> IO ()
serveDevelopment encoder publicPort backendPort nativePort native = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  -- Match jsaddle-warp's timeout for long-lived browser sessions.
  let settings = Warp.setPort publicPort $ Warp.setTimeout 3600 Warp.defaultSettings
  concurrently_ native $ Warp.runSettings settings $
    waiProxyToSettings (pure . chooseProxy encoder backendPort nativePort)
      defaultWaiProxySettings manager

chooseProxy
  :: Encoder Identity Identity (R (FullRoute br fr)) PageName
  -> Int -> Int -> Wai.Request -> WaiProxyResponse
chooseProxy encoder backendPort nativePort request
  | nativeTransport request = WPRProxyDest native
  | frontendPage encoder request = WPRModifiedRequest
      request { Wai.pathInfo = [], Wai.rawPathInfo = "/" } native
  | otherwise = WPRProxyDest $ ProxyDest "127.0.0.1" backendPort
  where native = ProxyDest "127.0.0.1" nativePort

nativeTransport :: Wai.Request -> Bool
nativeTransport request = case Wai.pathInfo request of
  [] -> isWebSocketsReq request
  ["jsaddle.js"] -> True
  ["sync", _] -> True
  _ -> False

frontendPage
  :: Encoder Identity Identity (R (FullRoute br fr)) PageName
  -> Wai.Request -> Bool
frontendPage encoder request = case decode encoder page of
  FullRoute_Frontend (JengaRoute_App _) :/ _ -> True
  _ -> False
  where
    page = byteStringsToPageName (BS.dropWhile (== '/') $ Wai.rawPathInfo request)
      (BS.dropWhile (== '?') $ Wai.rawQueryString request)
