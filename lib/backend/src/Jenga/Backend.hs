{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Snap-based backend scaffolding for the Jenga stack.
--
-- The MTL-era ghcjs frontend rendering pipeline (renderFrontendHtml,
-- serveGhcjsApp, GhcjsWidgets, HasCookies Snap) previously lived here
-- and depended on 'FrontendWidgetT'. That transformer has been removed
-- as part of the effect port. The SSR pipeline is now provided by
-- 'jenga-backend-servant' via 'Jenga.Frontend.renderFrontendHtml'.
--
-- What remains in this module is the Snap-agnostic plumbing that other
-- callers still rely on: config lookup, page-name / route decoding,
-- static-asset paths, and a couple of small Snap runner helpers.
module Jenga.Backend
  ( Backend (..)
  , StaticAssets (..)
  , defaultStaticAssets
  , defaultFrontendGhcjsAssets
  -- * Snap runners
  , runSnapWithCommandLineArgs
  , runSnapWithConfig
  , prettifyOutput
  -- * Route helpers
  , getPageName
  , getRouteWith
  , renderAllJsPath
  -- * Static rendering
  , staticRenderContentType
  , getPublicConfigs
  -- * Re-exports
  , Default (def)
  ) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ >= 906
import Control.Monad.IO.Class (MonadIO (liftIO))
#else
import Control.Monad.Except
#endif
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Jenga.ExecutableConfig.Lookup as Lookup
import Jenga.Route
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe
            , rqPathInfo, rqQueryString)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) Identity (R (FullRoute backendRoute frontendRoute)) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  } deriving (Generic)

prettifyOutput :: IO ()
prettifyOutput = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

defaultStaticAssets :: StaticAssets
defaultStaticAssets = StaticAssets
  { _staticAssets_processed = "static.assets"
  , _staticAssets_unprocessed = "static"
  }

defaultFrontendGhcjsAssets :: StaticAssets
defaultFrontendGhcjsAssets = StaticAssets
  { _staticAssets_processed = "frontend.jsexe.assets"
  , _staticAssets_unprocessed = "frontend.jsexe"
  }

runSnapWithConfig :: MonadIO m => Config Snap a -> Snap () -> m ()
runSnapWithConfig conf a = do
  let httpConf = conf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  liftIO $ httpServe httpConf a

-- Get the web server configuration from the command line
runSnapWithCommandLineArgs :: MonadIO m => Snap () -> m ()
runSnapWithCommandLineArgs s = liftIO (commandLineConfig defaultConfig) >>= \c ->
  runSnapWithConfig c s

getPageName :: (MonadSnap m) => m PageName
getPageName = do
  p <- getsRequest rqPathInfo
  q <- getsRequest rqQueryString
  return $ byteStringsToPageName p q

getRouteWith :: (MonadSnap m) => Encoder Identity parse route PageName -> m (parse route)
getRouteWith e = do
  pageName <- getPageName
  return $ tryDecode e pageName

renderAllJsPath :: Encoder Identity Identity (R (FullRoute a b)) PageName -> Text
renderAllJsPath validFullEncoder =
  renderJengaRoute validFullEncoder $ FullRoute_Frontend (JengaRoute_Resource ResourceRoute_Ghcjs) :/ ["all.js"]

data StaticAssets = StaticAssets
  { _staticAssets_processed :: !FilePath
  , _staticAssets_unprocessed :: !FilePath
  }
  deriving (Show, Read, Eq, Ord)

staticRenderContentType :: ByteString
staticRenderContentType = "text/html; charset=utf-8"

-- | Get configs from the canonical "public" locations (i.e., locations that Jenga expects to make available
-- to frontend applications, and hence visible to end users).
getPublicConfigs :: IO (Map Text ByteString)
getPublicConfigs = Map.filterWithKey (\k _ -> isMemberOf k ["common", "frontend"]) <$> Lookup.getConfigs
  where
    isMemberOf k = any (`T.isPrefixOf` k)
