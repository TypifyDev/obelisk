{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Backend server using Snap + Servant.
--
-- Replaces @Jenga.Backend@ from obelisk. Keeps Snap as the HTTP server
-- (compatible with existing infrastructure) and uses servant-snap
-- for type-safe API routing.
--
-- The backend handles:
--
-- 1. __API requests__ → servant-snap handlers (type-safe, shared types)
-- 2. __Frontend routes__ → SSR via 'renderFrontendHtml' (pre-rendered HTML)
-- 3. __Static assets__ → direct file serving with caching
-- 4. __JS\/WASM bundle__ → serves the compiled frontend
--
-- In dev mode (jsaddle-warp), the frontend runs on the server and
-- communicates with the browser via WebSocket.
module Jenga.Backend.Servant
  ( -- * Backend type
    Backend(..)
  , BackendConfig(..)
  , defaultBackendConfig
  , StaticAssets(..)
  , defaultStaticAssets

    -- * Running the backend
  , runBackend
  , runBackendWith

    -- * Script loading
  , GhcjsWidgets(..)
  , defaultGhcjsWidgets
  , deferredGhcjsScript
  , preloadGhcjs

    -- * Snap utilities (re-exported)
  , getPageName
  , runSnapWithCommandLineArgs

    -- * Serving
  , serveStaticAssets
  , serveDefaultJengaApp

    -- * Servant-Snap (re-exported)
  , serveSnap
  , serveSnapWithContext
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Applicative ((<|>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Default (Default(def))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics (Generic)
import           Snap.Core
import           Snap.Http.Server (quickHttpServe)
import qualified Snap.Util.FileServe as Snap
import qualified Jenga.Asset.Serve.Snap as JengaAsset

import           Jenga.Route.Servant (HasRoute(..), PageName, urlToSegments)
import           Jenga.Frontend (Frontend(..), renderFrontendHtml)
import           Servant.Server (serveSnap, serveSnapWithContext)

-- ─── Types ─────────────────────────────────────────────────────

-- | The backend of a Jenga application.
--
-- Compare with obelisk's @Backend backendRoute frontendRoute@ which
-- held an @Encoder@ and a @(R backendRoute -> Snap ()) -> IO () -> IO ()@.
-- Here, the API is defined as a Servant type and the routing is
-- handled by servant-snap.
data Backend api r = Backend
  { _backend_apiHandler :: !(Snap ())
    -- ^ Snap handler for API routes (mounted under /api/).
    -- Built from servant-snap or hand-written Snap handlers.
  , _backend_frontend :: !(Frontend r)
    -- ^ The frontend, used for SSR on initial page load.
  , _backend_fallbackRoute :: !r
    -- ^ Route to render for unrecognized URLs (404 → home page)
  }

-- | Configuration for the backend server.
data BackendConfig = BackendConfig
  { _backendConfig_runSnap :: !(Snap () -> IO ())
    -- ^ Function to run the Snap server
  , _backendConfig_staticAssets :: !StaticAssets
    -- ^ Static assets (CSS, images, etc.)
  , _backendConfig_frontendAssets :: !StaticAssets
    -- ^ Compiled frontend assets (JS/WASM bundle)
  , _backendConfig_ghcjsWidgets :: !(GhcjsWidgets Text)
    -- ^ Head preload + body script HTML for the frontend bundle
  } deriving (Generic)

-- | Paths to static assets (processed = hashed, unprocessed = original).
data StaticAssets = StaticAssets
  { _staticAssets_processed :: !FilePath
  , _staticAssets_unprocessed :: !FilePath
  } deriving (Show, Eq, Generic)

-- | Head preload and body script for loading the frontend JS/WASM.
data GhcjsWidgets a = GhcjsWidgets
  { _ghcjsWidgets_preload :: !a
    -- ^ Content for @\<link rel=\"preload\"\>@ in head
  , _ghcjsWidgets_script :: !a
    -- ^ Content for @\<script\>@ in body
  } deriving (Show, Eq, Functor, Generic)

-- ─── Defaults ──────────────────────────────────────────────────

defaultStaticAssets :: StaticAssets
defaultStaticAssets = StaticAssets
  { _staticAssets_processed = "static.assets"
  , _staticAssets_unprocessed = "static"
  }

defaultFrontendAssets :: StaticAssets
defaultFrontendAssets = StaticAssets
  { _staticAssets_processed = "frontend.jsexe.assets"
  , _staticAssets_unprocessed = "frontend.jsexe"
  }

defaultGhcjsWidgets :: GhcjsWidgets Text
defaultGhcjsWidgets = GhcjsWidgets
  { _ghcjsWidgets_preload = "<link rel=\"preload\" as=\"script\" href=\"/ghcjs/all.js\">"
  , _ghcjsWidgets_script = "<script type=\"text/javascript\" src=\"/ghcjs/all.js\" defer=\"defer\"></script>"
  }

defaultBackendConfig :: BackendConfig
defaultBackendConfig = BackendConfig
  { _backendConfig_runSnap = quickHttpServe
  , _backendConfig_staticAssets = defaultStaticAssets
  , _backendConfig_frontendAssets = defaultFrontendAssets
  , _backendConfig_ghcjsWidgets = defaultGhcjsWidgets
  }

-- ─── Running ───────────────────────────────────────────────────

-- | Run the backend with default configuration.
runBackend :: HasRoute api r => Backend api r -> IO ()
runBackend = runBackendWith defaultBackendConfig

-- | Run the backend with custom configuration.
--
-- Sets up the Snap server with routes:
--
-- * @\/api\/*@ → API handler (servant-snap)
-- * @\/static\/*@ → static assets
-- * @\/ghcjs\/*@ → frontend JS/WASM bundle
-- * @\/*@ → SSR the frontend at the requested route
runBackendWith :: forall api r. HasRoute api r => BackendConfig -> Backend api r -> IO ()
runBackendWith cfg backend = do
  -- Load configs from the config/ directory
  configs <- getPublicConfigs
  let allJsUrl = "/ghcjs/all.js"
  _backendConfig_runSnap cfg $ do
    -- API routes
    route
      [ ("api", _backend_apiHandler backend)
      -- Static assets
      , ("static", serveStaticDir (_backendConfig_staticAssets cfg))
      -- Frontend JS/WASM bundle
      , ("ghcjs", serveStaticDir (_backendConfig_frontendAssets cfg))
      -- Landing page / other static pages
      , ("landing", serveStaticDir (StaticAssets "landing-page" "landing-page"))
      ]
      -- Fallback: SSR the frontend for all other routes
      <|> serveFrontendRoute cfg configs backend

-- | Serve a frontend route via SSR.
serveFrontendRoute
  :: forall api r. HasRoute api r
  => BackendConfig
  -> Map Text ByteString
  -> Backend api r
  -> Snap ()
serveFrontendRoute cfg configs backend = do
  -- Parse the URL
  pageName <- getPageName
  let (segs, qparams) = pageName
      currentRoute = case decodeRoute @api segs qparams of
        Just r  -> r
        Nothing -> _backend_fallbackRoute backend

  -- Read cookies from the request
  cookies <- fmap (\c -> (cookieName c, cookieValue c)) <$> getsRequest rqCookies

  -- Render the frontend HTML (SSR) and inject the JS/WASM bundle script
  htmlBase <- liftIO $ renderFrontendHtml
    configs
    cookies
    (encodeRoute @api)
    currentRoute
    (_backend_frontend backend)
    (pure ())  -- headExtra
    (pure ())  -- bodyExtra
  -- Inject the frontend script tag before </body>
  let scriptTag = T.encodeUtf8 (_ghcjsWidgets_script (_backendConfig_ghcjsWidgets cfg))
      html = BS.concat
        [ BS.take (BS.length htmlBase - 14) htmlBase  -- everything before </body></html>
        , scriptTag
        , "</body></html>"
        ]

  -- Serve the response
  modifyResponse $ setContentType "text/html; charset=utf-8"
  modifyResponse $ setHeader "Cache-Control" "no-store private"
  writeBS html

-- ─── Static file serving ───────────────────────────────────────

-- | Serve files from a StaticAssets directory using the obelisk-style
-- hashed-asset protocol (reads type/target metadata, emits redirects to
-- the hashed asset, serves it with proper caching + encoding negotiation).
--
-- Previously used @Snap.serveDirectory@, which does trailing-slash
-- canonicalization for the asset directories (each hashed asset is
-- structured as a dir with @type@/@target@/@encodings@ files), so every
-- request for @/ghcjs\/all.js@ got 302'd to @/ghcjs\/all.js\/@ and fell
-- through to the SPA HTML. Matches obelisk-v1's @serveStaticAssets@.
serveStaticDir :: StaticAssets -> Snap ()
serveStaticDir assets =
  JengaAsset.serveAssets
    (_staticAssets_processed assets)
    (_staticAssets_unprocessed assets)

-- | Serve static assets (compatibility alias).
serveStaticAssets :: StaticAssets -> [Text] -> Snap ()
serveStaticAssets assets _path =
  serveStaticDir assets

-- | Serve the default jenga app with standard asset configuration.
serveDefaultJengaApp
  :: HasRoute api r
  => BackendConfig
  -> Map Text ByteString
  -> Backend api r
  -> Snap ()
serveDefaultJengaApp = serveFrontendRoute

-- ─── Script loading ────────────────────────────────────────────

-- | Emit a preload link for the frontend JS bundle.
deferredGhcjsScript :: Text -> Text
deferredGhcjsScript url =
  "<script type=\"text/javascript\" src=\"" <> url <> "\" defer=\"defer\"></script>"

-- | Emit a preload hint for the frontend JS bundle.
preloadGhcjs :: Text -> Text
preloadGhcjs url =
  "<link rel=\"preload\" as=\"script\" href=\"" <> url <> "\">"

-- ─── Snap utilities ────────────────────────────────────────────

-- | Extract the page name (path segments + query params) from a Snap request.
getPageName :: MonadSnap m => m ([Text], Map Text (Maybe Text))
getPageName = do
  req <- getRequest
  let path = T.decodeUtf8 $ rqPathInfo req
      query = T.decodeUtf8 $ rqQueryString req
      (segs, _) = urlToSegments ("/" <> path)
      qparams = case T.uncons query of
        Just (_, qs) | not (T.null qs) -> parseQS qs
        _ -> Map.empty
  pure (segs, qparams)
  where
    parseQS :: Text -> Map Text (Maybe Text)
    parseQS qs = Map.fromList
      [ case T.breakOn "=" part of
          (k, v) | T.null v  -> (k, Nothing)
                  | otherwise -> (k, Just (T.drop 1 v))
      | part <- T.splitOn "&" qs
      , not (T.null part)
      ]

-- | Run Snap with command-line argument parsing.
runSnapWithCommandLineArgs :: Snap () -> IO ()
runSnapWithCommandLineArgs = quickHttpServe

-- | Read public configs (common/ and frontend/ prefixed).
getPublicConfigs :: IO (Map Text ByteString)
getPublicConfigs = do
  allConfigs <- getConfigsFromDirectory "config"
  pure $ Map.filterWithKey (\k _ -> "common/" `T.isPrefixOf` k || "frontend/" `T.isPrefixOf` k) allConfigs

-- | Read configs from a directory tree.
getConfigsFromDirectory :: FilePath -> IO (Map Text ByteString)
getConfigsFromDirectory dir = do
  -- TODO: proper recursive directory walk
  -- For now, return empty — configs come from the deployment
  pure Map.empty
