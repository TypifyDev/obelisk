{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant-based route encoding/decoding for Jenga.
--
-- Replaces obelisk-route's @Encoder@ chain with Servant type-level
-- combinators. Route validity is checked at compile time instead
-- of startup.
--
-- Formerly lived in the standalone @jenga-route-servant@ package.
-- Now co-hosted alongside 'Jenga.Route' inside @jenga-route@; both
-- encoders share the type surface exported from 'Jenga.Route.Core'.
--
-- The dead @ResourceRoute_JSaddleWarp :: ResourceRoute [Text]@
-- variant from the servant-package version has been dropped in favour
-- of the obelisk-shaped @ResourceRoute_JSaddleWarp :: ResourceRoute (R JSaddleWarpRoute)@
-- (declared in 'Jenga.Route.Core'), so both encoder families see the
-- same single 'ResourceRoute' GADT.
module Jenga.Route.Servant
  ( -- * Core types (re-exported)
    R
  , pattern (:/)
  , pattern (:.)
  , PageName

    -- * Route hierarchy (re-exported)
  , FullRoute(..)
  , JengaRoute(..)
  , ResourceRoute(..)

    -- * Servant-based encoding
  , Page
  , HasRoute(..)
  , renderRoute
  , renderServantFrontendRoute
  , renderServantBackendRoute

    -- * URL parsing
  , parseUrlSegments
  , urlToSegments

    -- * Re-exports
  , Proxy(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Jenga.Route.Core

-- ─── Servant Page marker ───────────────────────────────────────

-- | Terminal marker for frontend page routes in Servant types.
-- Unlike @Get '[JSON] a@, this doesn't imply a server response —
-- it marks a URL pattern for frontend routing.
--
-- @
-- type FrontendPages =
--        "main"  :> Page
--   :<|> "login" :> Page
--   :<|> "user"  :> Capture "uid" Int :> Page
-- @
data Page

-- ─── HasRoute class ────────────────────────────────────────────

-- | Bidirectional route encoding using Servant types.
--
-- This replaces obelisk-route's @Encoder@ + @checkEncoder@ pattern.
-- Route validity is checked at compile time by Servant's type system
-- instead of at startup by the @check@ monad.
--
-- @api@ is the Servant API type, @r@ is the route sum type.
class HasRoute api r where
  -- | Encode a route value to a URL path.
  encodeRoute :: r -> Text

  -- | Decode a URL path to a route value. Returns Nothing for
  -- unrecognized paths.
  decodeRoute :: [Text] -> Map Text (Maybe Text) -> Maybe r

-- ─── URL rendering ─────────────────────────────────────────────

-- | Render a route to a URL text. Uses 'encodeRoute'.
renderRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderRoute _ = encodeRoute @api

-- | Render a frontend route.
--
-- Renamed from @renderFrontendRoute@ to disambiguate against the
-- obelisk-encoder sibling in 'Jenga.Route'.
renderServantFrontendRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderServantFrontendRoute = renderRoute @api

-- | Render a backend route.
--
-- Renamed from @renderBackendRoute@ to disambiguate against the
-- obelisk-encoder sibling in 'Jenga.Route'.
renderServantBackendRoute :: forall api r. HasRoute api r => Proxy api -> r -> Text
renderServantBackendRoute = renderRoute @api

-- ─── URL parsing helpers ───────────────────────────────────────

-- | Split a URL into path segments and query parameters.
urlToSegments :: Text -> ([Text], Map Text (Maybe Text))
urlToSegments url =
  let (path, queryPart) = T.breakOn "?" url
      segments = filter (not . T.null) $ T.splitOn "/" path
      queryParams = case T.uncons queryPart of
        Just ('?', qs) -> parseQueryString qs
        _ -> Map.empty
  in (segments, queryParams)

-- | Parse a query string into key-value pairs.
parseQueryString :: Text -> Map Text (Maybe Text)
parseQueryString qs = Map.fromList
  [ case T.breakOn "=" part of
      (k, v) | T.null v  -> (k, Nothing)
              | otherwise -> (k, Just (T.drop 1 v))
  | part <- T.splitOn "&" qs
  , not (T.null part)
  ]

-- | Parse path segments into a route using 'decodeRoute'.
parseUrlSegments :: forall api r. HasRoute api r => Proxy api -> Text -> Maybe r
parseUrlSegments _ url =
  let (segs, qparams) = urlToSegments url
  in decodeRoute @api segs qparams
