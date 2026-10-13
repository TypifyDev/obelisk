{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bridge between the two URL-shaped route representations that
-- coexist inside @jenga-route@ after the in-flight merge, plus a
-- lossless capture of the /full/ servant request shape (method,
-- headers, body, accept types) via a mock 'RunClient' interpreter.
--
--   * The __obelisk-shaped__ side ('Jenga.Route'), where a route is a
--     dependent sum @'R' ('FullRoute' br fr)@ paired with a runtime
--     @'FullRouteEncoder' br fr@ that produces a
--     @'PageName' = ([Text], Map Text (Maybe Text))@. Purely URL.
--
--   * The __servant-shaped__ side. Two flavours:
--
--       (a) __URL-only__ via 'HasRoute' from "Jenga.Route.Servant" —
--           what 'servantRouteToURI' / 'uriToServantRoute' expose.
--
--       (b) __Lossless__ via 'HasClient' from
--           "Servant.Client.Core.HasClient". A servant client action
--           (@'Client' 'CaptureM' api@) is /interpreted/ under a mock
--           'RunClient' instance that captures the outgoing
--           'SC.Request' rather than executing it — yielding the
--           full 'ServantRequestInfo' below.
--
-- __What "lossless" means here.__ 'ServantRequestInfo' captures:
--
--   * URL (path + query, as a 'URI' reference — no scheme/authority);
--   * HTTP method;
--   * Request headers as constructed by servant (before any client
--     manager adds transport-level headers such as @Host@ /
--     @Content-Length@);
--   * Request body, /if any/, as a lazy 'LBS.ByteString' paired with
--     its 'HTTP.MediaType'. Servant supports three body constructors:
--     'RequestBodyLBS' and 'RequestBodyBS' (both are captured), and
--     'RequestBodySource' (streaming). Streaming bodies cannot be
--     reified into a pure 'ByteString' without running the
--     'SourceIO', so they are NOT modelled — 'servantRequestInfo'
--     errors at capture time if given one. In practice ace-ws
--     endpoints do not stream request bodies.
--   * Accept types (from @'[JSON, PlainText]@-style content-type
--     lists on 'Get' \/ 'Post' \/ etc.).
--
-- __What is NOT modelled.__ Response type / decoding (the bridge is
-- about the /request/ shape, not response handling); streaming
-- request bodies (see above); HTTP version (defaulted by servant and
-- rarely useful to route consumers); and any transport-level headers
-- added by the underlying manager after the client hands off the
-- request.
module Jenga.Route.Bridge
  ( -- * Lossless servant request capture
    ServantRequestInfo (..)
  , servantRequestInfo
  , CaptureM (..)
  , requestInfoToURI

    -- * Lossless obelisk projection (preserves extras)
  , obeliskWithExtras

    -- * Obelisk side ⇄ URI (URL only)
  , obeliskRouteToURI
  , uriToObeliskRoute

    -- * Servant side ⇄ URI (URL only)
  , servantRouteToURI
  , uriToServantRoute

    -- * Convenience compositions through URI
  , obeliskToServantURI
  , servantToObeliskURI
  ) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Foldable              (toList)
import           Data.Functor.Identity      (runIdentity)
import qualified Data.Map                   as Map
import           Data.Proxy                 (Proxy)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Network.HTTP.Media         as HTTP (MediaType)
import qualified Network.HTTP.Types         as HTTP (Header, Method)
import qualified Network.HTTP.Types.URI     as HT
import           Network.URI                (URI (..), nullURI, parseURIReference)

import           Servant.Client.Core        (RunClient (..))
import qualified Servant.Client.Core        as SC
import           Servant.Client.Core.Request ( RequestBody (..)
                                             , requestPath
                                             , requestQueryString
                                             , requestBody
                                             , requestAccept
                                             , requestHeaders
                                             , requestMethod
                                             )

import           Jenga.Route                (FullRoute, FullRouteEncoder, PageName,
                                             R, encode, tryDecode)
import           Jenga.Route.Servant        (HasRoute, encodeRoute, parseUrlSegments)

--------------------------------------------------------------------------------
-- Lossless: ServantRequestInfo and CaptureM interpreter
--------------------------------------------------------------------------------

-- | Full shape of a servant client request, captured without
-- executing it. See the module docstring for what is and is not
-- modelled.
data ServantRequestInfo = ServantRequestInfo
  { sri_uri     :: URI
    -- ^ Path + query as a 'URI' /reference/ (no scheme or authority).
  , sri_method  :: HTTP.Method
    -- ^ HTTP method, e.g. @"GET"@ / @"POST"@.
  , sri_headers :: [HTTP.Header]
    -- ^ Request headers as constructed by servant. Does NOT include
    -- transport-level headers added later by the client manager
    -- (@Host@, @Content-Length@, …).
  , sri_body    :: Maybe (HTTP.MediaType, LBS.ByteString)
    -- ^ Request body, if any, together with its media type.
    -- Streaming bodies ('RequestBodySource') are not supported and
    -- cause 'servantRequestInfo' to error at capture time.
  , sri_accept  :: [HTTP.MediaType]
    -- ^ Accept types declared by the endpoint's content-type list.
  } deriving (Show, Eq)

-- | Mock monad used by the servant-mock capture pattern: interpret a
-- @'Client' 'CaptureM' api@ action, but on the first outgoing
-- 'SC.Request', short-circuit and return it instead of executing it.
--
-- Monadic bind on @'Left' req@ is the standard short-circuit
-- (matching @Either e@), so the client's downstream response
-- decoding never runs — which is exactly why we can capture the
-- request without ever needing a real 'SC.Response'.
--
-- 'throwClientError' is unreachable in a well-formed servant client
-- flow (capture happens via 'runRequestAcceptStatus' before any
-- decode step could error), and is implemented as an 'error' so
-- misuse is surfaced loudly rather than silently swallowed.
newtype CaptureM a = CaptureM { runCaptureM :: Either SC.Request a }
  deriving (Functor)

instance Applicative CaptureM where
  pure = CaptureM . Right
  CaptureM (Left req) <*> _              = CaptureM (Left req)
  CaptureM (Right _)  <*> CaptureM (Left req)  = CaptureM (Left req)
  CaptureM (Right f)  <*> CaptureM (Right x)   = CaptureM (Right (f x))

instance Monad CaptureM where
  return = pure
  CaptureM (Left req) >>= _ = CaptureM (Left req)
  CaptureM (Right a)  >>= k = k a

instance RunClient CaptureM where
  runRequestAcceptStatus _ req = CaptureM (Left req)
  throwClientError err =
    error $ "Jenga.Route.Bridge.CaptureM: throwClientError before request: "
         ++ show err

-- | Interpret a fully-applied servant client action under 'CaptureM'
-- and reify the outgoing request as a 'ServantRequestInfo'.
--
-- Typical use:
--
-- @
-- data MyApi = "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId
-- myApi = Proxy \@MyApi
--
-- postUser :: User -> 'CaptureM' UserId
-- postUser = 'SC.clientIn' myApi (Proxy \@'CaptureM')
--
-- info :: ServantRequestInfo
-- info = servantRequestInfo (postUser someUser)
-- @
--
-- The argument is @'CaptureM' r@ — that is, a servant client action
-- already fully applied to all its 'ReqBody' \/ 'Capture' \/
-- 'QueryParam' inputs. Callers construct it via
-- 'SC.clientIn'@ (Proxy \@api) (Proxy \@CaptureM)@ and apply. Because
-- 'CaptureM' short-circuits on the first 'runRequestAcceptStatus',
-- the response decoding path (which would need a real 'SC.Response')
-- is never entered.
servantRequestInfo :: forall r. CaptureM r -> ServantRequestInfo
servantRequestInfo action =
  case runCaptureM action of
    Left req -> requestToInfo req
    Right _  ->
      error "Jenga.Route.Bridge.servantRequestInfo: client action returned \
            \without issuing a request; the HasClient instance did not call \
            \runRequestAcceptStatus, which is malformed for a real endpoint."

-- | Convert a captured servant 'SC.Request' into 'ServantRequestInfo'.
requestToInfo :: SC.Request -> ServantRequestInfo
requestToInfo req =
  let pathBs   = LBS.toStrict (BB.toLazyByteString (requestPath req))
      qryItems = toList (requestQueryString req)
      -- servant's HasClient instances build the path Builder with
      -- segments already percent-encoded; query items are rendered
      -- here via http-types' renderQuery (which handles encoding).
      qryBs    = HT.renderQuery True qryItems
      uri      = nullURI
        { uriPath  =
            if BS.null pathBs
              then "/"
              else BSLC.unpack (LBS.fromStrict pathBs)
        , uriQuery =
            if BS.null qryBs
              then ""
              else BSLC.unpack (LBS.fromStrict qryBs)
        }
      body     = fmap bodyToLbs (requestBody req)
  in ServantRequestInfo
       { sri_uri     = uri
       , sri_method  = requestMethod req
       , sri_headers = toList (requestHeaders req)
       , sri_body    = body
       , sri_accept  = toList (requestAccept req)
       }
  where
    bodyToLbs :: (RequestBody, HTTP.MediaType) -> (HTTP.MediaType, LBS.ByteString)
    bodyToLbs (rb, mt) = case rb of
      RequestBodyLBS lbs -> (mt, lbs)
      RequestBodyBS  bs  -> (mt, LBS.fromStrict bs)
      RequestBodySource _ ->
        error "Jenga.Route.Bridge.requestToInfo: streaming request body \
              \(RequestBodySource) is not supported by ServantRequestInfo; \
              \ace-ws endpoints do not stream request bodies."

-- | Project a 'ServantRequestInfo' to just its URL. Convenience for
-- callers that want the URL half of the lossless capture without
-- pattern-matching the record.
requestInfoToURI :: ServantRequestInfo -> URI
requestInfoToURI = sri_uri

--------------------------------------------------------------------------------
-- Lossless obelisk projection: keep the extras
--------------------------------------------------------------------------------

-- | Lossless projection from a captured 'ServantRequestInfo' onto the
-- obelisk-shaped side. Returns both the decoded obelisk route and
-- the /original/ 'ServantRequestInfo' unchanged, so callers keep
-- access to method / headers / body / accept types alongside the
-- obelisk route.
--
-- The 'Either' on the route position mirrors 'uriToObeliskRoute' and
-- exists for future generalisation; today (with @parse = Identity@
-- in 'FullRouteEncoder') it is always 'Right'.
obeliskWithExtras
  :: FullRouteEncoder br fr
  -> ServantRequestInfo
  -> (Either Text (R (FullRoute br fr)), ServantRequestInfo)
obeliskWithExtras enc info =
  (uriToObeliskRoute enc (sri_uri info), info)

--------------------------------------------------------------------------------
-- Obelisk ⇄ URI (URL only)
--------------------------------------------------------------------------------

-- | Lossless. Encode an obelisk-shaped route to its URL under the
-- given encoder, and return that URL as a 'URI' (path + query, no
-- scheme or authority — this is a URI /reference/).
obeliskRouteToURI
  :: FullRouteEncoder br fr
  -> R (FullRoute br fr)
  -> URI
obeliskRouteToURI enc r = pageNameToURI (encode enc r)

-- | Parse a 'URI' back into an obelisk-shaped route via the encoder.
--
-- __Total, in practice.__ The obelisk 'FullRouteEncoder' has
-- @parse = Identity@ — it decodes /any/ 'PageName' to some route
-- (unknown paths fall through to the 404 handler that was passed to
-- 'mkFullRouteEncoder'). And the URI ⇄ 'PageName' step below uses
-- @http-types@ primitives ('HT.decodePathSegments',
-- 'HT.parseQueryText') which are themselves total on any
-- 'ByteString'. So this function always returns 'Right'; the
-- 'Either' wrapper exists to keep the signature open for a future
-- parse layer that /can/ fail.
uriToObeliskRoute
  :: FullRouteEncoder br fr
  -> URI
  -> Either Text (R (FullRoute br fr))
uriToObeliskRoute enc uri =
  Right (runIdentity (tryDecode enc (uriToPageName uri)))

--------------------------------------------------------------------------------
-- Servant ⇄ URI (URL only, via HasRoute)
--------------------------------------------------------------------------------

-- | Lossless w.r.t. the URL surface. Renders a servant-shaped route
-- to its URL under the given @api@ and returns that URL as a 'URI'
-- reference (path + query, no scheme or authority).
--
-- URL-only: method / body / headers / content-type are not
-- representable here because 'HasRoute' does not carry them. Callers
-- that need the full request shape should use 'servantRequestInfo'
-- with 'HasClient' instead.
servantRouteToURI
  :: forall api r. HasRoute api r
  => Proxy api
  -> r
  -> URI
servantRouteToURI _ r =
  let urlText = encodeRoute @api r
  in case parseURIReference (T.unpack urlText) of
       Just u  -> u
       Nothing -> nullURI { uriPath = T.unpack urlText }

-- | Parse a 'URI' into a servant-shaped route by matching against
-- the endpoints declared by @api@. Returns 'Nothing' if no endpoint
-- in @api@ matches the URL (same semantics as 'parseUrlSegments').
uriToServantRoute
  :: forall api r. HasRoute api r
  => Proxy api
  -> URI
  -> Maybe r
uriToServantRoute pApi uri = parseUrlSegments pApi (urlOfURI uri)

--------------------------------------------------------------------------------
-- Convenience compositions
--------------------------------------------------------------------------------

-- | Convenience: encode an obelisk route to a URI. Named separately
-- from 'obeliskRouteToURI' to document intent at the call site — the
-- returned 'URI' is the same one Servant clients accept, so this is
-- the function to reach for when handing an obelisk-routed URL into
-- Servant machinery.
obeliskToServantURI
  :: FullRouteEncoder br fr
  -> R (FullRoute br fr)
  -> URI
obeliskToServantURI = obeliskRouteToURI

-- | Convenience: encode a servant route to a URI and then re-parse
-- that URI as an obelisk route.
servantToObeliskURI
  :: forall api br fr r. HasRoute api r
  => FullRouteEncoder br fr
  -> Proxy api
  -> r
  -> Either Text (R (FullRoute br fr))
servantToObeliskURI enc pApi r =
  uriToObeliskRoute enc (servantRouteToURI pApi r)

--------------------------------------------------------------------------------
-- PageName ⇄ URI
--------------------------------------------------------------------------------

-- | Encode a 'PageName' (obelisk-side path + query map) into a 'URI'
-- reference. Uses @http-types@' 'HT.encodePathSegments' for the path
-- and 'HT.renderQueryText' for the query, so percent-encoding
-- matches the rest of the ace-ws stack.
pageNameToURI :: PageName -> URI
pageNameToURI (segs, qmap) =
  let pathBs  = BB.toLazyByteString (HT.encodePathSegments segs)
      qryBs   = BB.toLazyByteString (HT.renderQueryText True (Map.toList qmap))
      pathStr = BSLC.unpack pathBs
      pathStr' = if null pathStr then "/" else pathStr
      qryStr  = BSLC.unpack qryBs
  in nullURI { uriPath = pathStr', uriQuery = qryStr }

-- | Decode a 'URI' back into a 'PageName'. Total: both
-- 'HT.decodePathSegments' and 'HT.parseQueryText' accept any
-- 'ByteString'.
uriToPageName :: URI -> PageName
uriToPageName uri =
  let pathBs = BSLC.pack (uriPath uri)
      segs   = HT.decodePathSegments (LBS.toStrict pathBs)
      qryBs  = case uriQuery uri of
                 ('?' : rest) -> BSLC.pack rest
                 other        -> BSLC.pack other
      qtext  = HT.parseQueryText (LBS.toStrict qryBs)
  in (segs, Map.fromList qtext)

-- | Render a 'URI' back to the text URL shape that
-- 'parseUrlSegments' expects (path + query, with a leading '?' on
-- the query when present). We deliberately do NOT include scheme /
-- authority / fragment here — 'parseUrlSegments' works over the
-- path+query only.
urlOfURI :: URI -> Text
urlOfURI uri = T.pack (uriPath uri ++ uriQuery uri)
