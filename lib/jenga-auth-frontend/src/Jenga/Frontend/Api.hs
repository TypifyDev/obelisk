{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jenga.Frontend.Api where

import Jenga.Common.HasJengaConfig
import Effectful.Reader.Static (Reader)
import Jenga.Common.Errors
import Jenga.Route.Servant (HasRoute (..))
import Jenga.Route (R, renderObeliskBackendRoute)
import Reflex.Class (Reflex, Event, current, gate, leftmost, never, fanEither)
import Reflex.Dynamic (switchDyn)
import Reflex.Dom.Core (XhrRequest, XhrResponse, postJson
                       , xhrRequest_config, xhrRequestConfig_withCredentials
                       , xhrRequestConfig_headers, _xhrResponse_responseText
                       , performRequestAsync)
import qualified Reflex.Dom.Core as RDC
import Control.Monad.Reader (MonadReader)

import Data.Typeable
import Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import Text.Parsec
import Control.Applicative (some)
import Data.Bifunctor
import Data.Maybe
import Data.Functor.Identity
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Control.Lens ((.~), (&))

import Effectful (Eff, (:>), IOE)
import Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Host.HeadlessEffectful (PostBuild)
import Reflex.Host.HeadlessEffectful (TriggerEvent)
import Reflex.Host.HeadlessEffectful (PerformEventH, PerformEventM, PerformableForRow)
import qualified Reflex.Host.Class as RH
import Control.Monad.IO.Class (MonadIO)
import Language.Javascript.JSaddle.Types (MonadJSM)
import Reflex.Effectful.DomHost (DomBuilderE)
import Reflex.Dom.Core (GhcjsDomSpace)
import Reflex.Effectful.DomHost (HasPrerender, prerender)
import Reflex.Effectful.DomHost (ClientEs)
import Reflex.Effectful.DomHost (JSM')
import Jenga.Frontend.Effectful.Configs (Configs)

eitherDecodeText :: FromJSON a => T.Text -> Either String a
eitherDecodeText = eitherDecode . LBS.fromStrict . T.encodeUtf8

decodeXhrResponse' :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponse' = (fromMaybe $ Left "no response text") . fmap eitherDecodeText . _xhrResponse_responseText

-- | Constraint bundle needed to call any of the @runAPI*@ family.
-- Encapsulates the ClientEs-scoped constraints that
-- 'runAPIWithHeaders' internally requires so callers don't have to
-- re-list them.
type RunAPI t space es =
  ( Configs :> es
  , HasPrerender t space es
  , DomBuilderE t space :> es
  , space ~ GhcjsDomSpace
  , Hold t :> es, Sample t :> es
  , PostBuild t :> es
  , Reflex t
  , PerformEventH t :> es, TriggerEvent t :> es, JSM' :> es, IOE :> es
  , Configs :> ClientEs t space es
  , PerformEventH t :> ClientEs t space es
  , PerformEventM (RH.HostFrame t) t :> ClientEs t space es
  , TriggerEvent t :> ClientEs t space es
  , PerformableForRow (ClientEs t space es) ~ RH.HostFrame t
  , RH.ReflexHost t
  , MonadIO (RH.HostFrame t)
  , MonadJSM (PerformableForRow (ClientEs t space es))
  , JSM' :> ClientEs t space es
  , IOE :> ClientEs t space es
  )

runAPI
  :: forall api r t space es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es
     , HasPrerender t space es
     , DomBuilderE t space :> es
     , space ~ GhcjsDomSpace
        , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , Configs :> ClientEs t space es
     , Reader cfg :> ClientEs t space es
     , PerformEventH t :> ClientEs t space es
     , PerformEventM (RH.HostFrame t) t :> ClientEs t space es
     , TriggerEvent t :> ClientEs t space es
     , PerformableForRow (ClientEs t space es) ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow (ClientEs t space es))
     , JSM' :> ClientEs t space es
     , IOE :> ClientEs t space es
     , PerformEventH t :> es, TriggerEvent t :> es, JSM' :> es, IOE :> es
     , Reflex t
     )
  => r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPI route evPayload = runAPIWithHeaders @api @r @t @space @es @cfg route mempty evPayload

runAPIWithHeaders
  :: forall api r t space es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es
     , HasPrerender t space es
     , DomBuilderE t space :> es
     , space ~ GhcjsDomSpace
        , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , Configs :> ClientEs t space es
     , Reader cfg :> ClientEs t space es
     , PerformEventH t :> ClientEs t space es
     , PerformEventM (RH.HostFrame t) t :> ClientEs t space es
     , TriggerEvent t :> ClientEs t space es
     , PerformableForRow (ClientEs t space es) ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow (ClientEs t space es))
     , JSM' :> ClientEs t space es
     , IOE :> ClientEs t space es
     , PerformEventH t :> es, TriggerEvent t :> es, JSM' :> es, IOE :> es
     , Reflex t
     )
  => r
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIWithHeaders route headers evPayload = do
  fmap fanResponse' $ runRequestE @t @space $
    performJSONRequestResponseAnnotatedWithHeaders @api @r @t @(ClientEs t space es) @cfg route headers evPayload

-- PARKED: Eff version; GHC 9.14 ship prefers MTL. Future: re-enable when
-- effectful port completes. The MTL-styled `runAPIPostBuild` below is the
-- live definition; this block is kept verbatim so the Eff shape is
-- recoverable when the effectful stack is wired back in.
{-
runAPIPostBuild
  :: forall api r t space es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es
     , HasPrerender t space es
     , DomBuilderE t space :> es
     , space ~ GhcjsDomSpace
        , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , Configs :> ClientEs t space es
     , Reader cfg :> ClientEs t space es
     , PerformEventH t :> ClientEs t space es
     , PerformEventM (RH.HostFrame t) t :> ClientEs t space es
     , TriggerEvent t :> ClientEs t space es
     , PerformableForRow (ClientEs t space es) ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow (ClientEs t space es))
     , JSM' :> ClientEs t space es
     , IOE :> ClientEs t space es
     , PerformEventH t :> es, TriggerEvent t :> es, JSM' :> es, IOE :> es
     , PostBuild t :> es, Hold t :> es, Sample t :> es
     , Reflex t
     )
  => r
  -> (Event t () -> Event t toJson)
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIPostBuild route withPb = do
  fmap fanResponse' $ runRequest' performWithPb
  where
    runRequest' :: (Event t () -> Eff (ClientEs t space es) (Event t (Either T.Text (Either (BackendError err) fromJson))))
                -> Eff es (Event t (Either T.Text (Either (BackendError err) fromJson)))
    runRequest' req = fmap switchDyn $ prerender @t @space (pure never) $ getPostBuildE >>= req
    performWithPb = performJSONRequestResponseAnnotatedWithHeaders @api @r @t @(ClientEs t space es) @cfg route mempty . withPb
-}

-- | MTL-styled 'runAPIPostBuild'. Fires a JSON request once on
-- post-build (client-only, via 'prerender') and returns the decoded
-- response fanned into (error, success) events.
--
-- Semantics mirror the parked Eff version above: 'prerender' guards
-- the request to the client, 'getPostBuild' triggers it once, the
-- caller's @withPb@ shapes the payload from the post-build event, and
-- the response is decoded with route-annotated error messages via
-- 'decodeXhrAnnotate'.
runAPIPostBuild
  :: forall be fe t m cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson
     , Typeable fromJson, Typeable err
     , MonadReader cfg m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , RDC.DomBuilder t m, RDC.Prerender t m, RDC.MonadHold t m
     , RDC.PostBuild t (RDC.Client m)
     , RDC.PerformEvent t (RDC.Client m), RDC.TriggerEvent t (RDC.Client m)
     , MonadIO (RDC.Performable (RDC.Client m))
     , MonadJSM (RDC.Performable (RDC.Client m))
     , Reflex t
     )
  => R be
  -> (Event t () -> Event t toJson)
  -> m (Event t (RequestError err), Event t fromJson)
runAPIPostBuild route withPb = do
  (enc :: FullRouteEncoder be fe) <- asksM
  let routeStr = renderObeliskBackendRoute enc route
  evDyn <- RDC.prerender (pure never) $ do
    pb <- RDC.getPostBuild
    let evPayload = withPb pb
    evXhr <- RDC.performRequestAsync $ withCred . void . postJson routeStr <$> evPayload
    pure $ decodeXhrAnnotate @(Either (BackendError err) fromJson) routeStr <$> evXhr
  pure $ fanResponse' $ switchDyn evDyn

runAPIResponseGatedE
  :: forall api r t space es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es
     , HasPrerender t space es
     , DomBuilderE t space :> es
     , space ~ GhcjsDomSpace
        , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , Configs :> ClientEs t space es
     , Reader cfg :> ClientEs t space es
     , PerformEventH t :> ClientEs t space es
     , PerformEventM (RH.HostFrame t) t :> ClientEs t space es
     , TriggerEvent t :> ClientEs t space es
     , PerformableForRow (ClientEs t space es) ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow (ClientEs t space es))
     , JSM' :> ClientEs t space es
     , IOE :> ClientEs t space es
     , PerformEventH t :> es, TriggerEvent t :> es, JSM' :> es, IOE :> es
     , Hold t :> es, Sample t :> es
     , Reflex t
     )
  => r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIResponseGatedE route evPayload = do
  -- Note: mdo requires MonadFix which effectful supports
  shouldFire <- holdDyn True $ leftmost [ False <$ evPayload ]
  (err, res) <- runAPI @api @r @t @space @es @cfg route $ gate (current shouldFire) evPayload
  -- Re-enable after response
  _shouldFire' <- holdDyn True $ leftmost [ False <$ evPayload, True <$ res, True <$ err ]
  pure (err, res)

-- | MTL twin of 'runAPIResponseGatedE'. Gates re-firing of the request
-- so that only one request is in flight at a time: once a payload
-- fires, subsequent payload events are suppressed until the server
-- responds (either success or error), at which point the gate re-opens.
--
-- Body mirrors the Eff version shape: 'holdDyn' tracks whether the
-- next payload should pass, 'gate' filters the payload Event by the
-- current gate value, the request is performed inside 'prerender'
-- (client-only) via 'performRequestAsync' + 'decodeXhrAnnotate', and
-- the response is fanned into (error, success) via 'fanResponse''.
runAPIResponseGated
  :: forall be fe t m cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson
     , Typeable fromJson, Typeable err
     , MonadReader cfg m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , RDC.DomBuilder t m, RDC.Prerender t m, RDC.MonadHold t m
     , RDC.PerformEvent t (RDC.Client m), RDC.TriggerEvent t (RDC.Client m)
     , MonadIO (RDC.Performable (RDC.Client m))
     , MonadJSM (RDC.Performable (RDC.Client m))
     , Reflex t
     )
  => R be
  -> Event t toJson
  -> m (Event t (RequestError err), Event t fromJson)
runAPIResponseGated route evPayload = do
  (enc :: FullRouteEncoder be fe) <- asksM
  let routeStr = renderObeliskBackendRoute enc route
  evDyn <- RDC.prerender (pure (never, never)) $ do
    -- Close the gate when a payload fires; re-open it only after the
    -- server responds. Mirrors the Eff body's two holdDyn stanzas.
    shouldFire <- holdDyn True $ leftmost [ False <$ evPayload ]
    let evGated = gate (current shouldFire) evPayload
    evXhr <- RDC.performRequestAsync $ withCred . void . postJson routeStr <$> evGated
    let evDecoded = decodeXhrAnnotate @(Either (BackendError err) fromJson) routeStr <$> evXhr
    let (errEv, resEv) = fanResponse' evDecoded
    _reopened <- holdDyn True $ leftmost
      [ False <$ evPayload
      , True  <$ resEv
      , True  <$ errEv
      ]
    pure (errEv, resEv)
  let errEvOut = switchDyn (fst <$> evDyn)
      resEvOut = switchDyn (snd <$> evDyn)
  pure (errEvOut, resEvOut)

-- | MTL twin of the Eff 'runAPIWithHeaders'. Fires a JSON POST with
-- caller-supplied extra headers per input event, decodes the response
-- with route-annotated errors via 'decodeXhrAnnotate', and fans the
-- decoded 'Either' into @(error, success)@ events via 'fanResponse''.
runAPIWithHeadersMTL
  :: forall be fe t m cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson
     , Typeable fromJson, Typeable err
     , MonadReader cfg m
     , HasConfig cfg (FullRouteEncoder be fe)
     , HasConfig cfg BaseURL
     , RDC.DomBuilder t m, RDC.Prerender t m, RDC.MonadHold t m
     , RDC.PerformEvent t (RDC.Client m), RDC.TriggerEvent t (RDC.Client m)
     , MonadIO (RDC.Performable (RDC.Client m))
     , MonadJSM (RDC.Performable (RDC.Client m))
     , Reflex t
     )
  => R be
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> m (Event t (RequestError err), Event t fromJson)
runAPIWithHeadersMTL route headers evPayload = do
  (enc :: FullRouteEncoder be fe) <- asksM
  let routeStr = renderObeliskBackendRoute enc route
  evDyn <- RDC.prerender (pure (never, never)) $ do
    evXhr <- RDC.performRequestAsync $
      withHeaders headers . withCred . void . postJson routeStr <$> evPayload
    let evDecoded = decodeXhrAnnotate @(Either (BackendError err) fromJson) routeStr <$> evXhr
    let (errEv, resEv) = fanResponse' evDecoded
    pure (errEv, resEv)
  let errEvOut = switchDyn (fst <$> evDyn)
      resEvOut = switchDyn (snd <$> evDyn)
  pure (errEvOut, resEvOut)

performJSONRequestResponseAnnotatedWithHeaders
  :: forall api r t es cfg toJson fromJson.
     ( Configs :> es
     , PerformEventH t :> es
     , PerformEventM (RH.HostFrame t) t :> es
     , PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow es)
     , TriggerEvent t :> es
     , JSM' :> es
     , IOE :> es
     , ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     
     )
  => r
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> Eff es (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotatedWithHeaders route headers jsonEv = do
  evXhrResponse <- performJSONRequestWithHeaders @api @r @t @es @cfg route headers jsonEv
  let routeLink = Link (encodeRoute @api route)
  pure $ leftmost
    [ decodeXhrAnnotate (getLink routeLink) <$> evXhrResponse
    ]

-- | Plain (non-Annotated) Effectful counterpart to the MTL @performJSONRequestResponse@.
--   Runs a JSON request, decodes the response, and returns @Either ErrorRead fromJson@
--   (where 'ErrorRead' is a plain 'String' — same shape as the legacy MTL variant).
--
--   Prefer 'performJSONRequestResponseAnnotatedWithHeaders' for new code — the annotated
--   variant produces richer error messages that include the route and the expected type.
--   This variant exists to make direct MTL→Effectful ports possible without changing the
--   decoded error shape at the call site.
performJSONRequestResponseE
  :: forall api r t es cfg toJson fromJson.
     ( Configs :> es
     , PerformEventH t :> es
     , PerformEventM (RH.HostFrame t) t :> es
     , PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow es)
     , TriggerEvent t :> es
     , JSM' :> es
     , IOE :> es
     , ToJSON toJson
     , FromJSON fromJson
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     
     )
  => r
  -> Event t toJson
  -> Eff es (Event t (Either ErrorRead fromJson))
performJSONRequestResponseE route jsonEv = do
  evXhrResponse <- performJSONRequest @api @r @t @es @cfg route jsonEv
  pure $ decodeXhrResponse' <$> evXhrResponse

-- | MTL twin of 'performJSONRequestResponseE'. Runs a JSON request against
-- the given route and decodes the response into @Either ErrorRead fromJson@
-- (same shape as the legacy MTL variant). The caller is expected to wrap
-- the whole computation in 'runRequest' (which handles 'prerender') and
-- 'runReaderT' (which supplies the config).
--
-- Mirrors the Eff body shape using MTL types: resolve the route via the
-- 'FullRouteEncoder' pulled from the Reader, fire the XHR with
-- 'RDC.performRequestAsync' using 'withCred', then decode via
-- 'decodeXhrResponse''.
performJSONRequestResponse
  :: forall fe be t m cfg toJson fromJson.
     ( ToJSON toJson
     , FromJSON fromJson
     , MonadReader cfg m
     , HasConfig cfg (FullRouteEncoder be fe)
     , RDC.PerformEvent t m
     , RDC.TriggerEvent t m
     , MonadIO (RDC.Performable m)
     , MonadJSM (RDC.Performable m)
     , Reflex t
     )
  => R be
  -> Event t toJson
  -> m (Event t (Either ErrorRead fromJson))
performJSONRequestResponse route jsonEv = do
  (enc :: FullRouteEncoder be fe) <- asksM
  let routeStr = renderObeliskBackendRoute enc route
  evXhrResponse <- RDC.performRequestAsync $ withCred . void . postJson routeStr <$> jsonEv
  pure $ decodeXhrResponse' <$> evXhrResponse

decodeXhrAnnotate
  :: forall fromJson.
  ( Typeable fromJson
  , FromJSON fromJson
  ) => T.Text -> XhrResponse -> Either T.Text fromJson
decodeXhrAnnotate routeString xhr =
  let
    annotate e = annotateError
      routeString
      (fromMaybe "" $ _xhrResponse_responseText xhr)
      (T.pack . show $ typeRep (Proxy :: Proxy fromJson))
      e
    mapLeft :: (a -> b) -> Either a c -> Either b c
    mapLeft = flip bimap Prelude.id
  in
    mapLeft annotate . decodeXhrResponse' $ xhr

annotateError :: T.Text -> T.Text -> T.Text -> String -> T.Text
annotateError routeStr actualBody expectedType baseError =
  "Request Error from: {"
  <> routeStr
  <> "} Expected("
  <> expectedType
  <> ") but Received("
  <> determineActual actualBody
  <> ")"
  <> "During base error:"
  <> T.pack baseError
  where
    determineActual jsonString =
      case Aeson.decode (LBS.fromStrict . T.encodeUtf8 $ jsonString) :: Maybe Aeson.Value of
        Nothing -> jsonString
        Just val_ -> case val_ of
          Array _  -> "Array"
          String _ -> "String"
          Number _ -> "Number"
          Bool _   -> "Bool"
          Null     -> "Null"
          Aeson.Object keyMap_ -> case Aeson.lookup (Aeson.fromString "Right") keyMap_ of
            Nothing -> T.pack . show $ keyMap_
            Just rightCase -> case rightCase of
              Aeson.Object keyMap__ ->
                let
                  parser :: Stream s Identity Char => Parsec s u String
                  parser = char '_' *> some alphaNum <* char '_'
                  titlize s_ = T.toUpper (T.take 1 s_) <> (T.drop 1 s_)
                in
                  case Aeson.keys keyMap__ of
                    [] -> T.pack . show $ keyMap__
                    (k:_) -> case parse parser "" (Aeson.toString k) of
                      Left _ -> "An Object With Key:" <> Aeson.toText k
                      Right parent -> titlize . T.pack $ parent
              _ -> T.pack . show $ rightCase

showType :: forall a. Typeable a => a -> String
showType _ = show $ typeRep (Proxy :: Proxy a)

performJSONRequestWithHeaders
  :: forall api r t es cfg json.
     ( PerformEventH t :> es, JSM' :> es, IOE :> es
     , PerformEventM (RH.HostFrame t) t :> es
     , PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow es)
     , TriggerEvent t :> es
     , ToJSON json
     , Configs :> es
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     )
  => r
  -> Map.Map T.Text T.Text
  -> Event t json
  -> Eff es (Event t XhrResponse)
performJSONRequestWithHeaders route headers jsonEv = do
  let routeText = Link (encodeRoute @api route)
  performRequestAsync $ withHeaders headers . withCred . void . postJson (getLink routeText) <$> jsonEv

performJSONRequest
  :: forall api r t es cfg json.
     ( PerformEventH t :> es, JSM' :> es, IOE :> es
     , PerformEventM (RH.HostFrame t) t :> es
     , PerformableForRow es ~ RH.HostFrame t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadJSM (PerformableForRow es)
     , TriggerEvent t :> es
     , ToJSON json
     , Configs :> es
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     )
  => r
  -> Event t json
  -> Eff es (Event t XhrResponse)
performJSONRequest route jsonEv = do
  let routeText = Link (encodeRoute @api route)
  performRequestAsync $ withCred <$> void . postJson (getLink routeText) <$> jsonEv

withCred :: XhrRequest a -> XhrRequest a
withCred xhr = xhr & xhrRequest_config . xhrRequestConfig_withCredentials .~ True

withHeaders :: Map.Map T.Text T.Text -> XhrRequest a -> XhrRequest a
withHeaders headers xhr = xhr & xhrRequest_config . xhrRequestConfig_headers .~ headers

toEith :: Maybe a -> Either T.Text a
toEith = \case { Just a -> Right a ; Nothing -> Left "unable to parse response, please report this error" }

-- | Effectful variant of 'runRequest'. Guards an Eff-level request to the
-- client via 'prerender' and switches on the resulting Dynamic of Events.
runRequestE :: forall t space es r.
              ( HasPrerender t space es
              , DomBuilderE t space :> es
              , Reflex t
              )
           => Eff (ClientEs t space es) (Event t r) -> Eff es (Event t r)
runRequestE req = fmap switchDyn $ prerender @t @space (pure never) req

-- | MTL twin of 'runRequestE'. Guards an MTL-level request to the client
-- via 'RDC.prerender' and switches on the resulting Dynamic of Events.
-- Mirrors the Eff body shape using the reflex-dom-core MTL interface:
-- 'prerender' returns @Dynamic t (Event t r)@, which 'switchDyn' flattens.
runRequest
  :: ( Reflex t
     , RDC.Prerender t m
     , Applicative m
     )
  => RDC.Client m (Event t r)
  -> m (Event t r)
runRequest req = fmap switchDyn $ RDC.prerender (pure never) req

type ApiBE e a = Either (BackendError e) a

fanResponse
  :: Reflex t
  => Event t (Either ErrorRead (Either (BackendError e) a))
  -> (Event t ErrorRead, Event t (BackendError e), Event t a)
fanResponse res =
  let
    (errRead, apiResult) = fanEither res
    (errApi, good) = fanEither apiResult
  in (errRead, errApi, good)

fanResponse'
  :: Reflex t
  => Event t (Either T.Text (Either (BackendError e) a))
  -> (Event t (RequestError e), Event t a)
fanResponse' res =
  let
    (errRead, apiResult) = fanEither res
    (errApi, good) = fanEither apiResult
    err = leftmost [ Request_ErrorAPI <$> errApi
                   , Request_ErrorRead <$> errRead
                   ]
  in (err, good)
