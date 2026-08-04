{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | JSM/reflex-m and Effectful dual APIs for jenga-auth-frontend HTTP work.
--
-- The primary functions ('runAPI', 'runAPIPostBuild', etc.) match the
-- pre-Effectful-port reflex-m signatures — @ReaderT cfg m@ — so callers
-- written in traditional reflex-widget style continue to work.
--
-- Effectful (@Eff es@) variants are suffixed @-Eff@ ('runAPIEff',
-- 'runAPIPostBuildEff', etc.) for widgets that live in the Effectful
-- port. Both sets share the same monad-agnostic utilities
-- ('decodeXhrAnnotate', 'withHeaders', 'withCred', 'fanResponse'',
-- 'annotateError').
module Jenga.Frontend.Api where

import Jenga.Common.HasJengaConfig
import Jenga.Common.Errors
import Jenga.Configs
import Jenga.Route
import Jenga.Route (HasRoute)
import Reflex.Dom.Core

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Fix
import Data.Typeable
import Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import Text.Parsec
import Language.Javascript.JSaddle (MonadJSM)
import Control.Applicative (some)
import Data.Bifunctor
import Data.Maybe
import Data.Functor (void)
import Data.Functor.Identity
import Data.Proxy
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Control.Lens ((.~), (&))

-- Effectful imports (for the -Eff variants below)
import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Effectful.Reader.Static ()
import qualified Reflex as R (Event, current, gate, leftmost, never, switchDyn, Reflex, fanEither)
import qualified Reflex.Effectful.Effect.Hold as EHold
import qualified Reflex.Effectful.Effect.PostBuild as EPostBuild
import qualified Reflex.Effectful.Effect.TriggerEvent as ETrigger
import qualified Reflex.Effectful.Effect.PerformEvent as EPerform
import Reflex.Effectful.Effect.Dom (Dom)
import qualified Reflex.Effectful.Effect.Prerender as EPre
import Reflex.Effectful.Effect.JSM ()
import Jenga.Frontend.Effectful.Configs (Configs)

eitherDecodeText :: FromJSON a => T.Text -> Either String a
eitherDecodeText = eitherDecode . LBS.fromStrict . T.encodeUtf8

-- | Convenience function to decode JSON-encoded responses.
decodeXhrResponse' :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponse' = (fromMaybe $ Left "no response text") . fmap eitherDecodeText . _xhrResponse_responseText

-- ─── Constraint synonym for the reflex-m API surface ────────────────
type RunAPI t m =
  ( HasConfigs (Client m)
  , Prerender t m
  , Applicative m
  , DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  )

-- ─── reflex-m primary APIs ──────────────────────────────────────────

-- | Generic Req -> Response function (reflex-m).
runAPI
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON fromJson, Typeable fromJson
     , FromJSON err, Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Event t toJson
  -> ReaderT cfg m (Event t (RequestError err), Event t fromJson)
runAPI route evPayload = runAPIWithHeaders @fe route mempty evPayload

runAPIWithHeaders
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON fromJson, Typeable fromJson
     , FromJSON err, Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> ReaderT cfg m (Event t (RequestError err), Event t fromJson)
runAPIWithHeaders route headers evPayload = do
  fmap fanResponse' $ runRequest $
    performJSONRequestResponseAnnotatedWithHeaders @fe route headers evPayload

-- | Mutually exclusive to when you would use runAPIResponseGated
runAPIPostBuild
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson, Typeable fromJson, Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> (Event t () -> Event t toJson)
  -> ReaderT cfg m (Event t (RequestError err), Event t fromJson)
runAPIPostBuild route withPb = runAPIPostBuildWithHeaders @fe route mempty withPb

runAPIPostBuildWithHeaders
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson, Typeable fromJson, Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , Applicative m
     , DomBuilder t m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> (Event t () -> Event t toJson)
  -> ReaderT cfg m (Event t (RequestError err), Event t fromJson)
runAPIPostBuildWithHeaders route headers withPb = do
  fmap fanResponse' $ runRequest' $ performWithPb
  where
    runRequest' req = fmap switchDyn $ prerender (pure never) $ getPostBuild >>= req
    performWithPb = performJSONRequestResponseAnnotatedWithHeaders @fe route headers . withPb

-- | Mutually exclusive to when you would use runAPIPostBuild
runAPIResponseGated
  :: forall fe backendRoute cfg toJson fromJson t m err.
     ( ToJSON toJson
     , FromJSON err
     , FromJSON fromJson, Typeable fromJson, Typeable err
     , HasConfigs (Client m)
     , Prerender t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Event t toJson
  -> ReaderT cfg m (Event t (RequestError err), Event t fromJson)
runAPIResponseGated route evPayload = mdo
  shouldFire <- holdDyn True $ leftmost [ False <$ evPayload , True <$ res, True <$ err ]
  (err, res) <- runAPI @fe route $ gate (current shouldFire) evPayload
  pure (err, res)

-- | Decode-only variant (reflex-m).
performJSONRequestResponse
  :: forall fe backendRoute cfg toJson fromJson t m.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Event t toJson
  -> ReaderT cfg m (Event t (Either ErrorRead fromJson))
performJSONRequestResponse route jsonEv = do
  (fmap . fmap) decodeXhrResponse' $ performJSONRequest @fe route jsonEv

performJSONRequestResponseAnnotated
  :: forall fe backendRoute cfg t m toJson fromJson.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson, Typeable fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Event t toJson
  -> ReaderT cfg m (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotated route jsonEv =
  performJSONRequestResponseAnnotatedWithHeaders @fe route mempty jsonEv

performJSONRequestResponseAnnotatedWithHeaders
  :: forall fe backendRoute cfg t m toJson fromJson.
     ( HasConfigs m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , TriggerEvent t m
     , ToJSON toJson
     , FromJSON fromJson, Typeable fromJson
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> ReaderT cfg m (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotatedWithHeaders route headers jsonEv = do
  evXhrResponse :: Event t XhrResponse <- performJSONRequestWithHeaders @fe route headers jsonEv
  routeLink <- renderFullRouteBE @fe route
  pure $ leftmost
    [ decodeXhrAnnotate (getLink routeLink) <$> evXhrResponse
    ]

performJSONRequestWithHeaders
  :: forall fe backendRoute json cfg t m.
     ( MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , ToJSON json
     , HasConfigs m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Map.Map T.Text T.Text
  -> Event t json
  -> ReaderT cfg m (Event t XhrResponse)
performJSONRequestWithHeaders route headers jsonEv = do
  routeText <- renderFullRouteBE @fe route
  performRequestAsync $ withHeaders headers . withCred . postJson (getLink routeText) <$> jsonEv

performJSONRequest
  :: forall fe backendRoute json cfg t m.
     ( MonadJSM (Performable m)
     , TriggerEvent t m
     , PerformEvent t m
     , ToJSON json
     , HasConfigs m
     , HasConfig cfg (FullRouteEncoder backendRoute fe)
     , HasConfig cfg BaseURL
     )
  => R backendRoute
  -> Event t json
  -> ReaderT cfg m (Event t XhrResponse)
performJSONRequest route jsonEv = do
  routeText <- renderFullRouteBE @fe route
  performRequestAsync $ withCred <$> postJson (getLink routeText) <$> jsonEv

runRequest :: ( Prerender t m
              , Applicative m
              ) => Client m (Event t r) -> m (Event t r)
runRequest req = fmap switchDyn $ prerender (pure never) ( comment "runRequest:Hydrated" >> req )

-- ─── Monad-agnostic utilities (used by both APIs) ───────────────────

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

withCred :: XhrRequest a -> XhrRequest a
withCred xhr = xhr & xhrRequest_config . xhrRequestConfig_withCredentials .~ True

withHeaders :: Map.Map T.Text T.Text -> XhrRequest a -> XhrRequest a
withHeaders headers xhr = xhr & xhrRequest_config . xhrRequestConfig_headers .~ headers

toEith :: Maybe a -> Either T.Text a
toEith = \case { Just a -> Right a ; Nothing -> Left "unable to parse response, please report this error" }

type ApiBE e a = Either (BackendError e) a

fanResponse
  :: Reflex t
  => Event t (Either ErrorRead (Either (BackendError e) a))
  -> (Event t ErrorRead, Event t (BackendError e), Event t a)
fanResponse res =
  let
    (errRead, apiResult) = R.fanEither res
    (errApi, good) = R.fanEither apiResult
  in (errRead, errApi, good)

fanResponse'
  :: Reflex t
  => Event t (Either T.Text (Either (BackendError e) a))
  -> (Event t (RequestError e), Event t a)
fanResponse' res =
  let
    (errRead, apiResult) = R.fanEither res
    (errApi, good) = R.fanEither apiResult
    err = leftmost [ Request_ErrorAPI <$> errApi
                   , Request_ErrorRead <$> errRead
                   ]
  in
    (err, good)

-- ─── Effectful (Eff es) variants (suffixed @-Eff@) ──────────────────
--
-- Callers written against the Effectful port import these directly. Widget
-- and JSM-based callers use the un-suffixed versions above.

runAPIEff
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es, EPre.Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , EPerform.PerformEvent t :> es, ETrigger.TriggerEvent t :> es, KnownTimeline es t
     , Reflex t
     )
  => Proxy api
  -> r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIEff proxy route evPayload = runAPIWithHeadersEff @api @r @t @es @cfg proxy route mempty evPayload

runAPIWithHeadersEff
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es, EPre.Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , EPerform.PerformEvent t :> es, ETrigger.TriggerEvent t :> es, KnownTimeline es t
     , Reflex t
     )
  => Proxy api
  -> r
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIWithHeadersEff proxy route headers evPayload = do
  fmap fanResponse' $ runRequestEff $
    performJSONRequestResponseAnnotatedWithHeadersEff @api @r @t @es @cfg proxy route headers evPayload

runAPIPostBuildEff
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es, EPre.Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , EPerform.PerformEvent t :> es, ETrigger.TriggerEvent t :> es, KnownTimeline es t
     , EPostBuild.PostBuild t :> es, EHold.Hold t :> es
     , Reflex t
     )
  => Proxy api
  -> r
  -> (Event t () -> Event t toJson)
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIPostBuildEff proxy route withPb = do
  fmap fanResponse' $ runRequest' $ performWithPb
  where
    runRequest' req = fmap R.switchDyn $ EPre.prerender (pure R.never) $ EPostBuild.getPostBuild >>= req
    performWithPb = performJSONRequestResponseAnnotatedWithHeadersEff @api @r @t @es @cfg proxy route mempty . withPb

runAPIResponseGatedEff
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es, EPre.Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , EPerform.PerformEvent t :> es, ETrigger.TriggerEvent t :> es, KnownTimeline es t
     , EHold.Hold t :> es
     , Reflex t
     )
  => Proxy api
  -> r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIResponseGatedEff proxy route evPayload = do
  shouldFire <- EHold.holdDyn True $ R.leftmost [ False <$ evPayload ]
  (err, res) <- runAPIEff @api @r @t @es @cfg proxy route $ R.gate (R.current shouldFire) evPayload
  _shouldFire' <- EHold.holdDyn True $ R.leftmost [ False <$ evPayload, True <$ res, True <$ err ]
  pure (err, res)

performJSONRequestResponseAnnotatedWithHeadersEff
  :: forall api r t es cfg toJson fromJson.
     ( Configs :> es
     , EPerform.PerformEvent t :> es
     , ETrigger.TriggerEvent t :> es
     , ToJSON toJson
     , FromJSON fromJson
     , Typeable fromJson
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     , KnownTimeline es t
     )
  => Proxy api
  -> r
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> Eff es (Event t (Either T.Text fromJson))
performJSONRequestResponseAnnotatedWithHeadersEff proxy route headers jsonEv = do
  evXhrResponse <- performJSONRequestWithHeadersEff @api @r @t @es @cfg proxy route headers jsonEv
  routeLink <- renderFullRouteBE @api @r @cfg @es proxy route
  pure $ R.leftmost
    [ decodeXhrAnnotate (getLink routeLink) <$> evXhrResponse
    ]

performJSONRequestWithHeadersEff
  :: forall api r t es cfg json.
     ( EPerform.PerformEvent t :> es, KnownTimeline es t
     , ETrigger.TriggerEvent t :> es
     , ToJSON json
     , Configs :> es
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     )
  => Proxy api
  -> r
  -> Map.Map T.Text T.Text
  -> Event t json
  -> Eff es (Event t XhrResponse)
performJSONRequestWithHeadersEff proxy route headers jsonEv = do
  routeText <- renderFullRouteBE @api @r @cfg @es proxy route
  EPerform.performRequestAsync $ withHeaders headers . withCred . void . postJson (getLink routeText) <$> jsonEv

performJSONRequestEff
  :: forall api r t es cfg json.
     ( EPerform.PerformEvent t :> es, KnownTimeline es t
     , ETrigger.TriggerEvent t :> es
     , ToJSON json
     , Configs :> es
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reflex t
     )
  => Proxy api
  -> r
  -> Event t json
  -> Eff es (Event t XhrResponse)
performJSONRequestEff proxy route jsonEv = do
  routeText <- renderFullRouteBE @api @r @cfg @es proxy route
  EPerform.performRequestAsync $ withCred <$> void . postJson (getLink routeText) <$> jsonEv

runRequestEff :: (EPre.Prerender t :> es, Reflex t, KnownTimeline es t)
              => Eff es (Event t r) -> Eff es (Event t r)
runRequestEff req = fmap R.switchDyn $ EPre.prerender (pure R.never) req
