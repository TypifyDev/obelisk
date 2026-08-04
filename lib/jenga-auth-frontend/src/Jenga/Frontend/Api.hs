{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Api where

import Jenga.Common.HasJengaConfig
import Jenga.Common.Errors
import Jenga.Route (HasRoute)
import Reflex (Reflex, Event, current, gate, leftmost, never, switchDyn)
import Reflex.Dom.Core (XhrRequest, XhrResponse, postJson
                       , xhrRequest_config, xhrRequestConfig_withCredentials
                       , xhrRequestConfig_headers, _xhrResponse_responseText)

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
import Data.Proxy ()
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import Control.Lens ((.~), (&))

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Effectful.Reader.Static ()
import Reflex.Effectful.Effect.Hold (Hold, holdDyn)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performRequestAsync)
import Reflex.Effectful.Effect.Dom (Dom)
import Reflex.Effectful.Effect.Prerender (Prerender, prerender)
import Reflex.Effectful.Effect.JSM ()
import Jenga.Frontend.Effectful.Configs (Configs)

eitherDecodeText :: FromJSON a => T.Text -> Either String a
eitherDecodeText = eitherDecode . LBS.fromStrict . T.encodeUtf8

decodeXhrResponse' :: FromJSON a => XhrResponse -> Either String a
decodeXhrResponse' = (fromMaybe $ Left "no response text") . fmap eitherDecodeText . _xhrResponse_responseText

type RunAPI t es =
  ( Configs :> es
  , Prerender t :> es
  , Dom t :> es
  , Hold t :> es
  , PostBuild t :> es
  , Reflex t
  )

runAPI
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es, Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , PerformEvent t :> es, TriggerEvent t :> es, KnownTimeline es t
     , Reflex t
     )
  => Proxy api
  -> r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPI proxy route evPayload = runAPIWithHeaders @api @r @t @es @cfg proxy route mempty evPayload

runAPIWithHeaders
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON fromJson, Typeable fromJson
     , Typeable err, FromJSON err
     , Configs :> es, Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , PerformEvent t :> es, TriggerEvent t :> es, KnownTimeline es t
     , Reflex t
     )
  => Proxy api
  -> r
  -> Map.Map T.Text T.Text
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIWithHeaders proxy route headers evPayload = do
  fmap fanResponse' $ runRequest $
    performJSONRequestResponseAnnotatedWithHeaders @api @r @t @es @cfg proxy route headers evPayload

runAPIPostBuild
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es, Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , PerformEvent t :> es, TriggerEvent t :> es, KnownTimeline es t
     , PostBuild t :> es, Hold t :> es
     , Reflex t
     )
  => Proxy api
  -> r
  -> (Event t () -> Event t toJson)
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIPostBuild proxy route withPb = do
  fmap fanResponse' $ runRequest' $ performWithPb
  where
    runRequest' req = fmap switchDyn $ prerender (pure never) $ getPostBuild >>= req
    performWithPb = performJSONRequestResponseAnnotatedWithHeaders @api @r @t @es @cfg proxy route mempty . withPb

runAPIResponseGated
  :: forall api r t es cfg toJson fromJson err.
     ( ToJSON toJson, FromJSON err, FromJSON fromJson, Typeable fromJson, Typeable err
     , Configs :> es, Prerender t :> es, Dom t :> es
     , HasRoute api r, Reader cfg :> es, HasConfig cfg BaseURL
     , PerformEvent t :> es, TriggerEvent t :> es, KnownTimeline es t
     , Hold t :> es
     , Reflex t
     )
  => Proxy api
  -> r
  -> Event t toJson
  -> Eff es (Event t (RequestError err), Event t fromJson)
runAPIResponseGated proxy route evPayload = do
  -- Note: mdo requires MonadFix which effectful supports
  shouldFire <- holdDyn True $ leftmost [ False <$ evPayload ]
  (err, res) <- runAPI @api @r @t @es @cfg proxy route $ gate (current shouldFire) evPayload
  -- Re-enable after response
  _shouldFire' <- holdDyn True $ leftmost [ False <$ evPayload, True <$ res, True <$ err ]
  pure (err, res)

performJSONRequestResponseAnnotatedWithHeaders
  :: forall api r t es cfg toJson fromJson.
     ( Configs :> es
     , PerformEvent t :> es
     , TriggerEvent t :> es
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
performJSONRequestResponseAnnotatedWithHeaders proxy route headers jsonEv = do
  evXhrResponse <- performJSONRequestWithHeaders @api @r @t @es @cfg proxy route headers jsonEv
  routeLink <- renderFullRouteBE @api @r @cfg @es proxy route
  pure $ leftmost
    [ decodeXhrAnnotate (getLink routeLink) <$> evXhrResponse
    ]

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
     ( PerformEvent t :> es, KnownTimeline es t
     , TriggerEvent t :> es
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
performJSONRequestWithHeaders proxy route headers jsonEv = do
  routeText <- renderFullRouteBE @api @r @cfg @es proxy route
  performRequestAsync $ withHeaders headers . withCred . void . postJson (getLink routeText) <$> jsonEv

performJSONRequest
  :: forall api r t es cfg json.
     ( PerformEvent t :> es, KnownTimeline es t
     , TriggerEvent t :> es
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
performJSONRequest proxy route jsonEv = do
  routeText <- renderFullRouteBE @api @r @cfg @es proxy route
  performRequestAsync $ withCred <$> void . postJson (getLink routeText) <$> jsonEv

withCred :: XhrRequest a -> XhrRequest a
withCred xhr = xhr & xhrRequest_config . xhrRequestConfig_withCredentials .~ True

withHeaders :: Map.Map T.Text T.Text -> XhrRequest a -> XhrRequest a
withHeaders headers xhr = xhr & xhrRequest_config . xhrRequestConfig_headers .~ headers

toEith :: Maybe a -> Either T.Text a
toEith = \case { Just a -> Right a ; Nothing -> Left "unable to parse response, please report this error" }

runRequest :: (Prerender t :> es, Reflex t, KnownTimeline es t)
           => Eff es (Event t r) -> Eff es (Event t r)
runRequest req = fmap switchDyn $ prerender (pure never) req

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
  where fanEither = undefined -- TODO: from Reflex

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
  where fanEither = undefined -- TODO: from Reflex
