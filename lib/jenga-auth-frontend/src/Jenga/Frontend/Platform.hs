{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.Platform where

import Jenga.Common.Auth
import Jenga.Common.HasJengaConfig
import Language.Javascript.JSaddle
import Network.URI as URI
import Control.Lens ((^.))
import Data.Maybe
import Data.Text as T hiding (show)
import Data.Text.Encoding as T

import Effectful (Eff, (:>))
import Jenga.Frontend.Effectful.Configs (Configs, getConfig)
import qualified Jenga.Configs.MTL as MTL
import Data.ByteString (ByteString)

getDomain :: URI -> Maybe T.Text
getDomain baseUri_ =
  (T.pack . URI.uriRegName)
  <$> URI.uriAuthority baseUri_

newtype RouteFilePath = RouteFilePath { getRouteFile :: T.Text }

-- | Choose the configuration key used for backend websocket routing.
chooseJengaRouteFile :: MTL.HasConfigs m => m RouteFilePath
chooseJengaRouteFile = chooseJengaRouteFileWith MTL.getConfig

-- | Effectful counterpart of 'chooseJengaRouteFile'.
chooseJengaRouteFileE :: Configs :> es => Eff es RouteFilePath
chooseJengaRouteFileE = chooseJengaRouteFileWith getConfig

chooseJengaRouteFileWith :: Monad m => (T.Text -> m (Maybe ByteString)) -> m RouteFilePath
chooseJengaRouteFileWith readConfig = fmap RouteFilePath $ do
  route_ <- readConfig "common/route"
  case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "common/route"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        readConfig "common/ngrokRoute" >>= \case
          Nothing -> pure "common/route"
          Just _ -> pure "common/ngrokRoute"
#else
      Just "localhost" -> pure "common/route"
#endif
      _ -> pure "common/route"

-- | Read the frontend base URL using the platform's route configuration.
readJengaBaseURL :: MTL.HasConfigs m => m (Maybe BaseURL)
readJengaBaseURL = readJengaBaseURLWith MTL.getConfig

-- | Effectful counterpart of 'readJengaBaseURL'.
readJengaBaseURLE :: Configs :> es => Eff es (Maybe BaseURL)
readJengaBaseURLE = readJengaBaseURLWith getConfig

readJengaBaseURLWith :: Monad m => (T.Text -> m (Maybe ByteString)) -> m (Maybe BaseURL)
readJengaBaseURLWith readConfig = do
  route_ <- readConfig "common/route"
  uriString <- case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "https://acetalent.io"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        readConfig "common/ngrokRoute" >>= \case
          Nothing -> pure . T.pack $ show uri_
          Just url -> pure . T.strip . T.decodeUtf8 $ url
#else
      Just "localhost" -> pure . T.pack $ show uri_
#endif
      _ -> pure . T.pack $ show uri_
  pure $ fmap BaseURL . parseURI . T.unpack $ uriString

isNotMobile :: JSM Bool
isNotMobile = do
  width_ <- fromJSVal =<< (jsg (s "document")) ^. js (s "documentElement") ^. js (s "clientWidth")
  return (fromMaybe 1000 width_ > (640 :: Int))
  where
    s :: String -> String
    s = id

clientType :: ClientType
clientType =
#ifdef android_HOST_OS
  Mobile
#else
  Web
#endif
