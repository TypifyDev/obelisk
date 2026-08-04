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

getDomain :: URI -> Maybe T.Text
getDomain baseUri_ =
  (T.pack . URI.uriRegName)
  <$> URI.uriAuthority baseUri_

newtype RouteFilePath = RouteFilePath { getRouteFile :: T.Text }

chooseJengaRouteFile :: Configs :> es => Eff es RouteFilePath
chooseJengaRouteFile = fmap RouteFilePath $ do
  route_ <- getConfig "common/route"
  case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "common/route"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        getConfig "common/ngrokRoute" >>= \case
          Nothing -> pure "common/route"
          Just _ -> pure "common/ngrokRoute"
#else
      Just "localhost" -> pure "common/route"
#endif
      _ -> pure "common/route"

readJengaBaseURL :: Configs :> es => Eff es (Maybe BaseURL)
readJengaBaseURL = do
  route_ <- getConfig "common/route"
  uriString <- case parseURI =<< T.unpack . T.strip . T.decodeUtf8 <$> route_ of
    Nothing -> pure "https://acetalent.io"
    Just uri_ -> case getDomain uri_ of
#ifdef android_HOST_OS
      Just "localhost" -> do
        getConfig "common/ngrokRoute" >>= \case
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
