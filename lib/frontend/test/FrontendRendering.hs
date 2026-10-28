{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Jenga.Configs.MTL as Config
import Jenga.Frontend
import qualified Jenga.Route.FrontendMTL as Route
import qualified Reflex.Dom.Core as MTL
import qualified Reflex.Effectful.DomHost as E

main :: IO ()
main = do
  html <- renderFrontendHtml (Map.singleton "frontend/test" "configured")
    [("session", "cookie-value")] ("/" <>) "initial" mtlFrontend (pure ()) (pure ())
  mapM_ (assertContains html)
    [ "<title>MTL head</title>", "configured", "cookie-value"
    , "/initial", "static branch"
    ]
  unless (not $ "client branch" `BS.isInfixOf` html) $ fail "SSR ran the client branch"
  effectHtml <- renderFrontendHtml Map.empty [] id "" effectFrontend (pure ()) (pure ())
  assertContains effectHtml "Effectful body"
  putStrLn "Frontend rendering checks passed (MTL and Effectful)."

assertContains :: BS.ByteString -> BS.ByteString -> IO ()
assertContains html expected = unless (expected `BS.isInfixOf` html) $
  fail $ "Missing rendered content: " <> show expected

mtlFrontend :: Frontend Text
mtlFrontend = FrontendMTL
  (\_ -> MTL.el "title" $ MTL.text "MTL head")
  (\cookies -> do
    config <- Config.getConfig "frontend/test"
    MTL.text $ maybe "missing config" Text.decodeUtf8 config
    MTL.text $ maybe "missing cookie" Text.decodeUtf8 $ lookup "session" cookies
    route <- Route.askRoute
    encode <- Route.askRouteToUrl
    MTL.dynText $ encode <$> route
    MTL.prerender_ (MTL.text "static branch") (MTL.text "client branch"))

effectFrontend :: Frontend Text
effectFrontend = Frontend (E.el "title" $ E.text "Effectful head") (E.text "Effectful body")
