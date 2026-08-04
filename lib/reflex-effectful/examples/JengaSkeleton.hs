{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example: what a Jenga frontend looks like with Effectful.
--
-- Compare with the MTL version in skeleton/frontend/src/Frontend.hs.
module JengaSkeleton where

import           Jenga.Frontend.Effectful

-- Simulated route type (would come from Common.Route in a real project)
data FrontendRoute a where
  FrontendRoute_Main          :: FrontendRoute ()
  FrontendRoute_Login         :: FrontendRoute ()
  FrontendRoute_Signup        :: FrontendRoute ()
  FrontendRoute_ResetPassword :: FrontendRoute ()

-- ─── Before (MTL) ──────────────────────────────────────────────
--
-- frontendHead :: JengaWidget t route m => RoutedT t route m ()
-- frontendHead = do
--   el "title" $ text "Jenga App"
--
-- frontendBody :: JengaWidget t (R FrontendRoute) m => RoutedT t (R FrontendRoute) m ()
-- frontendBody = subRoute_ $ \case
--   FrontendRoute_Main -> do
--     el "h1" $ text "Jenga App"
--     el "p" $ text "Edit frontend/src/Frontend.hs to get started."
--   FrontendRoute_Login -> do
--     el "h1" $ text "Login"
--
-- ─── After (Effectful) ─────────────────────────────────────────

-- | Head widget — just sets the title.
-- Notice: no RoutedT wrapper, no @t@ in function body.
frontendHead :: DomEff' es t => Eff es ()
frontendHead = do
  el "title" $ text "Jenga App"

-- | Body widget — routes on FrontendRoute.
-- The constraint says exactly what this widget can do:
-- build DOM, hold state, read route, set route.
frontendBody :: JengaWidget' es t (FrontendRoute ()) => Eff es ()
frontendBody = do
  route <- askRoute
  -- For now, just display the route (full subRoute_ integration needs R type)
  el "div" $ do
    el "h1" $ text "Jenga App (Effectful)"
    el "p" $ text "Edit frontend/src/Frontend.hs to get started."

    -- Example: reading config
    mApiUrl <- getTextConfig "common/api-url"
    case mApiUrl of
      Nothing -> el "p" $ text "No API URL configured"
      Just url -> el "p" $ do
        text "API: "
        dynText (constDyn url)

    -- Example: using route-to-url
    toUrl <- askRouteToUrl
    el "p" $ text ("Current URL builder available: " <> toUrl (FrontendRoute_Main))

-- Helpers that would come from reflex
constDyn :: Reflex t => a -> Dynamic t a
constDyn = pure
