{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
module Frontend where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor ((<$))
import Data.Default (def)
import Data.Proxy (Proxy(..))
import Reflex (Reflex, leftmost, ffor)
import Reflex.Dom.Builder.Class (domEvent, EventName(Click))
import Reflex.Dom.Xhr (xhrRequest, _xhrResponse_responseText)

#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
import Servant.API ((:<|>)(..))
import Servant.Client.JSaddle (BaseUrl(..), Scheme(..), client, mkClientEnv, runClientM)
import Common.Route (SkeletonApi)
#endif

import Jenga.Frontend (Frontend(..), FrontendWidget)
import Jenga.Route.Frontend
import Reflex.Effectful

import qualified Grid.Effect.Eff as G
import Frontend.Grid (dashboardGrid)

import Classh
import Classh.Reflex.Eff (gridCol, col, centerSimple)

import Common.Route

frontend :: Frontend FrontendPages FrontendRoute
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

frontendHead :: FrontendWidget es t FrontendRoute => Eff es ()
frontendHead = el "title" $ text "Jenga + GridEff + ClasshSS"

frontendBody :: FrontendWidget es t FrontendRoute => Eff es ()
frontendBody = elClass "div" "max-w-7xl mx-auto p-5" $ do
  navbar
  switchRoute_ $ \case
    FrontendRoute_Main -> dashboardPage
    FrontendRoute_Login -> loginPage
    FrontendRoute_Signup -> signupPage
    _ -> dashboardPage

-- Navbar: box classh' for layout, text classh' for text styling
navbar :: FrontendWidget es t FrontendRoute => Eff es ()
navbar = elClass "nav" $(classh' [ py .~~ TWSize 4, bw_b .~~ B1, bc .~~ color (Gray C800), mb .~~ twSize' 8 ]) $ do
  elClass "span" $(classh' [ text_size .~~ XL, text_weight .~~ Bold, text_color .~~ color (Sky C400) ]) $ text "Jenga"
  elClass "div" "flex gap-4 ml-8" $ do
    navBtn FrontendRoute_Main "Dashboard"
    navBtn FrontendRoute_Login "Login"
    navBtn FrontendRoute_Signup "Sign Up"

navBtn :: FrontendWidget es t FrontendRoute => FrontendRoute -> Text -> Eff es ()
navBtn route label = do
  (e, _) <- elClass' "a" $(classh' [ cursor .~~ CursorPointer, px .~~ TWSize 4, py .~~ TWSize 2, br .~~ R_Lg ]) $
    elClass "span" $(classh' [ text_color .~~ color (Gray C400) ]) $ text label
  setRoute (route <$ domEvent Click e)

-- Dashboard with GridEff budget tracking
dashboardPage :: FrontendWidget es t FrontendRoute => Eff es ()
dashboardPage = do
  elClass "div" $(classh' [ py .~~ TWSize 8, pos .~~ centered ]) $ do
    elClass "h1" $(classh' [ text_size .~~ XL3, text_weight .~~ Bold ]) $ text "Type-Safe Grid Layout"
    elClass "p" $(classh' [ text_color .~~ color (Gray C500), text_size .~~ SM ]) $ text "Column budgets enforced at compile time"

  gridCol Col12 $ G.runGridEff_exact $ dashboardGrid
    sidebarW mainW
    (cardW "Counter" counterW) (cardW "API Call" apiW) (cardW "Toggle" toggleW)
    (infoW "Effectful" "All effects type-tracked.") (infoW "GridEff" "Overflow = type error.")
    (elClass "div" $(classh' [ py .~~ TWSize 4, pos .~~ centered ]) $
      elClass "span" $(classh' [ text_color .~~ color (Gray C600), text_size .~~ XS ]) $
        text "Jenga + Effectful + GridEff + ClasshSS")

-- Box styling with classh', text styling with separate classh'
sidebarW :: FrontendWidget es t FrontendRoute => Eff es ()
sidebarW = col [4] $ elClass "div" $(classh' [ bgColor .~~ solidColor (Gray C800), br .~~ R_Xl, p .~~ TWSize 5, bw .~~ B1, bc .~~ color (Gray C700) ]) $ do
  elClass "h2" $(classh' [ text_color .~~ color (Sky C400), text_weight .~~ Semibold ]) $ text "Navigation"
  mapM_ (\t -> elClass "div" $(classh' [ py .~~ TWSize 2, px .~~ TWSize 3, br .~~ R_Md, cursor .~~ CursorPointer ]) $
    elClass "span" $(classh' [ text_color .~~ color (Gray C400) ]) $ text t)
    ["Dashboard", "Analytics", "Settings", "Users"]

mainW :: FrontendWidget es t FrontendRoute => Eff es ()
mainW = col [8] $ elClass "div" $(classh' [ bgColor .~~ solidColor (Gray C800), br .~~ R_Xl, p .~~ TWSize 6, bw .~~ B1, bc .~~ color (Gray C700) ]) $ do
  elClass "h1" $(classh' [ text_size .~~ XL2, text_weight .~~ Bold ]) $ text "Welcome to Jenga"
  elClass "p" $(classh' [ text_color .~~ color (Gray C400) ]) $ text "Compile-time grid budget tracking."

cardW :: FrontendWidget es t FrontendRoute => Text -> Eff es () -> Eff es ()
cardW title w = col [4] $ elClass "div" $(classh' [ bgColor .~~ solidColor (Gray C800), br .~~ R_Xl, p .~~ TWSize 5, bw .~~ B1, bc .~~ color (Gray C700) ]) $ do
  elClass "h2" $(classh' [ text_color .~~ color (Sky C400), text_weight .~~ Semibold ]) $ text title
  w

infoW :: FrontendWidget es t FrontendRoute => Text -> Text -> Eff es ()
infoW title desc = col [6] $ elClass "div" $(classh' [ bgColor .~~ solidColor (Gray C800), br .~~ R_Xl, p .~~ TWSize 5, bw .~~ B1, bc .~~ color (Gray C700) ]) $ do
  el "div" $ blank
  elClass "h2" $(classh' [ text_color .~~ color (Sky C400), text_weight .~~ Semibold ]) $ text title
  elClass "p" $(classh' [ text_color .~~ color (Gray C400), text_size .~~ SM ]) $ text desc

counterW :: FrontendWidget es t FrontendRoute => Eff es ()
counterW = el "div" $ do
  (dec, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Gray C700), px .~~ TWSize 4, py .~~ TWSize 2, br .~~ R_Md, cursor .~~ CursorPointer ]) $ text "-"
  (inc, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Gray C700), px .~~ TWSize 4, py .~~ TWSize 2, br .~~ R_Md, cursor .~~ CursorPointer ]) $ text "+"
  val <- foldDyn (+) (0 :: Int) (leftmost [1 <$ domEvent Click inc, (-1) <$ domEvent Click dec])
  elClass "span" $(classh' [ text_size .~~ XL2, text_weight .~~ Bold, text_color .~~ color (Sky C400) ]) $ display val

apiW :: FrontendWidget es t FrontendRoute => Eff es ()
apiW = do
  (btn, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Blue C600), px .~~ TWSize 4, py .~~ TWSize 2, br .~~ R_Md, cursor .~~ CursorPointer ]) $ text "Fetch API"
#if !defined(javascript_HOST_ARCH) && !defined(wasm32_HOST_ARCH)
  -- Type-safe servant client via servant-jsaddle
  let env = mkClientEnv (BaseUrl Http "localhost" 8000 "api")
      helloC :<|> _ = client (Proxy @SkeletonApi)
  respEv <- performEvent $ ffor (domEvent Click btn) $ \_ -> do
    result <- runClientM helloC env
    pure $ case result of
      Left err -> "Error: " <> T.pack (show err)
      Right val -> T.pack (show val)
#else
  -- Raw XHR fallback on WASM
  xhrEv <- performRequestAsync $ ffor (domEvent Click btn) $ \_ ->
    xhrRequest "GET" "/api/hello" def
  let respEv = ffor xhrEv $ \xhr ->
        maybe "No response" id (_xhrResponse_responseText xhr)
#endif
  response <- holdDyn "Click to call /api/hello" respEv
  elClass "pre" $(classh' [ bgColor .~~ solidColor (Gray C900), p .~~ TWSize 3, br .~~ R_Md, mt .~~ twSize' 2 ]) $
    elClass "span" $(classh' [ text_size .~~ XS, text_color .~~ color (Green C400), text_font .~~ Mono ]) $ dynText response

toggleW :: FrontendWidget es t FrontendRoute => Eff es ()
toggleW = do
  (btn, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Gray C700), px .~~ TWSize 4, py .~~ TWSize 2, br .~~ R_Md, cursor .~~ CursorPointer ]) $ text "Toggle"
  showDetail <- toggle False (domEvent Click btn)
  dyn_ $ ffor showDetail $ \b ->
    elClass "div" $(classh' [ bgColor .~~ solidColor (Gray C900), p .~~ TWSize 3, br .~~ R_Md, mt .~~ twSize' 2 ]) $
      elClass "span" $(classh' [ text_color .~~ color (Gray C400), text_size .~~ SM ]) $
        if b then text "Widget swapped in!" else text "Click to swap."

loginPage :: FrontendWidget es t FrontendRoute => Eff es ()
loginPage = centerSimple $ elClass "div" "max-w-md w-full py-12" $ do
  elClass "h1" $(classh' [ text_size .~~ XL2, text_weight .~~ Bold ]) $ text "Login"
  (btn, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Blue C600), px .~~ TWSize 6, py .~~ TWSize 3, br .~~ R_Lg, cursor .~~ CursorPointer ]) $ text "Sign In"
  authEv <- performEvent $ ffor (domEvent Click btn) $ \_ -> liftIO $ threadDelay 800000 >> pure ("Authenticated!" :: Text)
  status <- holdDyn "" authEv
  elClass "p" $(classh' [ text_color .~~ color (Sky C400) ]) $ dynText status

signupPage :: FrontendWidget es t FrontendRoute => Eff es ()
signupPage = centerSimple $ elClass "div" "max-w-md w-full py-12" $ do
  elClass "h1" $(classh' [ text_size .~~ XL2, text_weight .~~ Bold ]) $ text "Create Account"
  (btn, _) <- elClass' "button" $(classh' [ bgColor .~~ solidColor (Green C600), px .~~ TWSize 6, py .~~ TWSize 3, br .~~ R_Lg, cursor .~~ CursorPointer ]) $ text "Create Account"
  msg <- holdDyn "" =<< performEvent (ffor (domEvent Click btn) $ \_ -> liftIO $ threadDelay 600000 >> pure ("Account created!" :: Text))
  elClass "p" $(classh' [ text_color .~~ color (Green C400) ]) $ dynText msg
