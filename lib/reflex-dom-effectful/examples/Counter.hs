{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Default (def)
import           Data.Text (Text, pack)
import qualified Data.Map as Map
import           Data.IORef
import           Reflex (leftmost, updated, ffor)
import           Reflex.Dom.Builder.Class (domEvent, EventName(..))
import           Reflex.Dom.Core (_inputElement_value, _element_raw)
import qualified Language.Javascript.JSaddle as JS
import qualified Language.Javascript.JSaddle.Warp as JW

import           Reflex.Effectful

-- ─── Helpers ───────────────────────────────────────────────────

-- | Log to browser console via JSaddle
consoleLog :: JS.JSString -> JS.JSM ()
consoleLog msg = do
  _ <- JS.jsg ("console" :: Text) JS.# ("log" :: Text) $ [JS.toJSVal msg]
  return ()

-- ─── 1. Counter ────────────────────────────────────────────────

counter :: FRP' es t => Event t () -> Event t () -> Eff es (Dynamic t Int)
counter inc dec = foldDyn (+) 0 (leftmost [1 <$ inc, (-1) <$ dec])

counterWidget :: DomEff' es t => Eff es ()
counterWidget = do
  el "h2" $ text "1. Counter"
  el "div" $ do
    (incEl, _) <- el' "button" $ text "+"
    (decEl, _) <- el' "button" $ text "-"
    val <- counter (domEvent Click incEl) (domEvent Click decEl)
    text " Value: "
    display val

-- ─── 2. Input Echo ─────────────────────────────────────────────

inputEchoWidget :: DomEff' es t => Eff es ()
inputEchoWidget = do
  el "h2" $ text "2. Input Echo"
  el "div" $ do
    inp <- inputElement def
    el "p" $ do
      text "You typed: "
      dynText (_inputElement_value inp)

-- ─── 3. PerformEvent + console.log ────────────────────────────

performWidget :: (DomAccessEff' es t) => Eff es ()
performWidget = do
  el "h2" $ text "3. PerformEvent + console.log"
  el "div" $ do
    (btn, _) <- el' "button" $ text "Click (logs to browser console)"
    let clickEv = domEvent Click btn
    ct <- count clickEv
    -- Log to server terminal
    performEvent_ $ ffor clickEv $ \_ ->
      putStrLn "PerformEvent: clicked! (server)"
    -- Log to browser console via JSaddle
    requestDomAction_ $ ffor clickEv $ \_ ->
      consoleLog "PerformEvent: clicked! (browser console)"
    el "p" $ do
      text "Clicks: "
      display ct

-- ─── 4. newTriggerEvent (async timer) ──────────────────────────

asyncTimerWidget :: forall es t. InteractiveEff' es t => Eff es ()
asyncTimerWidget = do
  el "h2" $ text "4. Async Timer (newTriggerEvent)"
  el "div" $ do
    (tickEv, fireTick) <- newTriggerEvent
    pb <- getPostBuild
    performEvent_ $ ffor pb $ \_ -> do
      _ <- forkIO $ do
        ref <- newIORef (0 :: Int)
        let loop = do
              threadDelay 2000000
              n <- atomicModifyIORef' ref (\x -> (x+1, x+1))
              fireTick (pack $ "tick #" <> show n)
              loop
        loop
      return ()
    msg <- holdDyn "waiting for first tick..." tickEv
    el "p" $ dynText msg

-- ─── 5. newTriggerEvent (manual) ───────────────────────────────

manualTriggerWidget :: InteractiveEff' es t => Eff es ()
manualTriggerWidget = do
  el "h2" $ text "5. Manual Trigger (newTriggerEvent)"
  el "div" $ do
    (msgEv, fireFn) <- newTriggerEvent
    (btn, _) <- el' "button" $ text "Fire trigger"
    performEvent_ $ ffor (domEvent Click btn) $ \_ ->
      fireFn ("fired!" :: Text)
    msgs <- foldDyn (\new acc -> acc <> " | " <> new) "" msgEv
    el "p" $ do
      text "Messages: "
      dynText msgs

-- ─── 6. Widget Swap ────────────────────────────────────────────

swapWidget :: WidgetEff' es t => Eff es ()
swapWidget = do
  el "h2" $ text "6. Widget Swap"
  el "div" $ do
    (swapEl, _) <- el' "button" $ text "Swap"
    showA <- toggle True (domEvent Click swapEl)
    let widgetEv = ffor (updated showA) $ \b ->
          if b then el "p" $ text "Widget A"
               else el "p" $ text "Widget B"
    widgetHold_ (el "p" $ text "Widget A") widgetEv

-- ─── 7. Nested Dynamic ────────────────────────────────────────

nestedDynWidget :: WidgetEff' es t => Eff es ()
nestedDynWidget = do
  el "h2" $ text "7. Nested Dynamic"
  el "div" $ do
    (outerBtn, _) <- el' "button" $ text "Outer swap"
    outerToggle <- toggle True (domEvent Click outerBtn)
    let outerEv = ffor (updated outerToggle) $ \b ->
          if b
          then do
            el "p" $ text "Outer A"
            (innerBtn, _) <- el' "button" $ text "Inner swap"
            innerToggle <- toggle True (domEvent Click innerBtn)
            widgetHold_ (el "span" $ text " [inner X]") $
              ffor (updated innerToggle) $ \b' ->
                el "span" $ text (if b' then " [inner X]" else " [inner Y]")
          else
            el "p" $ text "Outer B (no inner)"
    widgetHold_ (el "p" $ text "Outer A") outerEv

-- ─── 8. WebRTC Camera ──────────────────────────────────────────

cameraWidget :: DomAccessEff' es t => Eff es ()
cameraWidget = do
  el "h2" $ text "8. WebRTC Camera"
  el "div" $ do
    elAttr "video"
      (Map.fromList [("id", "cam"), ("autoplay", ""), ("playsinline", ""), ("width", "320")])
      blank
    pb <- getPostBuild
    requestDomAction_ $ ffor pb $ \_ -> do
      _ <- JS.eval
        ( "navigator.mediaDevices.getUserMedia({video:true})"
        <> ".then(function(s){document.getElementById('cam').srcObject=s})"
        <> ".catch(function(e){console.log('Camera error:',e)})"
        :: Text)
      return ()
    el "p" $ text "(allow camera access when prompted)"

-- ─── App ───────────────────────────────────────────────────────

app :: WidgetEff' es t => Eff es ()
app = do
  el "div" $ do
    el "h1" $ text "reflex-dom-effectful test suite"
    el "hr" blank
    counterWidget
    el "hr" blank
    inputEchoWidget
    el "hr" blank
    performWidget
    el "hr" blank
    asyncTimerWidget
    el "hr" blank
    manualTriggerWidget
    el "hr" blank
    swapWidget
    el "hr" blank
    nestedDynWidget
    el "hr" blank
    cameraWidget

main :: IO ()
main = do
  putStrLn "Running on http://localhost:3003"
  JW.run 3003 $ mainWidgetEff app
