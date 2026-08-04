{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Jenga.Frontend.JS where

import Jenga.Common.HasJengaConfig
import Jenga.Route (HasRoute)
import Reflex (Reflex, Event, Dynamic, never)
import qualified Reflex
import Reflex.Dom.Core (postJson, newXMLHttpRequestWithError
                       , XhrException, _xhrResponse_responseText)
import Language.Javascript.JSaddle hiding (liftJSM)
import qualified Language.Javascript.JSaddle as JS
import Control.Monad.IO.Class (liftIO)
import qualified GHCJS.DOM.Types as GHCJS
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Proxy (Proxy)
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Hold (Hold, foldDyn, headE)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.TriggerEvent (TriggerEvent, newTriggerEvent)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent)
import Reflex.Effectful.Effect.Dom (Dom, el, text, dynText, blank)
import Reflex.Effectful.Effect.Prerender (Prerender, prerender)
import Reflex.Effectful.Effect.JSM (JSM', liftJSM)

-- Local shim: prerender_ is not in Reflex.Effectful.Effect.Prerender (only prerender). Runs the same on both sides, discards the result.
prerender_ :: (Prerender t :> es, Reflex t, KnownTimeline es t) => Eff es () -> Eff es () -> Eff es ()
prerender_ server client = do
  _ <- prerender server client
  pure ()

runJS :: (JSM' :> es, Prerender t :> es, Reflex t, KnownTimeline es t)
      => JSM a
      -> Eff es (Dynamic t (Either String a))
runJS jsStatement = prerender (pure $ Left "Couldn't get JS value from its context") (Right <$> liftJSM jsStatement)

runJSWhen :: (Prerender t :> es, PerformEvent t :> es, Reflex t, KnownTimeline es t)
           => Event t a
           -> (a -> JSM b)
           -> Eff es (Event t b)
runJSWhen event jsFunc = do
  -- performEvent expects an Event t (JSM a); jsFunc already returns JSM b,
  -- so map directly without further lifting.
  d <- prerender (pure never) $ performEvent $ jsFunc <$> event
  -- prerender returned Dynamic t (Event t b); collapse with Reflex's switchDyn.
  pure (Reflex.switchDyn d)

runJSWhenPb :: ( Prerender t :> es, PostBuild t :> es, PerformEvent t :> es
               , TriggerEvent t :> es, Hold t :> es, IOE :> es, Reflex t
               , KnownTimeline es t)
            => JSM b -> Eff es (Event t b)
runJSWhenPb jsStatement = do
  pBuild <- getPostBuild
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

runJSWhenRendered :: ( Prerender t :> es, PostBuild t :> es, PerformEvent t :> es
                     , TriggerEvent t :> es, Hold t :> es, IOE :> es, Reflex t
                     , Dom t :> es, KnownTimeline es t)
  => JSM b -> Eff es (Event t b)
runJSWhenRendered jsStatement = do
  pBuild <- onRender
  output <- runJSWhen pBuild $ \_ -> jsStatement
  headE output

onRender :: (Prerender t :> es, Dom t :> es, Reflex t, KnownTimeline es t) => Eff es (Event t ())
onRender = do
  d <- prerender blank blank
  pure $ Reflex.updated d

clog :: (JSM' :> es, ToJSVal a) => a -> Eff es ()
clog a = liftJSM $ do
  _ <- jsg ("console" :: JSString) ^. js1 ("log" :: JSString) a
  pure ()

clogErr :: forall e es a. (ToJSVal e, JSM' :> es) => e -> Eff es (Either e a)
clogErr e = liftJSM $ do
  _ <- jsg ("console" :: JSString) ^. js1 ("error" :: JSString) e
  pure $ Left e

clogSend
  :: forall api r cfg es a.
     ( JSM' :> es
     , ToJSVal a
     , HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     )
  => Proxy api
  -> r
  -> a
  -> Eff es ()
clogSend proxy route a = do
  clog a
  logRoute <- renderFullRouteBE @api @r @cfg @es proxy route
  liftJSM $ do
    strA <- valToStr a
    let req = postJson (getLink logRoute) (show strA)
    _ <- newXMLHttpRequestWithError req $ \err ->
      let
        showXhr = \case
          Left e -> show (Left e :: Either XhrException String)
          Right res -> show (Right $ _xhrResponse_responseText res :: Either XhrException (Maybe T.Text))
      in do
        -- Callback runs in JSM (not Eff), so use JSaddle's console.log directly
        -- instead of `clog` (which returns Eff es ()).
        _ <- jsg ("console" :: JSString) ^. js1 ("log" :: JSString) (showXhr err)
        pure ()
    pure ()

type HTMLRef = JSVal
type HTMLEventType = T.Text
type HTMLEvent = JSVal

emptyEvent :: HTMLEvent -> JSM ()
emptyEvent = pure . const ()

addHTMLEventListener
  :: ( TriggerEvent t :> es, Prerender t :> es, JSM' :> es, Dom t :> es, Reflex t
     , KnownTimeline es t )
  => HTMLRef -> HTMLEventType -> (HTMLEvent -> JSM a) -> Eff es (Event t a)
addHTMLEventListener htmlRef htmlEventType actn = do
  (event, trigger) <- newTriggerEvent
  prerender_ blank $ liftJSM $ void $ htmlRef ^. js2 ("addEventListener" :: JSString) htmlEventType
    (fun $ \_ _ e -> case e of
        [] -> pure ()  -- JS callback with no args; nothing to dispatch
        (x:_) -> do
          out <- actn x
          liftIO $ trigger out
    )
  pure event

currentWindowUnchecked :: MonadJSM m => m JSVal
currentWindowUnchecked = JS.liftJSM $ jsg ("window" :: JSString)

currentDocumentUnchecked :: MonadJSM m => m GHCJS.Document
currentDocumentUnchecked = JS.liftJSM $ fmap GHCJS.Document $ jsg ("document" :: JSString)

type Trace t es = (Dom t :> es, Hold t :> es, PostBuild t :> es, Reflex t, KnownTimeline es t)

traceDyn' :: (Trace t es, Show a) => Dynamic t a -> Eff es ()
traceDyn' d = traceEvent' (T.pack "traceDyn") (Reflex.updated d)

traceEvent' :: (Trace t es, Show a) => T.Text -> Event t a -> Eff es ()
traceEvent' label ev = do
  el (T.pack "div") $ do
    el (T.pack "div") $ text label
    vals <- foldDyn (\a_ b_ -> a_ : b_) [] ev
    el (T.pack "div") $ dynText $ T.pack . show <$> vals
