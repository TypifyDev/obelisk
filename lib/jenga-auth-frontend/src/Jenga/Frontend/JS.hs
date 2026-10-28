{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Jenga.Frontend.JS where

import Jenga.Common.HasJengaConfig
import Jenga.Route.Servant (HasRoute (..))
import Reflex (Reflex, Event, Dynamic, never)
import qualified Reflex
import qualified Reflex.Dom.Core as RDC
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
import Effectful.Reader.Static (Reader)
import Reflex.Class (headE)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)
import Reflex.Dynamic (foldDyn)
import Reflex.Host.HeadlessEffectful (PostBuild, getPostBuildE)
import Reflex.Host.HeadlessEffectful (TriggerEvent, newTriggerEventE)
import Reflex.Host.HeadlessEffectful (PerformEventH, performEventH)
import Reflex.Effectful.DomHost (el, text, dynText, blank)
-- Prerender import removed: use prerender fn
import Reflex.Effectful.DomHost (HasPrerender, ClientEs)
import Reflex.Effectful.DomHost (prerender)
import Reflex.Effectful.DomHost (prerender_)
import Reflex.Effectful.DomHost (DomBuilderE)
import Reflex.Effectful.DomHost (DomBuilderSpaceE)
import Reflex.Host.HeadlessEffectful (NotReady, Adjustable)
import Reflex.Dom.Builder.Class (DomSpace)
import Reflex.Effectful.DomHost (DomBuilderTimelineE)
import Reflex.Effectful.DomHost (JSM', liftJSM)

-- prerender_ imported from Reflex.Effectful.DomHost; client arg lives in ClientEs t space es

runJS :: forall es t space a. (JSM' :> es, HasPrerender t space es, Reflex t)
      => JSM a
      -> Eff es (Dynamic t (Either String a))
runJS jsStatement = prerender @t @space (pure $ Left "Couldn't get JS value from its context") (Right <$> liftJSM jsStatement)

runJSWhen :: forall t space es a b.
             ( HasPrerender t space es, PerformEventH t :> es
             , PerformEventH t :> ClientEs t space es
             , Reflex t
             )
           => Event t a
           -> (a -> JSM b)
           -> Eff es (Event t b)
runJSWhen event jsFunc = do
  -- Client branch runs in ClientEs t space es; lift JSM into that row via liftJSM.
  d <- prerender @t @space (pure never) $ performEventH $ (liftJSM . jsFunc) <$> event
  -- prerender returned Dynamic t (Event t b); collapse with Reflex's switchDyn.
  pure (Reflex.switchDyn d)

-- MTL counterpart of runJSWhen; browser effects only run in the client.
runJSWhenMTL :: (Reflex t, RDC.Prerender t m, Applicative m)
             => Event t a -> (a -> JSM b) -> m (Event t b)
runJSWhenMTL event jsFunc = fmap Reflex.switchDyn $
  RDC.prerender (pure never) $ RDC.performEvent $ (JS.liftJSM . jsFunc) <$> event

runJSWhenPb :: forall t space es b. ( HasPrerender t space es, PostBuild t :> es, PerformEventH t :> es
               , PerformEventH t :> ClientEs t space es
               , TriggerEvent t :> es, Hold t :> es, Sample t :> es, IOE :> es, Reflex t
               )
            => JSM b -> Eff es (Event t b)
runJSWhenPb jsStatement = do
  pBuild <- getPostBuildE
  output <- runJSWhen @t @space pBuild $ \_ -> jsStatement
  headE output

runJSWhenRendered :: forall t space es b. ( HasPrerender t space es, PostBuild t :> es, PerformEventH t :> es
                     , PerformEventH t :> ClientEs t space es
                     , TriggerEvent t :> es, Hold t :> es, Sample t :> es, IOE :> es, Reflex t
                     , DomBuilderE t space :> es)
  => JSM b -> Eff es (Event t b)
runJSWhenRendered jsStatement = do
  pBuild <- onRender @t @space
  output <- runJSWhen @t @space pBuild $ \_ -> jsStatement
  headE output

onRender :: forall t space es. (HasPrerender t space es, DomBuilderE t space :> es, Reflex t) => Eff es (Event t ())
onRender = do
  d <- prerender @t @space blank blank
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
clogSend _proxy route a = do
  clog a
  let logRoute = Link (encodeRoute @api route)
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

addHTMLEventListener  :: forall t space es a.
  ( TriggerEvent t :> es, HasPrerender t space es, JSM' :> es, DomBuilderE t space :> es, Reflex t
      )
  => HTMLRef -> HTMLEventType -> (HTMLEvent -> JSM a) -> Eff es (Event t a)
addHTMLEventListener htmlRef htmlEventType actn = do
  (event, trigger) <- newTriggerEventE
  prerender_ @t @space blank $ liftJSM $ void $ htmlRef ^. js2 ("addEventListener" :: JSString) htmlEventType
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

type Trace t space es = (DomBuilderE t space :> es, NotReady t :> es, Adjustable t :> es, DomSpace space, space ~ DomBuilderSpaceE es, t ~ DomBuilderTimelineE es, Hold t :> es, Sample t :> es, PostBuild t :> es, Reflex t)

traceDyn' :: (Trace t space es, Show a) => Dynamic t a -> Eff es ()
traceDyn' d = traceEvent' (T.pack "traceDyn") (Reflex.updated d)

traceEvent' :: (Trace t space es, Show a) => T.Text -> Event t a -> Eff es ()
traceEvent' label ev = do
  el (T.pack "div") $ do
    el (T.pack "div") $ text label
    vals <- foldDyn (\a_ b_ -> a_ : b_) [] ev
    el (T.pack "div") $ dynText $ T.pack . show <$> vals
