{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Scroll where

import Jenga.Frontend.JS
import Jenga.Frontend.DomExtras
import Jenga.Route.Frontend (RouteToUrl, SetRoute)
import Reflex (Reflex, Event, ffor)
import Language.Javascript.JSaddle hiding (liftJSM)
import qualified GHCJS.DOM.Types as DOM
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.Effect.Dom (Dom, blank)
import Reflex.Effectful.Effect.PostBuild (PostBuild)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import Reflex.Effectful.Effect.Prerender (Prerender, prerender)
import Reflex.Effectful.Effect.JSM (JSM', liftJSM)
import Reflex.Effectful.Types (KnownTimeline)

routeLinkHash
  :: forall t es a route.
     ( Dom t :> es
     , RouteToUrl route :> es
     , SetRoute t route :> es
     , Prerender t :> es
     , PostBuild t :> es
     , PerformEvent t :> es
     , JSM' :> es
     , Reflex t
     , KnownTimeline es t
     )
  => route
  -> Maybe T.Text
  -> T.Text
  -> Eff es a
  -> Eff es a
routeLinkHash route mFragContainer frag wrappedChild = do
  (_e, a) <- routeLinkImpl @t @es @a @route mempty route wrappedChild
  scrollToFrag mFragContainer frag _e
  return a

scrollToTop :: (JSM' :> es) => Eff es ()
scrollToTop = liftJSM $ void $ jsg ("window" :: JSString) ^. js2 ("scrollTo" :: JSString) (0 :: Int) (0 :: Int)

scrollElem :: (JSM' :> es) => JSVal -> (Float,Float) -> Eff es ()
scrollElem e (x_,y_) = liftJSM $ void $ e ^. js1 ("scrollBy" :: JSString) opts
  where opts = do
          o <- create
          o ^. jss ("top" :: JSString) y_
          o ^. jss ("left" :: JSString) x_
          o ^. jss ("behavior" :: JSString) ("smooth" :: JSString)
          pure o

getDimensions :: (JSM' :> es) => T.Text -> Eff es (Float, Float)
getDimensions idTag = liftJSM $ do
  doc <- jsg ("document" :: JSString)
  idEl <- doc ^. js1 ("getElementById" :: JSString) idTag
  width_ <- fromJSVal =<< idEl ^. js ("clientWidth" :: JSString)
  height_ <- fromJSVal =<< idEl ^. js ("clientHeight" :: JSString)
  clogJSM (width_,height_)
  pure (fromMaybe 500 width_, fromMaybe 500 height_)

-- | Local browser-console log usable inside a JSM block (mirrors
-- 'Jenga.Frontend.JS.clog', which is in Eff and thus unusable here).
clogJSM :: ToJSVal a => a -> JSM ()
clogJSM a = do
  _ <- jsg ("console" :: JSString) ^. js1 ("log" :: JSString) a
  pure ()

scrollToId :: Maybe T.Text -> T.Text -> JSM ()
scrollToId containerId idFrag = do
  w' <- Just <$> currentWindowUnchecked
  doc' <- DOM.unDocument <$> currentDocumentUnchecked
  containerRef <- case containerId of
    Nothing -> pure Nothing
    Just idC -> do
      cont <- doc' ^. js1 ("getElementById" :: JSString) idC
      ghcjsPure (isNull cont) >>= \case
        True -> pure Nothing
        False -> pure $ Just cont
  clogJSM (doc', containerRef)
  case (,,) <$> (containerRef <|> (Just doc')) <*> Just doc' <*> w' of
    Nothing -> pure ()
    Just (container, doc, window) -> do
      eWithId <- doc ^. js1 ("getElementById" :: JSString) idFrag
      clogJSM =<< eWithId ^. js0 ("getBoundingClientRect" :: JSString)
      targetY :: Maybe Int <- fromJSVal =<< (eWithId ^. js0 ("getBoundingClientRect" :: JSString) ^. js ("top" :: JSString))
      containerY :: Maybe Int <- fromJSVal =<< (container ^. js0 ("getBoundingClientRect" :: JSString) ^. js ("top" :: JSString))
      scrollY' :: Maybe Int <- fromJSVal =<< window ^. js ("scrollY" :: JSString)
      containerScrollTop :: Maybe Int <- fromJSVal =<< container ^. js ("scrollTop" :: JSString)
      clogJSM ("scroll top" :: JSString, containerScrollTop)
      case (,,,) <$> targetY <*> containerY <*> containerScrollTop <*> scrollY' of
        Nothing -> pure ()
        Just (tTop, cTop, _cScrollTop, scrollY) -> do
          clogJSM (tTop, cTop, _cScrollTop)
          opts <- do
            o <- create
            o ^. jss ("top" :: JSString) (tTop - (cTop + scrollY))
            o ^. jss ("behavior" :: JSString) ("smooth" :: JSString)
            pure o
          _ <- container ^. js1 ("scrollTo" :: JSString) opts
          clogJSM opts
          clogJSM container
          pure ()

scrollToFrag :: forall es t. (Prerender t :> es, PerformEvent t :> es, Dom t :> es, JSM' :> es, Reflex t, KnownTimeline es t)
  => Maybe T.Text -> T.Text -> Event t () -> Eff es ()
scrollToFrag mContainer fragId e = do
  _ <- prerender blank $ performEvent_ $ ffor e $ \_ -> scrollToId mContainer fragId
  pure ()
