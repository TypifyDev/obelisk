{-# LANGUAGE AllowAmbiguousTypes #-}
module Jenga.Frontend.Scroll where

import Jenga.Frontend.JS
import Jenga.Frontend.DomExtras
import Jenga.Route.Frontend (RouteToUrl, SetRoute)
import Reflex (Reflex, Event, ffor)
import Language.Javascript.JSaddle hiding (liftJSM)
import qualified Language.Javascript.JSaddle as JSaddle (liftJSM)
import qualified GHCJS.DOM.Types as DOM
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text as T

import Effectful (Eff, (:>))
import Reflex.Effectful.DomHost (DomBuilderE, blank)
import Reflex.Dom.Builder.Immediate (GhcjsDomSpace)
import Reflex.Effectful.DomHost (DomBuilderSpaceE)
import Reflex.Host.HeadlessEffectful (PostBuild)
import Reflex.Host.HeadlessEffectful (PerformEventH, performEventH_)
import Reflex.Effectful.DomHost (HasPrerender, prerender, ClientEs)
import Reflex.Effectful.DomHost (JSM', liftJSM)
import qualified Reflex.Dom.Core as RD

routeLinkHash
  :: forall t space es a route.
     ( DomBuilderE t space :> es
     , space ~ DomBuilderSpaceE es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , RouteToUrl route :> es
     , SetRoute t route :> es
     , HasPrerender t space es
     , PostBuild t :> es
     , PerformEventH t :> es
     , JSM' :> es
     , Reflex t
     )
  => route
  -> Maybe T.Text
  -> T.Text
  -> Eff es a
  -> Eff es a
routeLinkHash route mFragContainer frag wrappedChild = do
  (_e, a) <- routeLinkImpl @t @es @a @route mempty route wrappedChild
  scrollToFragE @es @t @space mFragContainer frag _e
  return a

scrollToTopE :: (JSM' :> es) => Eff es ()
scrollToTopE = liftJSM $ void $ jsg ("window" :: JSString) ^. js2 ("scrollTo" :: JSString) (0 :: Int) (0 :: Int)

scrollToTop :: MonadJSM m => m ()
scrollToTop = JSaddle.liftJSM $ void $ jsg ("window" :: JSString) ^. js2 ("scrollTo" :: JSString) (0 :: Int) (0 :: Int)

scrollElemE :: (JSM' :> es) => JSVal -> (Float,Float) -> Eff es ()
scrollElemE e (x_,y_) = liftJSM $ void $ e ^. js1 ("scrollBy" :: JSString) opts
  where opts = do
          o <- create
          o ^. jss ("top" :: JSString) y_
          o ^. jss ("left" :: JSString) x_
          o ^. jss ("behavior" :: JSString) ("smooth" :: JSString)
          pure o

scrollElem :: MonadJSM m => JSVal -> (Float,Float) -> m ()
scrollElem e (x_,y_) = JSaddle.liftJSM $ void $ e ^. js1 ("scrollBy" :: JSString) opts
  where opts = do
          o <- create
          o ^. jss ("top" :: JSString) y_
          o ^. jss ("left" :: JSString) x_
          o ^. jss ("behavior" :: JSString) ("smooth" :: JSString)
          pure o

getDimensions :: MonadJSM m => T.Text -> m (Float, Float)
getDimensions idTag = JSaddle.liftJSM $ do
  doc <- jsg ("document" :: JSString)
  idEl <- doc ^. js1 ("getElementById" :: JSString) idTag
  width_ <- fromJSVal =<< idEl ^. js ("clientWidth" :: JSString)
  height_ <- fromJSVal =<< idEl ^. js ("clientHeight" :: JSString)
  clogJSM (width_,height_)
  pure (fromMaybe 500 width_, fromMaybe 500 height_)

getDimensionsE :: (JSM' :> es) => T.Text -> Eff es (Float, Float)
getDimensionsE idTag = liftJSM $ do
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

scrollToFragE :: forall es t space.
  ( HasPrerender t space es, PerformEventH t :> es
  , PerformEventH t :> ClientEs t space es
  , DomBuilderE t space :> es, JSM' :> es, Reflex t
  )
  => Maybe T.Text -> T.Text -> Event t () -> Eff es ()
scrollToFragE mContainer fragId e = do
  _ <- prerender @t @space blank $ performEventH_ $ ffor e $ \_ -> liftJSM $ scrollToId mContainer fragId
  pure ()

-- | MTL twin of 'scrollToFragE'. Uses reflex-dom's 'RD.prerender_' +
-- 'RD.performEvent_' so it unifies with polymorphic Reflex monad stacks
-- (e.g. 'Client m' inside a 'prerender_').
scrollToFrag :: forall t m.
  ( RD.Reflex t
  , Monad m
  , RD.Prerender t m
  )
  => Maybe T.Text -> T.Text -> RD.Event t () -> m ()
scrollToFrag mContainer fragId e =
  RD.prerender_ RD.blank $ RD.performEvent_ $ ffor e $ \_ ->
    JSaddle.liftJSM $ scrollToId mContainer fragId
