{-# LANGUAGE OverloadedStrings #-}

module Jenga.Frontend.DomExtras where

import Control.Lens ((%~), (.~), (&))
import Jenga.Route.Frontend (RouteToUrl, SetRoute, askRouteToUrl)
import Reflex (Reflex, Event, Dynamic, current, tag, updated, leftmost, ffor)
import Reflex.Dom.Core (GhcjsDomSpace, Element, EventResult, AttributeName(..)
                       , elementConfig_initialAttributes, elementConfig_eventSpec
                       , addEventSpecFlags, EventName(Click), preventDefault, domEvent
                       , ElementConfig(..)
                       , (=:), _element_raw)
import qualified GHCJS.DOM.Element as GElement
import Control.Monad (when)
import Data.Default (def)
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Reflex.Effectful.DomHost (element)
import Reflex.Effectful.DomHost (DomBuilderE)
import Reflex.Effectful.DomHost (DomBuilderSpaceE)
import Reflex.Host.HeadlessEffectful (PostBuild, getPostBuildE)
import Reflex.Host.HeadlessEffectful (PerformEventH, performEventH_)
import Reflex.Effectful.DomHost (JSM')

elDynHtmlAttr_
  :: ( DomBuilderE t (DomBuilderSpaceE es) :> es, DomBuilderSpaceE es ~ GhcjsDomSpace
     , PostBuild t :> es
     , PerformEventH t :> es
     , JSM' :> es
     , IOE :> es
     , Reflex t
     )
  => T.Text
  -> Map.Map T.Text T.Text
  -> Dynamic t T.Text
  -> Eff es (Element EventResult GhcjsDomSpace t)
elDynHtmlAttr_ elementTag attrs_ html = do
  let cfg = def & elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs_
  (e, _) <- element elementTag cfg $ pure ()
  postBuild <- getPostBuildE
  performEventH_ $ ffor (leftmost [updated html, tag (current html) postBuild]) $ \h ->
    -- performEventH_ expects Event t (JSM ()); GElement.setInnerHTML is MonadJSM
    -- so it already unifies with JSM () — no lift needed.
    GElement.setInnerHTML (_element_raw e) h
  return e

routeLinkImpl
  :: forall t es a route.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, DomBuilderSpaceE es ~ GhcjsDomSpace
     , RouteToUrl route :> es
     , SetRoute t route :> es
     , Reflex t
     )
  => Map.Map AttributeName T.Text
  -> route
  -> Eff es a
  -> Eff es (Event t (), a)
routeLinkImpl attrs route wrappedChild = do
  enc <- askRouteToUrl
  let
    targetBlank = Map.lookup "target" attrs == Just "_blank"
    cfg = (def :: ElementConfig EventResult t GhcjsDomSpace)
        & elementConfig_initialAttributes .~ ("href" =: enc route <> attrs)
        & (if targetBlank
           then id
           else elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy GhcjsDomSpace) Click (const preventDefault))
  (e, a) <- element "a" cfg wrappedChild
  when targetBlank $ pure ()
  return (domEvent Click e, a)
