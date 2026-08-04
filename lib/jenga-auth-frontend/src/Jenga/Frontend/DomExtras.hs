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

import Effectful (Eff, (:>))
import Reflex.Effectful.Types (KnownTimeline)
import Reflex.Effectful.Effect.Dom (Dom, element)
import Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)
import Reflex.Effectful.Effect.PerformEvent (PerformEvent, performEvent_)
import Reflex.Effectful.Effect.JSM (JSM')

elDynHtmlAttr_
  :: ( Dom t :> es
     , PostBuild t :> es
     , PerformEvent t :> es
     , JSM' :> es
     , Reflex t
     , KnownTimeline es t
     )
  => T.Text
  -> Map.Map T.Text T.Text
  -> Dynamic t T.Text
  -> Eff es (Element EventResult GhcjsDomSpace t)
elDynHtmlAttr_ elementTag attrs_ html = do
  let cfg = def & elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs_
  (e, _) <- element elementTag cfg $ pure ()
  postBuild <- getPostBuild
  performEvent_ $ ffor (leftmost [updated html, tag (current html) postBuild]) $ \h ->
    -- performEvent_ expects Event t (JSM ()); GElement.setInnerHTML is MonadJSM
    -- so it already unifies with JSM () — no lift needed.
    GElement.setInnerHTML (_element_raw e) h
  return e

routeLinkImpl
  :: forall t es a route.
     ( Dom t :> es
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
