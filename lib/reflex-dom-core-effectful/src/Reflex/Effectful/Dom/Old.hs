{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Effectful port of @Reflex.Dom.Old@ — the reflex-dom legacy compat
-- layer of widget helpers that predate the current 'DomBuilder' API.
--
-- Every helper's MTL-shape @MonadWidget t m@ constraint is replaced by
-- the effect-row bundle 'MonadWidgetE'; every @m a@ return type becomes
-- @Eff es a@. The old @WidgetHost m = Performable m@ alias dies with
-- @Performable m@ — perform-time actions run in the same 'Eff' row now.
--
-- API-shape changes from upstream:
--
--   * @MonadWidget t m@ (class) → @MonadWidgetE t es@ (type
--     synonym constraint bundle).
--   * Every polymorphic @DomBuilderSpace m@ in a signature becomes
--     @DomBuilderSpace es@ (resolved via the type family exported from
--     'Reflex.Dom.Core').
--   * @Attributes m attrs t@ (class) → @Attributes attrs t@ (class,
--     minus the m parameter). The method now takes / returns 'Eff es'
--     with the ambient 'MonadWidgetE' constraint.
--   * The old @WidgetHost m@ alias is deleted along with @Performable m@.
--     @addVoidAction@ now takes a plain @Eff es ()@ effect action.
module Reflex.Effectful.Dom.Old
  ( MonadWidgetE
  , WidgetHost
  , El
  , ElConfig (..)
  , elConfig_namespace
  , elConfig_attributes
  , _el_clicked
  , _el_element
  , _el_events
  , addVoidAction
  , AttributeMap
  , Attributes (..)
  , buildElement
  , buildElementNS
  , buildEmptyElement
  , buildEmptyElementNS
  , elDynHtml'
  , elDynHtmlAttr'
  , elStopPropagationNS
  , elWith
  , elWith'
  , emptyElWith
  , emptyElWith'
  , namedNodeMapGetNames
  , nodeClear
  , onEventName
  , schedulePostBuild
  , text'
  , unsafePlaceElement
  , wrapElement
  ) where

import Control.Arrow (first)
import Control.Lens (Lens, Lens', (%~), (&), (.~), (^.))
import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.Default
import Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.EventM (EventM)
import GHCJS.DOM.NamedNodeMap as NNM
import GHCJS.DOM.Node (getFirstChild, getNodeName, removeChild)
import GHCJS.DOM.Types (JSM, IsHTMLElement, IsNode, runJSM)
import qualified GHCJS.DOM.Types as DOM

import Effectful (Eff, IOE, (:>))

import Reflex.Class
import Reflex.Class (Hold)
import Reflex.Class (PerformEvent, performEvent_)
import Reflex.Class (PostBuild, getPostBuild)
import Reflex.Class (Sample)
import Reflex.Class (TriggerEvent)
import Reflex.Dom.Core
import Reflex.Dom.Core
import Reflex.Class (Global)
import Reflex.Dom.Core

-- ---------------------------------------------------------------------
-- ElConfig — attribute+namespace bundle used by 'elWith' etc.
-- ---------------------------------------------------------------------

data ElConfig attrs = ElConfig
  { _elConfig_namespace :: Maybe Text
  , _elConfig_attributes :: attrs
  }

instance attrs ~ Map Text Text => Default (ElConfig attrs) where
  def = ElConfig
    { _elConfig_namespace = Nothing
    , _elConfig_attributes = mempty
    }

elConfig_namespace :: Lens' (ElConfig attrs1) (Maybe Text)
elConfig_namespace f (ElConfig a b) = (\a' -> ElConfig a' b) <$> f a
{-# INLINE elConfig_namespace #-}

elConfig_attributes :: Lens (ElConfig attrs1) (ElConfig attrs2) attrs1 attrs2
elConfig_attributes f (ElConfig a b) = (\b' -> ElConfig a b') <$> f b
{-# INLINE elConfig_attributes #-}

-- ---------------------------------------------------------------------
-- The MonadWidgetE constraint bundle (effect-row replacement for
-- MTL MonadWidget)
-- ---------------------------------------------------------------------

-- | Effect-row constraint bundle a widget needs to talk to Reflex, DOM,
-- and JS. Direct 1:1 mapping from upstream's 13-constraint
-- @MonadWidgetConstraints t m@ bundle. Since perform-time actions run in
-- the same 'Eff' row now (no split @Performable m@), the split
-- @MonadJSM (Performable m)@ / @MonadJSM m@ upstream constraints
-- collapse into a single @JSM' :> es@ row entry (the port uses one
-- effect row, not two). The other split @Performable m@ constraints
-- (@Ref (Performable m) ~ IORef@ etc.) collapse into 'IOE'. Both
-- 'IOE :> es' and 'JSM' :> es' are needed here — the widget row
-- performs both raw IO ('newIORef', 'forkIO') and JS-side work
-- ('liftJSM', 'runJSM' against the ambient 'JSContextRef').
type MonadWidgetE t es =
  ( t ~ DomBuilderTimeline es
  , Reflex t
  
  , DomBuilder t (DomBuilderSpace es) :> es
  , DomBuilderSpace es ~ GhcjsDomSpace
  , Hold t :> es
  , Sample t :> es
  , PostBuild t :> es
  , PerformEvent t :> es
  , TriggerEvent t :> es
  , HasDocument (DomBuilderSpace es) :> es
  , IOE :> es
  , JSM' :> es
  )

-- ---------------------------------------------------------------------
-- Type aliases
-- ---------------------------------------------------------------------

-- | Upstream compatibility alias for @Reflex.Dom.Old.WidgetHost@
-- (@type WidgetHost m = Performable m@).
--
-- Upstream distinguished the "widget monad" @m@ from its perform-time
-- callback monad @Performable m@ — @performEvent :: Event t (Performable m a) -> m (Event t a)@
-- ran its @Performable m a@ argument in a potentially-different monad.
-- The effect-row port has one row: @performEvent@ callbacks execute in
-- the same @Eff es@ scope as the caller, so @Performable m == m@ and
-- @WidgetHost m == m@ collapse to the identity alias.
--
-- Restored so downstream code that referenced @WidgetHost m@ (e.g.
-- @Event t (WidgetHost m ())@) continues to type-check when migrated to
-- @Eff es@.
type WidgetHost es = es

type El = Element EventResult GhcjsDomSpace

type AttributeMap = Map Text Text

-- ---------------------------------------------------------------------
-- addVoidAction / schedulePostBuild
-- ---------------------------------------------------------------------

-- | Perform an 'Eff' action every time an 'Event' fires; ignore the
-- return value. Replaces upstream's @addVoidAction :: Event t (WidgetHost m ()) -> m ()@ —
-- @WidgetHost m@ dies with @Performable m@.
addVoidAction
  :: (Reflex t, PerformEvent t :> es)
  => Event t (EventME Global ())
  -> Eff es ()
addVoidAction = performEvent_

-- | Perform an IO action once, at post-build time.
schedulePostBuild
  :: forall t es
   . (Reflex t, PostBuild t :> es, PerformEvent t :> es)
  => EventME Global ()
  -> Eff es ()
schedulePostBuild w = do
  postBuild <- getPostBuild
  performEvent_ (w <$ postBuild)

-- ---------------------------------------------------------------------
-- Element builders (compat surface over the current DomBuilder API)
-- ---------------------------------------------------------------------

buildElement
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> attrs -> Eff es a -> Eff es (RawElement (DomBuilderSpace es), a)
buildElement = buildElementNS Nothing

buildEmptyElement
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> attrs -> Eff es (RawElement (DomBuilderSpace es))
buildEmptyElement elementTag attrs =
  fst <$> buildElementNS Nothing elementTag attrs blank

buildEmptyElementNS
  :: (MonadWidgetE t es, Attributes attrs t)
  => Maybe Text -> Text -> attrs -> Eff es (RawElement (DomBuilderSpace es))
buildEmptyElementNS ns elementTag attrs =
  fst <$> buildElementNS ns elementTag attrs blank

buildElementNS
  :: (MonadWidgetE t es, Attributes attrs t)
  => Maybe Text -> Text -> attrs -> Eff es a
  -> Eff es (RawElement (DomBuilderSpace es), a)
buildElementNS ns elementTag attrs child =
  first _element_raw <$> buildElementInternal ns elementTag attrs child

-- | Class specifying how a given attribute-shape gets wired into an
-- 'ElementConfig'. Two instances: static @Map Text Text@ and dynamic
-- @Dynamic t (Map Text Text)@.
--
-- Note the drop of the @m@ parameter compared to upstream: the ambient
-- @MonadWidgetE t es@ constraint on 'buildElementInternal' pins
-- the effect row, so the class only needs to key on @attrs@ + @t@.
class Attributes attrs t where
  buildElementInternal
    :: (MonadWidgetE t es)
    => Maybe Text -> Text -> attrs -> Eff es a
    -> Eff es (Element EventResult (DomBuilderSpace es) t, a)

instance Attributes (Map Text Text) t where
  buildElementInternal ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ ns
    buildElementCommon elementTag child =<< addStaticAttributes attrs cfg

addStaticAttributes
  :: Applicative f
  => Map Text Text
  -> ElementConfig er t space
  -> f (ElementConfig er t space)
addStaticAttributes attrs cfg = do
  let initialAttrs = Map.fromList $ first (AttributeName Nothing) <$> Map.toList attrs
  pure $ cfg & elementConfig_initialAttributes .~ initialAttrs

instance Attributes (Dynamic t (Map Text Text)) t where
  buildElementInternal ns elementTag attrs child = do
    let cfg = def & elementConfig_namespace .~ ns
    buildElementCommon elementTag child =<< addDynamicAttributes attrs cfg

addDynamicAttributes
  :: (Reflex t, PostBuild t :> es)
  => Dynamic t (Map Text Text)
  -> ElementConfig er t (DomBuilderSpace es)
  -> Eff es (ElementConfig er t (DomBuilderSpace es))
addDynamicAttributes attrs cfg = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  pure $ cfg & elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs

buildElementCommon
  :: (MonadWidgetE t es)
  => Text -> Eff es a -> ElementConfig er t (DomBuilderSpace es)
  -> Eff es (Element er (DomBuilderSpace es) t, a)
buildElementCommon elementTag child cfg = element elementTag cfg child

-- ---------------------------------------------------------------------
-- Event-name / text-node helpers
-- ---------------------------------------------------------------------

onEventName :: IsHTMLElement e => EventName en -> e -> EventM e (EventType en) () -> JSM (JSM ())
onEventName = elementOnEventName

text'
  :: forall t es
   . (MonadWidgetE t es)
  => Text -> Eff es DOM.Text
text' s = _textNode_raw <$> textNode ((def :: TextNodeConfig t) & textNodeConfig_initialContents .~ s)

instance HasAttributes (ElConfig attrs) where
  type Attrs (ElConfig attrs) = attrs
  attributes = elConfig_attributes

instance HasNamespace (ElConfig attrs) where
  namespace = elConfig_namespace

elWith
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> ElConfig attrs -> Eff es a -> Eff es a
elWith elementTag cfg child = snd <$> elWith' elementTag cfg child

elWith'
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> ElConfig attrs -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpace es) t, a)
elWith' elementTag cfg =
  buildElementInternal (cfg ^. namespace) elementTag (cfg ^. attributes)

emptyElWith
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> ElConfig attrs -> Eff es ()
emptyElWith elementTag cfg = void $ emptyElWith' elementTag cfg

emptyElWith'
  :: (MonadWidgetE t es, Attributes attrs t)
  => Text -> ElConfig attrs -> Eff es (Element EventResult (DomBuilderSpace es) t)
emptyElWith' elementTag cfg = fst <$> elWith' elementTag cfg (pure ())

-- ---------------------------------------------------------------------
-- Deprecated accessors (kept for source-compat with old callers)
-- ---------------------------------------------------------------------

{-# DEPRECATED _el_clicked "Use 'domEvent Click' instead" #-}
_el_clicked :: Reflex t => Element EventResult d t -> Event t ()
_el_clicked = domEvent Click

{-# DEPRECATED _el_element "Use '_element_raw' instead" #-}
_el_element :: El t -> RawElement GhcjsDomSpace
_el_element = _element_raw

{-# DEPRECATED _el_events "Use '_element_events' instead; or, if possible, use 'domEvent' instead to retrieve a particular event" #-}
_el_events :: Element er d t -> EventSelector t (WrapArg er EventName)
_el_events = _element_events

{-# DEPRECATED _el_keypress "Use 'domEvent Keypress' instead" #-}
_el_keypress :: Reflex t => El t -> Event t Word
_el_keypress = domEvent Keypress

{-# DEPRECATED _el_scrolled "Use 'domEvent Scroll' instead" #-}
_el_scrolled :: Reflex t => El t -> Event t Double
_el_scrolled = domEvent Scroll

-- ---------------------------------------------------------------------
-- wrapElement / unsafePlaceElement
-- ---------------------------------------------------------------------

wrapElement
  :: forall t es
   . (MonadWidgetE t es)
  => (forall en. DOM.HTMLElement -> EventName en -> EventM DOM.Element (EventType en) (Maybe (EventResult en)))
  -> DOM.HTMLElement -> Eff es (El t)
wrapElement eh e = do
  let h :: (EventName en, GhcjsDomEvent en) -> JSM (Maybe (EventResult en))
      h (en, GhcjsDomEvent evt) = runReaderT (eh e en) evt
  wrapRawElement (DOM.toElement e) $ (def :: RawElementConfig EventResult t GhcjsDomSpace)
    { _rawElementConfig_eventSpec = def
        { _ghcjsEventSpec_handler = GhcjsEventHandler h
        }
    }

unsafePlaceElement
  :: (MonadWidgetE t es)
  => DOM.HTMLElement -> Eff es (Element EventResult (DomBuilderSpace es) t)
unsafePlaceElement e = do
  placeRawElement $ DOM.toElement e
  wrapRawElement (DOM.toElement e) def

-- ---------------------------------------------------------------------
-- DOM utility (JSM, no Reflex involvement)
-- ---------------------------------------------------------------------

namedNodeMapGetNames :: DOM.NamedNodeMap -> JSM (Set Text)
namedNodeMapGetNames self = do
  l <- NNM.getLength self
  Set.fromList <$> forM (take (fromIntegral l) [0..]) (
    NNM.itemUnchecked self >=> getNodeName)

nodeClear :: IsNode self => self -> JSM ()
nodeClear n = do
  mfc <- getFirstChild n
  case mfc of
    Nothing -> pure ()
    Just fc -> do
      _ <- removeChild n fc
      nodeClear n

-- ---------------------------------------------------------------------
-- Namespace-aware element helpers
-- ---------------------------------------------------------------------

elStopPropagationNS
  :: forall t es en a
   . (MonadWidgetE t es)
  => Maybe Text -> Text -> EventName en -> Eff es a -> Eff es a
elStopPropagationNS ns elementTag en child = do
  let f = GhcjsEventFilter $ \_ -> pure (stopPropagation, pure Nothing)
      cfg = (def :: ElementConfig EventResult t (DomBuilderSpace es))
        & namespace .~ ns
        & elementConfig_eventSpec . ghcjsEventSpec_filters %~ DMap.insert en f
  snd <$> element elementTag cfg child

elDynHtmlAttr'
  :: (MonadWidgetE t es)
  => Text -> Map Text Text -> Dynamic t Text
  -> Eff es (Element EventResult (DomBuilderSpace es) t)
elDynHtmlAttr' elementTag attrs html = do
  let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs
  (e, _) <- element elementTag cfg (pure ())
  postBuild <- getPostBuild
  ctx <- askJSM
  let ev = leftmost [updated html, tag (current html) postBuild]
  performEvent_ $ (\t -> EventME (runJSM (Element.setInnerHTML (_element_raw e) t) ctx)) <$> ev
  pure e

elDynHtml'
  :: (MonadWidgetE t es)
  => Text -> Dynamic t Text -> Eff es (Element EventResult (DomBuilderSpace es) t)
elDynHtml' elementTag = elDynHtmlAttr' elementTag mempty
