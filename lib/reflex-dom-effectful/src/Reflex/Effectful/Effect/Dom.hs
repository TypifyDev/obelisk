{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reflex.Effectful.Effect.Dom
  ( Dom(..)
  , element, inputElement, textAreaElement, selectElement
  , textNode, commentNode, placeRawElement, wrapRawElement
  , el, el', elAttr, elAttr', elClass, elClass', elDynAttr, elDynAttr'
  , text, dynText, display, blank
  , (=:)
  ) where

import           Data.Default             (Default(def))
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text, pack)

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)

import qualified Reflex                   as R
import qualified Reflex.Dom.Builder.Class as DBC
import           Reflex.Dom.Builder.Class (AttributeName(..), elementConfig_initialAttributes, elementConfig_modifyAttributes)
import           Control.Lens             ((.~), (&))

import           Reflex.Effectful.Types
import           Reflex.Effectful.Effect.PostBuild (PostBuild, getPostBuild)

data Dom t :: Effect where
  Element_
    :: Text
    -> ElementConfig EventResult t GhcjsDomSpace
    -> m a
    -> Dom t m (Element EventResult GhcjsDomSpace t, a)
  TextNode_       :: TextNodeConfig t -> Dom t m (TextNode GhcjsDomSpace t)
  CommentNode_    :: CommentNodeConfig t -> Dom t m (CommentNode GhcjsDomSpace t)
  InputElement_   :: InputElementConfig EventResult t GhcjsDomSpace -> Dom t m (InputElement EventResult GhcjsDomSpace t)
  TextAreaElement_ :: TextAreaElementConfig EventResult t GhcjsDomSpace -> Dom t m (TextAreaElement EventResult GhcjsDomSpace t)
  SelectElement_
    :: SelectElementConfig EventResult t GhcjsDomSpace
    -> m a
    -> Dom t m (SelectElement EventResult GhcjsDomSpace t, a)
  PlaceRawElement_ :: DBC.RawElement GhcjsDomSpace -> Dom t m ()
  WrapRawElement_
    :: DBC.RawElement GhcjsDomSpace
    -> RawElementConfig EventResult t GhcjsDomSpace
    -> Dom t m (Element EventResult GhcjsDomSpace t)
  Text_    :: Text -> Dom t m ()
  DynText_ :: R.Dynamic t Text -> Dom t m ()

type instance DispatchOf (Dom t) = 'Dynamic

-- NOTE: `DomBuilderSpace` is an ASSOCIATED type of the `DomBuilder` class in
-- reflex-dom-core — it CANNOT be defined as a free `type instance` (GHC-06205:
-- "Associated type must be inside a class instance"). A full
-- `DomBuilder t (Eff es)` instance ALSO won't work: the class demands
-- superclasses `Adjustable t m`, `NotReady t m`, `Reflex t`, whose fundeps
-- (`m -> t` on `Adjustable`) cannot be satisfied for `m ~ Eff es` — the
-- timeline `t` is carried inside the effect row `es` (via `Timeline t`), a
-- position GHC's instance resolver cannot syntactically project.
--
-- Resolution: consumers whose records reference `DomBuilderSpace m` under
-- `m ~ Eff es` use the E-suffixed type aliases in `Templates.Types`
-- (`ElE t`, `InputElE t`, `TextAreaElE t`, `SelectElE t`,
-- `TaggedElE t a`, `TaggedOptionsE t a`, `RawSelectE' t a`) which hardcode
-- `GhcjsDomSpace` — the concrete space every `element` / `inputElement` /
-- `textAreaElement` / `selectElement` constructor above returns. Struct
-- records that were `MyStruct t m` with fields typed against `El t m`
-- become an E-sibling `MyStructE t` with fields typed against `ElE t`.
--
-- The previous attempted `type instance DBC.DomBuilderSpace (Eff es) = GhcjsDomSpace`
-- was rejected by GHC-06205; nothing here changes that.

-- ─── Core operations ───────────────────────────────────────────

element :: Dom t :> es => Text -> ElementConfig EventResult t GhcjsDomSpace -> Eff es a -> Eff es (Element EventResult GhcjsDomSpace t, a)
element tag cfg child = send (Element_ tag cfg child)

inputElement :: Dom t :> es => InputElementConfig EventResult t GhcjsDomSpace -> Eff es (InputElement EventResult GhcjsDomSpace t)
inputElement = send . InputElement_

textAreaElement :: Dom t :> es => TextAreaElementConfig EventResult t GhcjsDomSpace -> Eff es (TextAreaElement EventResult GhcjsDomSpace t)
textAreaElement = send . TextAreaElement_

selectElement :: Dom t :> es => SelectElementConfig EventResult t GhcjsDomSpace -> Eff es a -> Eff es (SelectElement EventResult GhcjsDomSpace t, a)
selectElement cfg child = send (SelectElement_ cfg child)

textNode :: Dom t :> es => TextNodeConfig t -> Eff es (TextNode GhcjsDomSpace t)
textNode = send . TextNode_

commentNode :: Dom t :> es => CommentNodeConfig t -> Eff es (CommentNode GhcjsDomSpace t)
commentNode = send . CommentNode_

placeRawElement :: forall t es. Dom t :> es => DBC.RawElement GhcjsDomSpace -> Eff es ()
placeRawElement = send @(Dom t) . PlaceRawElement_

wrapRawElement :: forall t es. Dom t :> es => DBC.RawElement GhcjsDomSpace -> RawElementConfig EventResult t GhcjsDomSpace -> Eff es (Element EventResult GhcjsDomSpace t)
wrapRawElement rawEl cfg = send @(Dom t) (WrapRawElement_ rawEl cfg)

-- ─── Convenience wrappers (t inferred via KnownTimeline fundep) ─

el :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Eff es a -> Eff es a
el tag child = snd <$> el' tag child

el' :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Eff es a -> Eff es (Element EventResult GhcjsDomSpace t, a)
el' tag = element tag def

elAttr :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Map Text Text -> Eff es a -> Eff es a
elAttr tag attrs child = snd <$> elAttr' tag attrs child

elAttr' :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Map Text Text -> Eff es a -> Eff es (Element EventResult GhcjsDomSpace t, a)
elAttr' tag attrs = element tag cfg
  where cfg = def & elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs

elClass :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Text -> Eff es a -> Eff es a
elClass tag c child = snd <$> elClass' tag c child

elClass' :: (KnownTimeline es t, Dom t :> es, R.Reflex t) => Text -> Text -> Eff es a -> Eff es (Element EventResult GhcjsDomSpace t, a)
elClass' tag c = elAttr' tag ("class" =: c)

elDynAttr'
  :: (KnownTimeline es t, Dom t :> es, PostBuild t :> es, R.Reflex t)
  => Text -> R.Dynamic t (Map Text Text) -> Eff es a
  -> Eff es (Element EventResult GhcjsDomSpace t, a)
elDynAttr' tag dynAttrs child = do
  pb <- getPostBuild
  let updates = R.leftmost
        [ Map.mapKeys (AttributeName Nothing) . fmap Just <$> R.updated dynAttrs
        , Map.mapKeys (AttributeName Nothing) . fmap Just <$> R.tag (R.current dynAttrs) pb
        ]
      cfg = def
        & elementConfig_initialAttributes .~ Map.empty
        & elementConfig_modifyAttributes .~ updates
  element tag cfg child

elDynAttr
  :: (KnownTimeline es t, Dom t :> es, PostBuild t :> es, R.Reflex t)
  => Text -> R.Dynamic t (Map Text Text) -> Eff es a -> Eff es a
elDynAttr tag dynAttrs child = snd <$> elDynAttr' tag dynAttrs child

text :: forall es t. (KnownTimeline es t, Dom t :> es) => Text -> Eff es ()
text = send @(Dom t) . Text_

dynText :: forall es t. (KnownTimeline es t, Dom t :> es) => R.Dynamic t Text -> Eff es ()
dynText = send @(Dom t) . DynText_

display :: forall es t a. (KnownTimeline es t, Dom t :> es, R.Reflex t, Show a) => R.Dynamic t a -> Eff es ()
display d = dynText (R.ffor d (pack . show))

blank :: Applicative (Eff es) => Eff es ()
blank = pure ()

(=:) :: k -> v -> Map k v
(=:) = Map.singleton
