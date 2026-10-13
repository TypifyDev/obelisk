{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE ImpredicativeTypes #-}
#else
{-# LANGUAGE Rank2Types #-} -- Implied by ImpredicativeTypes
#endif
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Port of @Reflex.Dom.Builder.Class@ to reflex-dom-core-effectful, with
-- 'DomBuilder' / 'HasDocument' / 'DomRenderHook' / 'MountableDomBuilder'
-- rewritten as Effectful 'Effect's instead of MTL type classes.
--
-- Motivation: the upstream MTL 'DomBuilder' class carries an associated
-- type @DomBuilderSpace m@ that forces every @Eff es@ to a single space
-- at a time. In the effectful port we want the SAME @Eff es@ to be able
-- to host both @DomBuilder t GhcjsDomSpace@ and @DomBuilder t StaticDomSpace@
-- as two distinct rows on the same monad. Making 'DomBuilder' an effect
-- parameterised by both the timeline @t@ and the space @space@ is the
-- honest way to express that: two distinct effect heads coexist on one
-- monad without an associated type conflict.
--
-- The pure lens classes ('InitialAttributes', 'ModifyAttributes',
-- 'HasNamespace', 'HasSetValue', 'HasDomEvent') and all data types stay
-- unchanged from the upstream shape — they have no monadic surface and
-- so are orthogonal to the effect vs. class question.
module Reflex.Effectful.Dom.Builder.Class
  ( -- * DomSpace and raw types
    DomSpace (..)
  , Namespace
  , DomFragment
    -- * Text / comment nodes
  , TextNodeConfig (..)
  , textNodeConfig_initialContents
  , textNodeConfig_setContents
  , TextNode (..)
  , CommentNodeConfig (..)
  , commentNodeConfig_initialContents
  , commentNodeConfig_setContents
  , CommentNode (..)
    -- * Attributes / event flags
  , AttributeName (..)
  , mapKeysToAttributeName
  , Propagation (..)
  , EventFlags (..)
  , preventDefault
  , stopPropagation
    -- * Element configs
  , ElementConfig (..)
  , elementConfig_namespace
  , elementConfig_initialAttributes
  , elementConfig_modifyAttributes
  , elementConfig_eventSpec
  , Element (..)
  , RawElementConfig (..)
  , rawElementConfig_modifyAttributes
  , rawElementConfig_eventSpec
  , extractRawElementConfig
    -- * Input / textarea / select
  , InputElementConfig (..)
  , inputElementConfig_initialValue
  , inputElementConfig_initialChecked
  , inputElementConfig_elementConfig
  , inputElementConfig_setValue
  , inputElementConfig_setChecked
  , InputElement (..)
  , TextAreaElementConfig (..)
  , textAreaElementConfig_initialValue
  , textAreaElementConfig_elementConfig
  , textAreaElementConfig_setValue
  , TextAreaElement (..)
  , SelectElementConfig (..)
  , selectElementConfig_initialValue
  , selectElementConfig_elementConfig
  , selectElementConfig_setValue
  , SelectElement (..)
    -- * Lens classes over configs
  , InitialAttributes (..)
  , ModifyAttributes (..)
  , HasNamespace (..)
  , HasSetValue (..)
  , HasDomEvent (..)
    -- * Deprecated
  , liftElementConfig
    -- * DomBuilder effect
  , DomBuilder (..)
  , DomBuilderSpace
  , DomBuilderTimeline
  , textNode
  , commentNode
  , element
  , inputElement
  , textAreaElement
  , selectElement
  , placeRawElement
  , wrapRawElement
    -- * HasDocument effect
  , HasDocument (..)
  , HasDocumentSpace
  , askDocument
    -- * DomRenderHook effect
  , DomRenderHook (..)
  , DomRenderHookTimeline
  , withRenderHook
  , requestDomAction
  , requestDomAction_
    -- * MountableDomBuilder effect
  , MountableDomBuilder (..)
  , buildDomFragment
  , mountDomFragment
  , module Reflex.Dom.Core
  ) where

import Control.Lens hiding (element)
import Data.Default
import Data.Functor.Misc (WrapArg (..))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM.Types as DOM

#if !MIN_VERSION_base(4,18,0)
import Data.Semigroup
#endif

import Effectful (Effect, DispatchOf, Dispatch (Dynamic), (:>), Eff)
import Effectful.Dispatch.Dynamic (send)

import Reflex.Class as Reflex
import Reflex.Effectful.Dom.Builder.Class.TH (makeLensesWithoutField)
import Reflex.Dom.Core
  ( EventResult (..), EventTag, EventName, EventResultType
  )
import Data.Functor.Misc (WrapArg (..))

-- | The raw-type family for each DOM space. Kept as an MTL-style
-- multi-parameter type class because it is a pure family over the
-- space tag and has no monadic surface.
class Default (EventSpec d EventResult) => DomSpace d where
  type EventSpec d :: (EventTag -> Type) -> Type
  type RawDocument d :: Type
  type RawTextNode d :: Type
  type RawCommentNode d :: Type
  type RawElement d :: Type
  type RawInputElement d :: Type
  type RawTextAreaElement d :: Type
  type RawSelectElement d :: Type
  addEventSpecFlags :: proxy d -> EventName en -> (Maybe (er en) -> EventFlags) -> EventSpec d er -> EventSpec d er

-- | Standalone type family for the DOM fragment shape of each space.
-- Was an associated type on the old @MountableDomBuilder@ MTL class;
-- lives standalone now so the effect head 'MountableDomBuilder t space'
-- doesn't need an associated type.
type family DomFragment (space :: Type) :: Type

-- | @'DomBuilder' t space@ — build dynamic DOM in the 'Reflex'
-- timeline @t@, in the DOM space @space@.
--
-- @space@ is a phantom effect index (like @t@): it is not carried at
-- runtime but pins every method's raw-type family via 'DomSpace'.
-- This is what lets a single @Eff es@ carry both
-- @DomBuilder t GhcjsDomSpace@ and @DomBuilder t StaticDomSpace@ as
-- two distinct effect rows.
--
-- Child continuations for 'Element' / 'SelectElement' are @m a@
-- (matching the HOE pattern used by 'Adjustable's @RunWithReplace@);
-- an interpreter runs them in the outer effect row via
-- @localSeqUnliftIO@ or its equivalent.
-- Constructor names are prefixed 'DB_' to avoid clashing with the
-- result-type newtypes 'TextNode', 'CommentNode', 'Element',
-- 'InputElement', 'TextAreaElement', 'SelectElement' — those are the
-- return-value structs (with '_textNode_raw' etc. accessors) and share
-- the desired public names. The send-wrappers ('textNode', 'commentNode',
-- ...) below expose the ergonomic call-site names.
data DomBuilder (t :: Type) (space :: Type) :: Effect where
  DB_TextNode
    :: TextNodeConfig t
    -> DomBuilder t space m (TextNode space t)
  DB_CommentNode
    :: CommentNodeConfig t
    -> DomBuilder t space m (CommentNode space t)
  DB_Element
    :: Text
    -> ElementConfig er t space
    -> m a
    -> DomBuilder t space m (Element er space t, a)
  DB_InputElement
    :: InputElementConfig er t space
    -> DomBuilder t space m (InputElement er space t)
  DB_TextAreaElement
    :: TextAreaElementConfig er t space
    -> DomBuilder t space m (TextAreaElement er space t)
  DB_SelectElement
    :: SelectElementConfig er t space
    -> m a
    -> DomBuilder t space m (SelectElement er space t, a)
  DB_PlaceRawElement
    :: RawElement space
    -> DomBuilder t space m ()
  DB_WrapRawElement
    :: RawElement space
    -> RawElementConfig er t space
    -> DomBuilder t space m (Element er space t)

type instance DispatchOf (DomBuilder t space) = 'Dynamic

-- | Given an effect row, resolve to the 'space' phantom of the row's
-- 'DomBuilder' effect. Recurses through the row until it finds the
-- first 'DomBuilder t space' constructor. Recovers the upstream
-- @DomBuilderSpace m@ ergonomics: helpers whose sig contains a
-- 'DomBuilder' constraint can name @space@ as @DomBuilderSpace es@
-- without introducing a free type variable at the call site (which
-- otherwise forces @AllowAmbiguousTypes@ + explicit type applications).
--
-- No base case: an effect row without a 'DomBuilder' is a type error
-- at use time, which is the correct semantics (a widget helper without
-- a 'DomBuilder' constraint shouldn't be calling us).
type family DomBuilderSpace (es :: [Effect]) :: Type where
  DomBuilderSpace (DomBuilder _ space : _) = space
  DomBuilderSpace (_                  : rest) = DomBuilderSpace rest

-- | Companion to 'DomBuilderSpace' — recovers the 'Reflex' timeline @t@
-- from an effect row's 'DomBuilder' entry. Needed for helpers whose
-- signature doesn't otherwise mention @t@ (e.g. 'placeRawElement',
-- 'MountableDomBuilder' methods): without this family, GHC can't pin @t@
-- from the constraint alone, producing an ambiguous-type error at the
-- call site.
type family DomBuilderTimeline (es :: [Effect]) :: Type where
  DomBuilderTimeline (DomBuilder t _ : _) = t
  DomBuilderTimeline (_              : rest) = DomBuilderTimeline rest

-- | Send-wrapper for 'TextNode'.
textNode
  :: forall t es
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => TextNodeConfig t
  -> Eff es (TextNode (DomBuilderSpace es) t)
textNode cfg = send (DB_TextNode cfg)
{-# INLINABLE textNode #-}

-- | Send-wrapper for 'CommentNode'.
commentNode
  :: forall t es
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => CommentNodeConfig t
  -> Eff es (CommentNode (DomBuilderSpace es) t)
commentNode cfg = send (DB_CommentNode cfg)
{-# INLINABLE commentNode #-}

-- | Send-wrapper for 'Element'.
element
  :: forall t er es a
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => Text
  -> ElementConfig er t (DomBuilderSpace es)
  -> Eff es a
  -> Eff es (Element er (DomBuilderSpace es) t, a)
element elementTag cfg child = send (DB_Element elementTag cfg child)
{-# INLINABLE element #-}

-- | Send-wrapper for 'InputElement'.
inputElement
  :: forall t er es
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => InputElementConfig er t (DomBuilderSpace es)
  -> Eff es (InputElement er (DomBuilderSpace es) t)
inputElement cfg = send (DB_InputElement cfg)
{-# INLINABLE inputElement #-}

-- | Send-wrapper for 'TextAreaElement'.
textAreaElement
  :: forall t er es
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => TextAreaElementConfig er t (DomBuilderSpace es)
  -> Eff es (TextAreaElement er (DomBuilderSpace es) t)
textAreaElement cfg = send (DB_TextAreaElement cfg)
{-# INLINABLE textAreaElement #-}

-- | Send-wrapper for 'SelectElement'.
selectElement
  :: forall t er es a
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => SelectElementConfig er t (DomBuilderSpace es)
  -> Eff es a
  -> Eff es (SelectElement er (DomBuilderSpace es) t, a)
selectElement cfg child = send (DB_SelectElement cfg child)
{-# INLINABLE selectElement #-}

-- | Send-wrapper for 'PlaceRawElement'. @t@ is derived from the effect
-- row via 'DomBuilderTimeline' because the argument/return types
-- otherwise don't mention @t@ (would be ambiguous with just @DomBuilderSpace@).
placeRawElement
  :: forall es
   . (DomBuilder (DomBuilderTimeline es) (DomBuilderSpace es) :> es)
  => RawElement (DomBuilderSpace es)
  -> Eff es ()
placeRawElement raw = send @(DomBuilder (DomBuilderTimeline es) (DomBuilderSpace es)) (DB_PlaceRawElement raw)
{-# INLINABLE placeRawElement #-}

-- | Send-wrapper for 'WrapRawElement'.
wrapRawElement
  :: forall t er es
   . (DomBuilder t (DomBuilderSpace es) :> es)
  => RawElement (DomBuilderSpace es)
  -> RawElementConfig er t (DomBuilderSpace es)
  -> Eff es (Element er (DomBuilderSpace es) t)
wrapRawElement raw cfg = send (DB_WrapRawElement raw cfg)
{-# INLINABLE wrapRawElement #-}

-- | @'MountableDomBuilder' t space@ — build and mount 'DomFragment's
-- into the running DOM. Fragment shape comes from the 'DomFragment'
-- type family so the effect head has no associated type of its own.
data MountableDomBuilder (t :: Type) (space :: Type) :: Effect where
  BuildDomFragment
    :: m a
    -> MountableDomBuilder t space m (DomFragment space, a)
  MountDomFragment
    :: DomFragment space
    -> Event t (DomFragment space)
    -> MountableDomBuilder t space m ()

type instance DispatchOf (MountableDomBuilder t space) = 'Dynamic

-- | Send-wrapper for 'BuildDomFragment'. @t@ / @space@ are derived from
-- the row via the type families (same rationale as 'placeRawElement').
buildDomFragment
  :: forall es a
   . (MountableDomBuilder (DomBuilderTimeline es) (DomBuilderSpace es) :> es)
  => Eff es a
  -> Eff es (DomFragment (DomBuilderSpace es), a)
buildDomFragment m = send @(MountableDomBuilder (DomBuilderTimeline es) (DomBuilderSpace es)) (BuildDomFragment m)
{-# INLINABLE buildDomFragment #-}

-- | Send-wrapper for 'MountDomFragment'.
mountDomFragment
  :: forall es
   . (MountableDomBuilder (DomBuilderTimeline es) (DomBuilderSpace es) :> es)
  => DomFragment (DomBuilderSpace es)
  -> Event (DomBuilderTimeline es) (DomFragment (DomBuilderSpace es))
  -> Eff es ()
mountDomFragment df ev = send @(MountableDomBuilder (DomBuilderTimeline es) (DomBuilderSpace es)) (MountDomFragment df ev)
{-# INLINABLE mountDomFragment #-}

type Namespace = Text

data TextNodeConfig t
   = TextNodeConfig { _textNodeConfig_initialContents :: {-# UNPACK #-} !Text
                    , _textNodeConfig_setContents :: !(Maybe (Event t Text))
                    }

#ifndef USE_TEMPLATE_HASKELL
textNodeConfig_initialContents :: Lens' (TextNodeConfig t) Text
textNodeConfig_initialContents f (TextNodeConfig a b) = (\a' -> TextNodeConfig a' b) <$> f a
{-# INLINE textNodeConfig_initialContents #-}
#endif

instance (Reflex t) => Default (TextNodeConfig t) where
  {-# INLINABLE def #-}
  def = TextNodeConfig
    { _textNodeConfig_initialContents = mempty
    , _textNodeConfig_setContents = Nothing
    }

newtype TextNode d t = TextNode
  { _textNode_raw :: RawTextNode d
  }

data CommentNodeConfig t
   = CommentNodeConfig { _commentNodeConfig_initialContents :: {-# UNPACK #-} !Text
                       , _commentNodeConfig_setContents :: !(Maybe (Event t Text))
                       }

#ifndef USE_TEMPLATE_HASKELL
commentNodeConfig_initialContents :: Lens' (CommentNodeConfig t) Text
commentNodeConfig_initialContents f (CommentNodeConfig a b) = (\a' -> CommentNodeConfig a' b) <$> f a
{-# INLINE commentNodeConfig_initialContents #-}
#endif

instance (Reflex t) => Default (CommentNodeConfig t) where
  {-# INLINABLE def #-}
  def = CommentNodeConfig
    { _commentNodeConfig_initialContents = mempty
    , _commentNodeConfig_setContents = Nothing
    }

newtype CommentNode d t = CommentNode
  { _commentNode_raw :: RawCommentNode d
  }

data AttributeName = AttributeName !(Maybe Namespace) !Text deriving (Show, Read, Eq, Ord)

mapKeysToAttributeName :: Map Text v -> Map AttributeName v
mapKeysToAttributeName = Map.mapKeysMonotonic (AttributeName Nothing)

-- | By default, AttributeNames are unnamespaced
instance IsString AttributeName where
  fromString = AttributeName Nothing . fromString

data Propagation
   = Propagation_Continue
   | Propagation_Stop
   | Propagation_StopImmediate
   deriving (Show, Read, Eq, Ord)

instance Semigroup Propagation where
  {-# INLINABLE (<>) #-}
  (<>) = max

instance Monoid Propagation where
  {-# INLINABLE mempty #-}
  mempty = Propagation_Continue
  {-# INLINABLE mappend #-}
  mappend = (<>)

data EventFlags = EventFlags
  { _eventFlags_propagation :: Propagation
  , _eventFlags_preventDefault :: Bool
  }

instance Semigroup EventFlags where
  {-# INLINABLE (<>) #-}
  EventFlags p pd <> EventFlags p' pd' = EventFlags (p <> p') (pd || pd')

instance Monoid EventFlags where
  {-# INLINABLE mempty #-}
  mempty = EventFlags Propagation_Continue False
  {-# INLINABLE mappend #-}
  mappend = (<>)

preventDefault :: EventFlags
preventDefault = mempty { _eventFlags_preventDefault = True }

stopPropagation :: EventFlags
stopPropagation = mempty { _eventFlags_propagation = Propagation_Stop }

data ElementConfig er t s
   = ElementConfig { _elementConfig_namespace :: Maybe Namespace
                   , _elementConfig_initialAttributes :: Map AttributeName Text
                   , _elementConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
                   , _elementConfig_eventSpec :: EventSpec s er
                   }

#ifndef USE_TEMPLATE_HASKELL
elementConfig_namespace :: Lens' (ElementConfig er t s) (Maybe Namespace)
elementConfig_namespace f (ElementConfig a b c d) = (\a' -> ElementConfig a' b c d) <$> f a
{-# INLINE elementConfig_namespace #-}
elementConfig_initialAttributes :: Lens' (ElementConfig er t s) (Map AttributeName Text)
elementConfig_initialAttributes f (ElementConfig a b c d) = (\b' -> ElementConfig a b' c d) <$> f b
{-# INLINE elementConfig_initialAttributes #-}
elementConfig_eventSpec :: Lens
    (ElementConfig er1 t s1)
    (ElementConfig er2 t s2)
    (EventSpec s1 er1)
    (EventSpec s2 er2)
elementConfig_eventSpec f (ElementConfig a b c d) = (\d' -> ElementConfig a b c d') <$> f d
{-# INLINE elementConfig_eventSpec #-}
#endif

data Element er d t
   = Element { _element_events :: EventSelector t (WrapArg er EventName)
             , _element_raw :: RawElement d
             }

data InputElementConfig er t s
   = InputElementConfig { _inputElementConfig_initialValue :: Text
                        , _inputElementConfig_setValue :: Maybe (Event t Text)
                        , _inputElementConfig_initialChecked :: Bool
                        , _inputElementConfig_setChecked :: Maybe (Event t Bool)
                        , _inputElementConfig_elementConfig :: ElementConfig er t s
                        }

#ifndef USE_TEMPLATE_HASKELL
inputElementConfig_initialValue :: Lens' (InputElementConfig er t m) Text
inputElementConfig_initialValue f (InputElementConfig a b c d e) = (\a' -> InputElementConfig a' b c d e) <$> f a
{-# INLINE inputElementConfig_initialValue #-}
inputElementConfig_initialChecked :: Lens' (InputElementConfig er t m) Bool
inputElementConfig_initialChecked f (InputElementConfig a b c d e) = (\c' -> InputElementConfig a b c' d e) <$> f c
{-# INLINE inputElementConfig_initialChecked #-}
inputElementConfig_elementConfig :: Lens
    (InputElementConfig er1 t m1)
    (InputElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
inputElementConfig_elementConfig f (InputElementConfig a b c d e) = (\e' -> InputElementConfig a b c d e') <$> f e
{-# INLINE inputElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (ElementConfig er t s) where
  {-# INLINABLE def #-}
  def = ElementConfig
    { _elementConfig_namespace = Nothing
    , _elementConfig_initialAttributes = mempty
    , _elementConfig_modifyAttributes = Nothing
    , _elementConfig_eventSpec = def
    }

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (InputElementConfig er t s) where
  {-# INLINABLE def #-}
  def = InputElementConfig
    { _inputElementConfig_initialValue = ""
    , _inputElementConfig_setValue = Nothing
    , _inputElementConfig_initialChecked = False
    , _inputElementConfig_setChecked = Nothing
    , _inputElementConfig_elementConfig = def
    }

data InputElement er d t
   = InputElement { _inputElement_value :: Dynamic t Text
                  , _inputElement_checked :: Dynamic t Bool
                  , _inputElement_checkedChange :: Event t Bool
                  , _inputElement_input :: Event t Text
                  , _inputElement_hasFocus :: Dynamic t Bool
                  , _inputElement_element :: Element er d t
                  , _inputElement_raw :: RawInputElement d
                  , _inputElement_files :: Dynamic t [DOM.File]
                  }

data TextAreaElementConfig er t m
   = TextAreaElementConfig { _textAreaElementConfig_initialValue :: Text
                           , _textAreaElementConfig_setValue :: Maybe (Event t Text)
                           , _textAreaElementConfig_elementConfig :: ElementConfig er t m
                           }

#ifndef USE_TEMPLATE_HASKELL
textAreaElementConfig_initialValue :: Lens' (TextAreaElementConfig er t m) Text
textAreaElementConfig_initialValue f (TextAreaElementConfig a b c) = (\a' -> TextAreaElementConfig a' b c) <$> f a
{-# INLINE textAreaElementConfig_initialValue #-}
textAreaElementConfig_elementConfig :: Lens
    (TextAreaElementConfig er1 t m1)
    (TextAreaElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
textAreaElementConfig_elementConfig f (TextAreaElementConfig a b c) = (\c' -> TextAreaElementConfig a b c') <$> f c
{-# INLINE textAreaElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (TextAreaElementConfig er t s) where
  {-# INLINABLE def #-}
  def = TextAreaElementConfig
    { _textAreaElementConfig_initialValue = ""
    , _textAreaElementConfig_setValue = Nothing
    , _textAreaElementConfig_elementConfig = def
    }

data TextAreaElement er d t
   = TextAreaElement { _textAreaElement_value :: Dynamic t Text
                     , _textAreaElement_input :: Event t Text
                     , _textAreaElement_hasFocus :: Dynamic t Bool
                     , _textAreaElement_element :: Element er d t
                     , _textAreaElement_raw :: RawTextAreaElement d
                     }

extractRawElementConfig :: ElementConfig er t m -> RawElementConfig er t m
extractRawElementConfig cfg = RawElementConfig
  { _rawElementConfig_modifyAttributes = _elementConfig_modifyAttributes cfg
  , _rawElementConfig_eventSpec = _elementConfig_eventSpec cfg
  }

data RawElementConfig er t s = RawElementConfig
  { _rawElementConfig_modifyAttributes :: Maybe (Event t (Map AttributeName (Maybe Text)))
  , _rawElementConfig_eventSpec :: EventSpec s er
  }

#ifndef USE_TEMPLATE_HASKELL
rawElementConfig_eventSpec :: Lens
    (RawElementConfig er1 t s1)
    (RawElementConfig er2 t s2)
    (EventSpec s1 er1)
    (EventSpec s2 er2)
rawElementConfig_eventSpec f (RawElementConfig a b) = (\b' -> RawElementConfig a b') <$> f b
{-# INLINE rawElementConfig_eventSpec #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (RawElementConfig er t s) where
  def = RawElementConfig
    { _rawElementConfig_modifyAttributes = Nothing
    , _rawElementConfig_eventSpec = def
    }

data SelectElementConfig er t m = SelectElementConfig
  { _selectElementConfig_initialValue :: Text
  , _selectElementConfig_setValue :: Maybe (Event t Text)
  , _selectElementConfig_elementConfig :: ElementConfig er t m
  }

#ifndef USE_TEMPLATE_HASKELL
selectElementConfig_initialValue :: Lens' (SelectElementConfig er t m) Text
selectElementConfig_initialValue f (SelectElementConfig a b c) = (\a' -> SelectElementConfig a' b c) <$> f a
{-# INLINE selectElementConfig_initialValue #-}
selectElementConfig_elementConfig :: Lens
    (SelectElementConfig er1 t m1)
    (SelectElementConfig er2 t m2)
    (ElementConfig er1 t m1)
    (ElementConfig er2 t m2)
selectElementConfig_elementConfig f (SelectElementConfig a b c) = (\c' -> SelectElementConfig a b c') <$> f c
{-# INLINE selectElementConfig_elementConfig #-}
#endif

instance (Reflex t, er ~ EventResult, DomSpace s) => Default (SelectElementConfig er t s) where
  def = SelectElementConfig
    { _selectElementConfig_initialValue = ""
    , _selectElementConfig_setValue = Nothing
    , _selectElementConfig_elementConfig = def
    }

data SelectElement er d t = SelectElement
  { _selectElement_element :: Element er d t
  , _selectElement_value :: Dynamic t Text
  , _selectElement_change :: Event t Text -- ^ Fires when the value is changed by the user, but not when it is set by setValue
  , _selectElement_hasFocus :: Dynamic t Bool
  , _selectElement_raw :: RawSelectElement d
  }

#ifdef USE_TEMPLATE_HASKELL
concat <$> mapM (uncurry makeLensesWithoutField)
  [ (["_textNodeConfig_setContents"], ''TextNodeConfig)
  , (["_commentNodeConfig_setContents"], ''CommentNodeConfig)
  , ([ "_inputElementConfig_setValue"
     , "_inputElementConfig_setChecked" ], ''InputElementConfig)
  , (["_rawElementConfig_modifyAttributes"], ''RawElementConfig)
  , (["_elementConfig_modifyAttributes"], ''ElementConfig)
  , (["_textAreaElementConfig_setValue"], ''TextAreaElementConfig)
  , (["_selectElementConfig_setValue"], ''SelectElementConfig)
  ]
#endif

-- | Illegal-lens accessor: the underlying 'TextNodeConfig' uses a 'Maybe'
-- for efficiency, but lens-based access treats @Nothing@ as @never@.
textNodeConfig_setContents :: Reflex t => Lens' (TextNodeConfig t) (Event t Text)
textNodeConfig_setContents =
  let getter = fromMaybe never . _textNodeConfig_setContents
      setter t e = t { _textNodeConfig_setContents = Just e }
  in lens getter setter

commentNodeConfig_setContents :: Reflex t => Lens (CommentNodeConfig t) (CommentNodeConfig t) (Event t Text) (Event t Text)
commentNodeConfig_setContents =
  let getter = fromMaybe never . _commentNodeConfig_setContents
      setter t e = t { _commentNodeConfig_setContents = Just e }
  in lens getter setter

inputElementConfig_setValue :: Reflex t => Lens' (InputElementConfig er t m) (Event t Text)
inputElementConfig_setValue =
  let getter = fromMaybe never . _inputElementConfig_setValue
      setter t e = t { _inputElementConfig_setValue = Just e }
  in lens getter setter

inputElementConfig_setChecked :: Reflex t => Lens' (InputElementConfig er t m) (Event t Bool)
inputElementConfig_setChecked =
  let getter = fromMaybe never . _inputElementConfig_setChecked
      setter t e = t { _inputElementConfig_setChecked = Just e }
  in lens getter setter

rawElementConfig_modifyAttributes :: Reflex t => Lens' (RawElementConfig er t m) (Event t (Map AttributeName (Maybe Text)))
rawElementConfig_modifyAttributes =
  let getter = fromMaybe never . _rawElementConfig_modifyAttributes
      setter t e = t { _rawElementConfig_modifyAttributes = Just e }
  in lens getter setter

elementConfig_modifyAttributes :: Reflex t => Lens' (ElementConfig er t m) (Event t (Map AttributeName (Maybe Text)))
elementConfig_modifyAttributes =
  let getter = fromMaybe never . _elementConfig_modifyAttributes
      setter t e = t { _elementConfig_modifyAttributes = Just e }
  in lens getter setter

textAreaElementConfig_setValue :: Reflex t => Lens' (TextAreaElementConfig er t m) (Event t Text)
textAreaElementConfig_setValue =
  let getter = fromMaybe never . _textAreaElementConfig_setValue
      setter t e = t { _textAreaElementConfig_setValue = Just e }
  in lens getter setter

selectElementConfig_setValue :: Reflex t => Lens' (SelectElementConfig er t m) (Event t Text)
selectElementConfig_setValue =
  let getter = fromMaybe never . _selectElementConfig_setValue
      setter t e = t { _selectElementConfig_setValue = Just e }
  in lens getter setter

class InitialAttributes a where
  initialAttributes :: Lens' a (Map AttributeName Text)

instance InitialAttributes (ElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = elementConfig_initialAttributes

instance InitialAttributes (InputElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = inputElementConfig_elementConfig . elementConfig_initialAttributes

instance InitialAttributes (TextAreaElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = textAreaElementConfig_elementConfig . elementConfig_initialAttributes

instance InitialAttributes (SelectElementConfig er t m) where
  {-# INLINABLE initialAttributes #-}
  initialAttributes = selectElementConfig_elementConfig . elementConfig_initialAttributes

class ModifyAttributes t a | a -> t where
  modifyAttributes :: Reflex t => Lens' a (Event t (Map AttributeName (Maybe Text)))

instance ModifyAttributes t (ElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = elementConfig_modifyAttributes

instance ModifyAttributes t (InputElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = inputElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (TextAreaElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = textAreaElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (SelectElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = selectElementConfig_elementConfig . elementConfig_modifyAttributes

instance ModifyAttributes t (RawElementConfig er t m) where
  {-# INLINABLE modifyAttributes #-}
  modifyAttributes = rawElementConfig_modifyAttributes

class HasNamespace a where
  namespace :: Lens' a (Maybe Namespace)

instance HasNamespace (ElementConfig er t m) where
  {-# INLINABLE namespace #-}
  namespace = elementConfig_namespace

-- * Convenience: pure @HasDomEvent@ lens class

class HasDomEvent t target eventName | target -> t where
  type DomEventType target eventName :: Type
  domEvent :: EventName eventName -> target -> Event t (DomEventType target eventName)

instance Reflex t => HasDomEvent t (Element EventResult d t) en where
  type DomEventType (Element EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en e = coerceEvent $ Reflex.select (_element_events e) (WrapArg en)

instance Reflex t => HasDomEvent t (InputElement EventResult d t) en where
  type DomEventType (InputElement EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en = domEvent en . _inputElement_element

instance Reflex t => HasDomEvent t (TextAreaElement EventResult d t) en where
  type DomEventType (TextAreaElement EventResult d t) en = EventResultType en
  {-# INLINABLE domEvent #-}
  domEvent en = domEvent en . _textAreaElement_element

-- | @'DomRenderHook' t@ — hook into the JSM render pipeline.
data DomRenderHook (t :: Type) :: Effect where
  WithRenderHook
    :: (forall x. JSM x -> JSM x)
    -> m a
    -> DomRenderHook t m a
  RequestDomAction
    :: Event t (JSM a)
    -> DomRenderHook t m (Event t a)
  RequestDomAction_
    :: Event t (JSM a)
    -> DomRenderHook t m ()

type instance DispatchOf (DomRenderHook t) = 'Dynamic

-- | Extract the 'Reflex' timeline of an effect row's 'DomRenderHook'.
-- Analogous to 'DomBuilderTimeline' but keyed on the render-hook slot.
type family DomRenderHookTimeline (es :: [Effect]) :: Type where
  DomRenderHookTimeline (DomRenderHook t : _) = t
  DomRenderHookTimeline (_               : rest) = DomRenderHookTimeline rest

-- | @t@ derived from the row via 'DomRenderHookTimeline'.
withRenderHook
  :: forall es a
   . (DomRenderHook (DomRenderHookTimeline es) :> es)
  => (forall x. JSM x -> JSM x)
  -> Eff es a
  -> Eff es a
withRenderHook hook m = send @(DomRenderHook (DomRenderHookTimeline es)) (WithRenderHook hook m)
{-# INLINABLE withRenderHook #-}

requestDomAction
  :: forall es a
   . (DomRenderHook (DomRenderHookTimeline es) :> es)
  => Event (DomRenderHookTimeline es) (JSM a)
  -> Eff es (Event (DomRenderHookTimeline es) a)
requestDomAction ev = send @(DomRenderHook (DomRenderHookTimeline es)) (RequestDomAction ev)
{-# INLINABLE requestDomAction #-}

requestDomAction_
  :: forall es a
   . (DomRenderHook (DomRenderHookTimeline es) :> es)
  => Event (DomRenderHookTimeline es) (JSM a)
  -> Eff es ()
requestDomAction_ ev = send @(DomRenderHook (DomRenderHookTimeline es)) (RequestDomAction_ ev)
{-# INLINABLE requestDomAction_ #-}

{-# DEPRECATED liftElementConfig "Use 'id' instead; this function is no longer necessary" #-}
liftElementConfig :: ElementConfig er t s -> ElementConfig er t s
liftElementConfig = id

-- | @'HasDocument' space@ — access the ambient 'RawDocument' for @space@.
data HasDocument (space :: Type) :: Effect where
  AskDocument :: HasDocument space m (RawDocument space)

type instance DispatchOf (HasDocument space) = 'Dynamic

-- | Extract the 'space' of an effect row's 'HasDocument'. Analogous to
-- 'DomBuilderSpace' but keyed on the 'HasDocument' slot.
type family HasDocumentSpace (es :: [Effect]) :: Type where
  HasDocumentSpace (HasDocument space : _) = space
  HasDocumentSpace (_                 : rest) = HasDocumentSpace rest

askDocument
  :: forall es
   . (HasDocument (HasDocumentSpace es) :> es)
  => Eff es (RawDocument (HasDocumentSpace es))
askDocument = send @(HasDocument (HasDocumentSpace es)) AskDocument
{-# INLINABLE askDocument #-}

class HasSetValue a where
  type SetValue a :: Type
  setValue :: Lens' a (SetValue a)

instance Reflex t => HasSetValue (TextAreaElementConfig er t m) where
  type SetValue (TextAreaElementConfig er t m) = Event t Text
  setValue = textAreaElementConfig_setValue
