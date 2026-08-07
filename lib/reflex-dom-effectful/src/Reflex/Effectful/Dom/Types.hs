{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Effectful.Types
  ( -- * Timeline inference
    Timeline
  , KnownTimeline
  , T
  , runTimeline

    -- * Core FRP types (re-exported from reflex)
  , Event
  , Dynamic
  , Behavior
  , Incremental

    -- * Push/Pull monads
  , Reflex.PushM
  , Reflex.PullM

    -- * DOM types (re-exported from reflex-dom)
  , Element
  , InputElement
  , TextNode
  , CommentNode
  , TextAreaElement
  , SelectElement

    -- * Config types (re-exported)
  , ElementConfig
  , InputElementConfig
  , TextAreaElementConfig
  , SelectElementConfig
  , TextNodeConfig
  , CommentNodeConfig
  , RawElementConfig

    -- * DOM space
  , GhcjsDomSpace

    -- * Event handling
  , EventResult
  , EventSelector
  , EventName

    -- * Patches
  , PatchDMap
  , PatchDMapWithMove
  , PatchIntMap

    -- * Concrete timeline (for mainWidgetEff)
  , DomTimeline
  ) where

import           Data.Kind                (Type)
import           Effectful                (Effect, Eff, Dispatch(..), DispatchOf)
import           Effectful.Dispatch.Static (SideEffects(..), StaticRep, evalStaticRep)

import           Reflex
                   ( Behavior, Dynamic, Event, EventSelector
                   , Incremental, PatchDMap, PatchDMapWithMove, PatchIntMap
                   )
import qualified Reflex

import           Reflex.Dom.Core
                   ( Element, InputElement, TextNode, CommentNode
                   , TextAreaElement, SelectElement
                   , ElementConfig, InputElementConfig, TextAreaElementConfig
                   , SelectElementConfig, TextNodeConfig, CommentNodeConfig
                   , RawElementConfig, GhcjsDomSpace, EventResult, EventName
                   )

import           Reflex.Dom.Main (DomTimeline)

-- | Phantom effect that marks which timeline @t@ is in use.
-- Zero runtime cost. Placed in the effect stack by 'mainWidgetEff'.
-- The type family 'T' extracts @t@ from the stack.
data Timeline (t :: Type) :: Effect
type instance DispatchOf (Timeline t) = 'Static 'NoSideEffects
newtype instance StaticRep (Timeline t) = TimelineRep ()

-- | Extract the timeline type from an effect stack.
--
-- Scans the stack for @Timeline t@ and returns @t@.
-- This is what eliminates the need for @\@t@ type applications.
--
-- @
-- myWidget :: (Dom (T es) :> es, Reflex (T es)) => Eff es ()
-- myWidget = el "div" $ text "hello"   -- no \@t needed
-- @
type family T (es :: [Effect]) :: Type where
  T (Timeline t ': _)  = t
  T (_ ': es)          = T es

-- | Resolves the timeline @t@ from the effect stack @es@ via functional
-- dependency. GHC scans @es@ for @Timeline t@ and determines @t@.
--
-- This eliminates the need for @\@t@ type applications on function calls.
-- Put @KnownTimeline es t@ in your constraint and @t@ is inferred everywhere.
class KnownTimeline (es :: [Effect]) t | es -> t
instance {-# OVERLAPPING #-} KnownTimeline (Timeline t ': es) t
instance {-# OVERLAPPABLE #-} KnownTimeline es t => KnownTimeline (e ': es) t

-- | Strip the 'Timeline' effect from the stack. Used by 'mainWidgetEff'.
runTimeline :: Eff (Timeline t : es) a -> Eff es a
runTimeline = evalStaticRep (TimelineRep ())
