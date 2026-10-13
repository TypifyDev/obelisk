{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- 'GhcjsDomSpace' + supporting types for the Effectful-native port of
-- @Reflex.Dom.Builder.Immediate@.
--
-- This module is a straight port of the space-related declarations at
-- lines 673-733 of the upstream monolith. There is no MTL machinery
-- here to translate — 'DomSpace' is a type-class over a phantom type
-- and the definitions are pure data — so this module is
-- byte-comparable to upstream up to import flips.
--
-- The 'DomSpace' class itself is re-exported from
-- "Reflex.Dom.Core"; we only add the instance.
module Reflex.Effectful.Dom.Builder.Immediate.Space
  ( -- * DOM handler newtypes
    GhcjsDomHandler (..)
  , GhcjsDomHandler1 (..)
  , GhcjsDomEvent (..)
    -- * The space
  , GhcjsDomSpace
  , HydrationDomSpace
    -- * Event spec + filter
  , GhcjsEventSpec (..)
  , GhcjsEventFilter (..)
  , GhcjsEventHandler (..)
  , ghcjsEventSpec_filters
  , ghcjsEventSpec_handler
    -- * Small helpers
  , Pair1 (..)
  , Maybe1 (..)
  ) where

import           Control.Lens                                  (Getter, Lens')
import           Data.Functor.Contravariant                    (phantom)
import           Control.Monad.Reader                          (runReaderT)
import           Data.Default                                  (Default (..))
import qualified Data.Dependent.Map                            as DMap
import           Data.Dependent.Map                            (DMap)
import           Data.Kind                                     (Type)

import qualified GHCJS.DOM.Types                               as DOM
import           GHCJS.DOM.Types                               (JSM, uncheckedCastTo)
import qualified GHCJS.DOM.Event                               as Event

import           Reflex.Dom.Core            (DomSpace (..),
                                                                EventFlags)
import           Reflex.Dom.Core     (EventName,
                                                                EventResult,
                                                                EventTag)
import           Reflex.Dom.Core
                                                               (EventType,
                                                                defaultDomEventHandler,
                                                                withIsEvent)

-- ---------------------------------------------------------------------
-- Handler newtypes
-- ---------------------------------------------------------------------

-- | Handler that maps @a@ to @b@ in the 'JSM' monad. Used as a
-- building block for event-handler configuration; not consumed by any
-- exported operation directly in the port.
newtype GhcjsDomHandler a b = GhcjsDomHandler
  { unGhcjsDomHandler :: a -> JSM b }

-- | Higher-kinded variant of 'GhcjsDomHandler' where the input and
-- output are indexed by an 'EventTag'.
newtype GhcjsDomHandler1 a b = GhcjsDomHandler1
  { unGhcjsDomHandler1 :: forall (x :: EventTag). a x -> JSM (b x) }

-- | Newtype over the browser-native event object for a given
-- 'EventTag'; used by 'GhcjsEventFilter' and by 'GhcjsEventHandler'
-- so the handler code doesn't have to spell out the 'EventType' family
-- application at every call site.
newtype GhcjsDomEvent en = GhcjsDomEvent
  { unGhcjsDomEvent :: EventType en }

-- ---------------------------------------------------------------------
-- The space
-- ---------------------------------------------------------------------

-- | Phantom marker for the concrete DOM-in-browser space. Every raw
-- type in the 'DomSpace' instance below points at a jsaddle/ghcjs-dom
-- type, so the space is unambiguously the browser DOM.
data GhcjsDomSpace

instance DomSpace GhcjsDomSpace where
  type EventSpec GhcjsDomSpace = GhcjsEventSpec
  type RawDocument GhcjsDomSpace = DOM.Document
  type RawTextNode GhcjsDomSpace = DOM.Text
  type RawCommentNode GhcjsDomSpace = DOM.Comment
  type RawElement GhcjsDomSpace = DOM.Element
  type RawInputElement GhcjsDomSpace = DOM.HTMLInputElement
  type RawTextAreaElement GhcjsDomSpace = DOM.HTMLTextAreaElement
  type RawSelectElement GhcjsDomSpace = DOM.HTMLSelectElement
  addEventSpecFlags _ en f es = es
    { _ghcjsEventSpec_filters =
        let f' = Just . GhcjsEventFilter . \case
              Nothing -> \evt -> do
                mEventResult <- unGhcjsEventHandler (_ghcjsEventSpec_handler es) (en, evt)
                return (f mEventResult, return mEventResult)
              Just (GhcjsEventFilter oldFilter) -> \evt -> do
                (oldFlags, oldContinuation) <- oldFilter evt
                mEventResult <- oldContinuation
                let newFlags = oldFlags <> f mEventResult
                return (newFlags, return mEventResult)
        in DMap.alter f' en $ _ghcjsEventSpec_filters es
    }

-- | Phantom marker for the DOM space used *during* server-side
-- hydration. Every "raw" handle is @()@ because no live DOM node exists
-- yet — the builder is emitting the structure that will be reified
-- once hydration attaches to the pre-rendered HTML. 'EventSpec' and
-- 'addEventSpecFlags' are shared with 'GhcjsDomSpace' (the event-spec
-- shape is space-independent; only raw handles differ).
data HydrationDomSpace

instance DomSpace HydrationDomSpace where
  type EventSpec HydrationDomSpace = GhcjsEventSpec
  type RawDocument HydrationDomSpace = DOM.Document
  type RawTextNode HydrationDomSpace = ()
  type RawCommentNode HydrationDomSpace = ()
  type RawElement HydrationDomSpace = ()
  type RawInputElement HydrationDomSpace = ()
  type RawTextAreaElement HydrationDomSpace = ()
  type RawSelectElement HydrationDomSpace = ()
  addEventSpecFlags _ en f es = es
    { _ghcjsEventSpec_filters =
        let f' = Just . GhcjsEventFilter . \case
              Nothing -> \evt -> do
                mEventResult <- unGhcjsEventHandler (_ghcjsEventSpec_handler es) (en, evt)
                return (f mEventResult, return mEventResult)
              Just (GhcjsEventFilter oldFilter) -> \evt -> do
                (oldFlags, oldContinuation) <- oldFilter evt
                mEventResult <- oldContinuation
                let newFlags = oldFlags <> f mEventResult
                return (newFlags, return mEventResult)
        in DMap.alter f' en $ _ghcjsEventSpec_filters es
    }

-- ---------------------------------------------------------------------
-- Event spec + filter + handler
-- ---------------------------------------------------------------------

-- | Filter for a single 'EventName'. Given the browser event, returns
-- ('EventFlags' to apply to the DOM event, and a continuation to
-- produce the parsed 'er en' if any).
newtype GhcjsEventFilter er en = GhcjsEventFilter
  (GhcjsDomEvent en -> JSM (EventFlags, JSM (Maybe (er en))))

-- | @'Pair1' f g@ is a heterogeneous pair indexed by a common
-- kind. Used by upstream to key event filters + subscribed triggers
-- together. Kept as a utility so downstream ports don't have to
-- re-derive it.
data Pair1 (f :: k -> Type) (g :: k -> Type) (a :: k) = Pair1 (f a) (g a)

-- | Kind-indexed 'Maybe'. Same rationale as 'Pair1' — a utility used
-- by upstream that downstream ports may want.
data Maybe1 f a = Nothing1 | Just1 (f a)

-- | The full event-spec for an element: a per-event filter map plus a
-- generic fallback handler.
data GhcjsEventSpec er = GhcjsEventSpec
  { _ghcjsEventSpec_filters :: DMap EventName (GhcjsEventFilter er)
    -- ^ Per-'EventName' filters and handlers.
  , _ghcjsEventSpec_handler :: GhcjsEventHandler er
    -- ^ Fallback handler used for any 'EventName' not in the filter map.
  }

-- | Fallback event handler: given the event name and the raw browser
-- event, parse (if possible) into the caller's event-result type
-- @er en@.
newtype GhcjsEventHandler er = GhcjsEventHandler
  { unGhcjsEventHandler :: forall en. (EventName en, GhcjsDomEvent en) -> JSM (Maybe (er en)) }

-- | Hand-written lens for '_ghcjsEventSpec_filters'. Upstream uses TH
-- to derive lenses; we hand-write to keep the module free of
-- TemplateHaskell in the port (matching the @#ifndef
-- USE_TEMPLATE_HASKELL@ branch upstream).
ghcjsEventSpec_filters :: forall er. Lens' (GhcjsEventSpec er) (DMap EventName (GhcjsEventFilter er))
ghcjsEventSpec_filters f (GhcjsEventSpec a b) = (\a' -> GhcjsEventSpec a' b) <$> f a
{-# INLINE ghcjsEventSpec_filters #-}

-- | Hand-written getter for '_ghcjsEventSpec_handler'. Getter, not
-- Lens, because @er@ is universally quantified inside
-- 'GhcjsEventHandler' — you can read but not update.
ghcjsEventSpec_handler :: forall er en. Getter (GhcjsEventSpec er) ((EventName en, GhcjsDomEvent en) -> JSM (Maybe (er en)))
ghcjsEventSpec_handler f (GhcjsEventSpec _ (GhcjsEventHandler b)) = phantom (f b)
{-# INLINE ghcjsEventSpec_handler #-}

-- | The default event spec: no per-event filters, and a fallback
-- handler that delegates to 'defaultDomEventHandler' on the target
-- element.
instance er ~ EventResult => Default (GhcjsEventSpec er) where
  def = GhcjsEventSpec
    { _ghcjsEventSpec_filters = mempty
    , _ghcjsEventSpec_handler = GhcjsEventHandler $ \(en, GhcjsDomEvent evt) -> do
        t :: DOM.EventTarget <- withIsEvent en $ Event.getTargetUnchecked evt
        let e = uncheckedCastTo DOM.Element t
        runReaderT (defaultDomEventHandler e en) evt
    }
