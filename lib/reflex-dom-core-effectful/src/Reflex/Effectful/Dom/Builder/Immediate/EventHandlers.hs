{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Pure jsaddle event helpers for the Effectful port of
-- @Reflex.Dom.Builder.Immediate@.
--
-- Every declaration in this module is a straight port of upstream
-- (lines 2254–2736 of the monolith): the 'EventType' family, the
-- 'withIsEvent' pattern-match, the two @default…EventHandler@
-- dispatchers, the @…OnEventName@ subscribers, the small per-event
-- accessors ('getKeyEvent', 'getMouseEventCoords', 'getPasteData',
-- 'getTouchEvent', 'getWheelEvent'), and the 'ElementEventTarget'
-- newtype used to unify SVG + HTML event targets.
--
-- None of it depends on MTL transformer plumbing — these are all pure
-- 'JSM' or 'EventM' helpers over ghcjs-dom types — so the port is
-- byte-comparable to upstream up to import path flips. It is safe to
-- consume from any module in the Immediate port (Space, Wrap,
-- Element, Window, Focus).
module Reflex.Effectful.Dom.Builder.Immediate.EventHandlers
  ( -- * Event type family
    EventType
    -- * IsEvent witness
  , withIsEvent
    -- * Default handlers
  , defaultDomEventHandler
  , defaultDomWindowEventHandler
    -- * On-event-name subscribers
  , elementOnEventName
  , windowOnEventName
    -- * Per-event accessors
  , getKeyEvent
  , getMouseEventCoords
  , getPasteData
  , getTouchEvent
  , getWheelEvent
    -- * Element event target adapter
  , ElementEventTarget (..)
    -- * Debug
  , showEventName
  ) where

import           Control.Monad                                    (forM)
import           Data.Bitraversable                               (bisequence)
import           Data.Text                                        (Text)

import qualified GHCJS.DOM.ClipboardEvent                         as ClipboardEvent
import qualified GHCJS.DOM.DataTransfer                           as DataTransfer
import qualified GHCJS.DOM.DocumentAndElementEventHandlers        as Events
import qualified GHCJS.DOM.Element                                as Element
import           GHCJS.DOM.Element                                (getScrollTop)
import           GHCJS.DOM.EventM                                 (EventM,
                                                                   event, on)
import qualified GHCJS.DOM.GlobalEventHandlers                    as Events
import qualified GHCJS.DOM.KeyboardEvent                          as KeyboardEvent
import           GHCJS.DOM.KeyboardEvent                          (getCharCode,
                                                                   getKeyCode)
import           GHCJS.DOM.MouseEvent                             (getClientX,
                                                                   getClientY)
import qualified GHCJS.DOM.Touch                                  as Touch
import qualified GHCJS.DOM.TouchEvent                             as TouchEvent
import qualified GHCJS.DOM.TouchList                              as TouchList
import qualified GHCJS.DOM.Types                                  as DOM
import           GHCJS.DOM.Types                                  (ClipboardEvent,
                                                                   FocusEvent,
                                                                   IsElement,
                                                                   IsEvent,
                                                                   IsNode,
                                                                   JSM,
                                                                   KeyboardEvent,
                                                                   MouseEvent,
                                                                   TouchEvent,
                                                                   UIEvent,
                                                                   WheelEvent)
import qualified GHCJS.DOM.UIEvent                                as UIEvent ()
import qualified GHCJS.DOM.WheelEvent                             as WheelEvent
import qualified GHCJS.DOM.Window                                 as Window

import           Reflex.Dom.Core        (DeltaMode (..),
                                                                   EventName (..),
                                                                   EventResult (..),
                                                                   EventTag (..),
                                                                   TouchEventResult (..),
                                                                   TouchResult (..),
                                                                   WheelEventResult (..))

-- ---------------------------------------------------------------------
-- EventType family
-- ---------------------------------------------------------------------

-- | Maps an 'EventTag' at the type-level to the concrete browser event
-- type carried by that 'EventName'. Consumers use @EventType en@
-- wherever they need the actual browser event (e.g. inside 'EventM'
-- callbacks).
type family EventType (en :: EventTag) where
  EventType 'AbortTag        = UIEvent
  EventType 'BlurTag         = FocusEvent
  EventType 'ChangeTag       = DOM.Event
  EventType 'ClickTag        = MouseEvent
  EventType 'ContextmenuTag  = MouseEvent
  EventType 'DblclickTag     = MouseEvent
  EventType 'DragTag         = MouseEvent
  EventType 'DragendTag      = MouseEvent
  EventType 'DragenterTag    = MouseEvent
  EventType 'DragleaveTag    = MouseEvent
  EventType 'DragoverTag     = MouseEvent
  EventType 'DragstartTag    = MouseEvent
  EventType 'DropTag         = MouseEvent
  EventType 'ErrorTag        = UIEvent
  EventType 'FocusTag        = FocusEvent
  EventType 'InputTag        = DOM.Event
  EventType 'InvalidTag      = DOM.Event
  EventType 'KeydownTag      = KeyboardEvent
  EventType 'KeypressTag     = KeyboardEvent
  EventType 'KeyupTag        = KeyboardEvent
  EventType 'LoadTag         = UIEvent
  EventType 'MousedownTag    = MouseEvent
  EventType 'MouseenterTag   = MouseEvent
  EventType 'MouseleaveTag   = MouseEvent
  EventType 'MousemoveTag    = MouseEvent
  EventType 'MouseoutTag     = MouseEvent
  EventType 'MouseoverTag    = MouseEvent
  EventType 'MouseupTag      = MouseEvent
  EventType 'MousewheelTag   = MouseEvent
  EventType 'ScrollTag       = UIEvent
  EventType 'SelectTag       = UIEvent
  EventType 'SubmitTag       = DOM.Event
  EventType 'WheelTag        = WheelEvent
  EventType 'BeforecutTag    = ClipboardEvent
  EventType 'CutTag          = ClipboardEvent
  EventType 'BeforecopyTag   = ClipboardEvent
  EventType 'CopyTag         = ClipboardEvent
  EventType 'BeforepasteTag  = ClipboardEvent
  EventType 'PasteTag        = ClipboardEvent
  EventType 'ResetTag        = DOM.Event
  EventType 'SearchTag       = DOM.Event
  EventType 'SelectstartTag  = DOM.Event
  EventType 'TouchstartTag   = TouchEvent
  EventType 'TouchmoveTag    = TouchEvent
  EventType 'TouchendTag     = TouchEvent
  EventType 'TouchcancelTag  = TouchEvent

-- ---------------------------------------------------------------------
-- withIsEvent
-- ---------------------------------------------------------------------

-- | Runtime witness that @EventType en@ has an 'IsEvent' instance for
-- every constructor of 'EventName'. Case-splits over 'EventName' to
-- convince GHC.
withIsEvent :: EventName en -> (IsEvent (EventType en) => r) -> r
withIsEvent en r = case en of
  Click        -> r
  Dblclick     -> r
  Keypress     -> r
  Scroll       -> r
  Keydown      -> r
  Keyup        -> r
  Mousemove    -> r
  Mouseup      -> r
  Mousedown    -> r
  Mouseenter   -> r
  Mouseleave   -> r
  Focus        -> r
  Blur         -> r
  Change       -> r
  Drag         -> r
  Dragend      -> r
  Dragenter    -> r
  Dragleave    -> r
  Dragover     -> r
  Dragstart    -> r
  Drop         -> r
  Abort        -> r
  Contextmenu  -> r
  Error        -> r
  Input        -> r
  Invalid      -> r
  Load         -> r
  Mouseout     -> r
  Mouseover    -> r
  Select       -> r
  Submit       -> r
  Beforecut    -> r
  Cut          -> r
  Beforecopy   -> r
  Copy         -> r
  Beforepaste  -> r
  Paste        -> r
  Reset        -> r
  Search       -> r
  Selectstart  -> r
  Touchstart   -> r
  Touchmove    -> r
  Touchend     -> r
  Touchcancel  -> r
  Mousewheel   -> r
  Wheel        -> r

-- ---------------------------------------------------------------------
-- Debug
-- ---------------------------------------------------------------------

-- | Display name for an 'EventName'. Used by hydration warnings and
-- diagnostic tooling.
showEventName :: EventName en -> String
showEventName en = case en of
  Abort        -> "Abort"
  Blur         -> "Blur"
  Change       -> "Change"
  Click        -> "Click"
  Contextmenu  -> "Contextmenu"
  Dblclick     -> "Dblclick"
  Drag         -> "Drag"
  Dragend      -> "Dragend"
  Dragenter    -> "Dragenter"
  Dragleave    -> "Dragleave"
  Dragover     -> "Dragover"
  Dragstart    -> "Dragstart"
  Drop         -> "Drop"
  Error        -> "Error"
  Focus        -> "Focus"
  Input        -> "Input"
  Invalid      -> "Invalid"
  Keydown      -> "Keydown"
  Keypress     -> "Keypress"
  Keyup        -> "Keyup"
  Load         -> "Load"
  Mousedown    -> "Mousedown"
  Mouseenter   -> "Mouseenter"
  Mouseleave   -> "Mouseleave"
  Mousemove    -> "Mousemove"
  Mouseout     -> "Mouseout"
  Mouseover    -> "Mouseover"
  Mouseup      -> "Mouseup"
  Mousewheel   -> "Mousewheel"
  Scroll       -> "Scroll"
  Select       -> "Select"
  Submit       -> "Submit"
  Wheel        -> "Wheel"
  Beforecut    -> "Beforecut"
  Cut          -> "Cut"
  Beforecopy   -> "Beforecopy"
  Copy         -> "Copy"
  Beforepaste  -> "Beforepaste"
  Paste        -> "Paste"
  Reset        -> "Reset"
  Search       -> "Search"
  Selectstart  -> "Selectstart"
  Touchstart   -> "Touchstart"
  Touchmove    -> "Touchmove"
  Touchend     -> "Touchend"
  Touchcancel  -> "Touchcancel"

-- ---------------------------------------------------------------------
-- Per-event accessors
-- ---------------------------------------------------------------------

-- | Read the currently-fired key event's 'Word' key code. Tries
-- @which@, then @charCode@, then falls back to @keyCode@. All three
-- accessors are needed for cross-browser coverage; upstream comment
-- notes this ordering.
getKeyEvent :: EventM e KeyboardEvent Word
getKeyEvent = do
  e <- event
  which <- KeyboardEvent.getWhich e
  if which /= 0 then return which else do
    charCode <- getCharCode e
    if charCode /= 0 then return charCode else
      getKeyCode e

-- | Read the currently-fired mouse event's @(clientX, clientY)@
-- coordinates.
getMouseEventCoords :: EventM e MouseEvent (Int, Int)
getMouseEventCoords = do
  e <- event
  bisequence (getClientX e, getClientY e)

-- | Read the currently-fired clipboard event's text payload, if any.
-- Returns 'Nothing' when no DataTransfer is attached (some browsers
-- do this for non-text payloads).
getPasteData :: EventM e ClipboardEvent (Maybe Text)
getPasteData = do
  e <- event
  mdt <- ClipboardEvent.getClipboardData e
  case mdt of
    Nothing -> return Nothing
    Just dt -> Just <$> DataTransfer.getData dt ("text" :: Text)

-- | Read the currently-fired touch event into a 'TouchEventResult'
-- collecting every touch identifier and its coordinates from
-- @changedTouches@, @targetTouches@, and @touches@.
getTouchEvent :: EventM e TouchEvent TouchEventResult
getTouchEvent = do
  let touchResults ts = do
          n <- TouchList.getLength ts
          forM (takeWhile (< n) [0..]) $ \ix -> do
            t <- TouchList.item ts ix
            identifier <- Touch.getIdentifier t
            screenX <- Touch.getScreenX t
            screenY <- Touch.getScreenY t
            clientX <- Touch.getClientX t
            clientY <- Touch.getClientY t
            pageX <- Touch.getPageX t
            pageY <- Touch.getPageY t
            return TouchResult
              { _touchResult_identifier = identifier
              , _touchResult_screenX = screenX
              , _touchResult_screenY = screenY
              , _touchResult_clientX = clientX
              , _touchResult_clientY = clientY
              , _touchResult_pageX = pageX
              , _touchResult_pageY = pageY
              }
  e <- event
  altKey <- TouchEvent.getAltKey e
  ctrlKey <- TouchEvent.getCtrlKey e
  shiftKey <- TouchEvent.getShiftKey e
  metaKey <- TouchEvent.getMetaKey e
  changedTouches <- touchResults =<< TouchEvent.getChangedTouches e
  targetTouches <- touchResults =<< TouchEvent.getTargetTouches e
  touches <- touchResults =<< TouchEvent.getTouches e
  return $ TouchEventResult
    { _touchEventResult_altKey = altKey
    , _touchEventResult_changedTouches = changedTouches
    , _touchEventResult_ctrlKey = ctrlKey
    , _touchEventResult_metaKey = metaKey
    , _touchEventResult_shiftKey = shiftKey
    , _touchEventResult_targetTouches = targetTouches
    , _touchEventResult_touches = touches
    }

-- | Read the currently-fired wheel event's delta components and
-- interpret its @deltaMode@ enum. The final @_ -> error@ arm is a
-- port-preserving choice: the DOM spec only defines 0, 1, 2, so any
-- other value is genuinely impossible in a spec-compliant browser and
-- we prefer a loud crash to a silent default (aligns with the
-- crash-don't-fail-silently rule).
getWheelEvent :: EventM e WheelEvent WheelEventResult
getWheelEvent = do
  e <- event
  dx :: Double <- WheelEvent.getDeltaX e
  dy :: Double <- WheelEvent.getDeltaY e
  dz :: Double <- WheelEvent.getDeltaZ e
  deltaMode :: Word <- WheelEvent.getDeltaMode e
  return $ WheelEventResult
    { _wheelEventResult_deltaX = dx
    , _wheelEventResult_deltaY = dy
    , _wheelEventResult_deltaZ = dz
    , _wheelEventResult_deltaMode = case deltaMode of
        0 -> DeltaPixel
        1 -> DeltaLine
        2 -> DeltaPage
        -- See https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent/deltaMode
        _ -> error "getWheelEvent: impossible encoding"
    }

-- ---------------------------------------------------------------------
-- Default handlers
-- ---------------------------------------------------------------------

-- | Default 'EventM' handler for element events. For events that
-- carry a meaningful payload ('Dblclick' → coords, 'Keypress' → key
-- code, 'Scroll' → scroll offset, 'Paste' → clipboard text,
-- 'Touch*' → touch list, 'Wheel' → wheel delta) reads and wraps in
-- 'EventResult'; for the rest, returns 'Just ()' as the event
-- happened.
defaultDomEventHandler :: IsElement e => e -> EventName en -> EventM e (EventType en) (Maybe (EventResult en))
defaultDomEventHandler e evt = fmap (Just . EventResult) $ case evt of
  Click        -> return ()
  Dblclick     -> getMouseEventCoords
  Keypress     -> getKeyEvent
  Scroll       -> fromIntegral <$> getScrollTop e
  Keydown      -> getKeyEvent
  Keyup        -> getKeyEvent
  Mousemove    -> getMouseEventCoords
  Mouseup      -> getMouseEventCoords
  Mousedown    -> getMouseEventCoords
  Mouseenter   -> return ()
  Mouseleave   -> return ()
  Focus        -> return ()
  Blur         -> return ()
  Change       -> return ()
  Drag         -> return ()
  Dragend      -> return ()
  Dragenter    -> return ()
  Dragleave    -> return ()
  Dragover     -> return ()
  Dragstart    -> return ()
  Drop         -> return ()
  Abort        -> return ()
  Contextmenu  -> return ()
  Error        -> return ()
  Input        -> return ()
  Invalid      -> return ()
  Load         -> return ()
  Mouseout     -> return ()
  Mouseover    -> return ()
  Select       -> return ()
  Submit       -> return ()
  Beforecut    -> return ()
  Cut          -> return ()
  Beforecopy   -> return ()
  Copy         -> return ()
  Beforepaste  -> return ()
  Paste        -> getPasteData
  Reset        -> return ()
  Search       -> return ()
  Selectstart  -> return ()
  Touchstart   -> getTouchEvent
  Touchmove    -> getTouchEvent
  Touchend     -> getTouchEvent
  Touchcancel  -> getTouchEvent
  Mousewheel   -> return ()
  Wheel        -> getWheelEvent

-- | Default 'EventM' handler for window-scoped events. Same shape as
-- 'defaultDomEventHandler', except 'Scroll' reads the window's
-- @scrollY@ instead of an element's @scrollTop@.
defaultDomWindowEventHandler :: DOM.Window -> EventName en -> EventM DOM.Window (EventType en) (Maybe (EventResult en))
defaultDomWindowEventHandler w evt = fmap (Just . EventResult) $ case evt of
  Click        -> return ()
  Dblclick     -> getMouseEventCoords
  Keypress     -> getKeyEvent
  Scroll       -> Window.getScrollY w
  Keydown      -> getKeyEvent
  Keyup        -> getKeyEvent
  Mousemove    -> getMouseEventCoords
  Mouseup      -> getMouseEventCoords
  Mousedown    -> getMouseEventCoords
  Mouseenter   -> return ()
  Mouseleave   -> return ()
  Focus        -> return ()
  Blur         -> return ()
  Change       -> return ()
  Drag         -> return ()
  Dragend      -> return ()
  Dragenter    -> return ()
  Dragleave    -> return ()
  Dragover     -> return ()
  Dragstart    -> return ()
  Drop         -> return ()
  Abort        -> return ()
  Contextmenu  -> return ()
  Error        -> return ()
  Input        -> return ()
  Invalid      -> return ()
  Load         -> return ()
  Mouseout     -> return ()
  Mouseover    -> return ()
  Select       -> return ()
  Submit       -> return ()
  Beforecut    -> return ()
  Cut          -> return ()
  Beforecopy   -> return ()
  Copy         -> return ()
  Beforepaste  -> return ()
  Paste        -> getPasteData
  Reset        -> return ()
  Search       -> return ()
  Selectstart  -> return ()
  Touchstart   -> getTouchEvent
  Touchmove    -> getTouchEvent
  Touchend     -> getTouchEvent
  Touchcancel  -> getTouchEvent
  Mousewheel   -> return ()
  Wheel        -> getWheelEvent

-- ---------------------------------------------------------------------
-- ElementEventTarget
-- ---------------------------------------------------------------------

-- | Wrapper to let both SVG and HTML elements participate in the
-- same @on@-style event registration. Necessary because the
-- @IsGlobalEventHandlers@ / @IsDocumentAndElementEventHandlers@
-- classes are defined on 'DOM.Element' generically, and we want to
-- take advantage of that without breaking SVG-specific code.
newtype ElementEventTarget = ElementEventTarget DOM.Element
  deriving (DOM.IsGObject, DOM.ToJSVal, DOM.IsSlotable, DOM.IsParentNode
           , DOM.IsNonDocumentTypeChildNode, DOM.IsChildNode
           , DOM.IsAnimatable, IsNode, IsElement)

instance DOM.FromJSVal ElementEventTarget where
  fromJSVal = fmap (fmap ElementEventTarget) . DOM.fromJSVal
instance DOM.IsEventTarget ElementEventTarget
instance DOM.IsGlobalEventHandlers ElementEventTarget
instance DOM.IsDocumentAndElementEventHandlers ElementEventTarget

-- ---------------------------------------------------------------------
-- OnEventName subscribers
-- ---------------------------------------------------------------------

-- | Register a per-'EventName' listener on an element and return a
-- 'JSM' unsubscribe action. Dispatches to the appropriate
-- ghcjs-dom @on@-family function based on the 'EventName'.
elementOnEventName :: IsElement e => EventName en -> e -> EventM e (EventType en) () -> JSM (JSM ())
elementOnEventName en e_ = let e = ElementEventTarget (DOM.toElement e_) in case en of
  Abort        -> on e Events.abort
  Blur         -> on e Events.blur
  Change       -> on e Events.change
  Click        -> on e Events.click
  Contextmenu  -> on e Events.contextMenu
  Dblclick     -> on e Events.dblClick
  Drag         -> on e Events.drag
  Dragend      -> on e Events.dragEnd
  Dragenter    -> on e Events.dragEnter
  Dragleave    -> on e Events.dragLeave
  Dragover     -> on e Events.dragOver
  Dragstart    -> on e Events.dragStart
  Drop         -> on e Events.drop
  Error        -> on e Events.error
  Focus        -> on e Events.focus
  Input        -> on e Events.input
  Invalid      -> on e Events.invalid
  Keydown      -> on e Events.keyDown
  Keypress     -> on e Events.keyPress
  Keyup        -> on e Events.keyUp
  Load         -> on e Events.load
  Mousedown    -> on e Events.mouseDown
  Mouseenter   -> on e Events.mouseEnter
  Mouseleave   -> on e Events.mouseLeave
  Mousemove    -> on e Events.mouseMove
  Mouseout     -> on e Events.mouseOut
  Mouseover    -> on e Events.mouseOver
  Mouseup      -> on e Events.mouseUp
  Mousewheel   -> on e Events.mouseWheel
  Scroll       -> on e Events.scroll
  Select       -> on e Events.select
  Submit       -> on e Events.submit
  Wheel        -> on e Events.wheel
  Beforecut    -> on e Events.beforeCut
  Cut          -> on e Events.cut
  Beforecopy   -> on e Events.beforeCopy
  Copy         -> on e Events.copy
  Beforepaste  -> on e Events.beforePaste
  Paste        -> on e Events.paste
  Reset        -> on e Events.reset
  Search       -> on e Events.search
  Selectstart  -> on e Element.selectStart
  Touchstart   -> on e Events.touchStart
  Touchmove    -> on e Events.touchMove
  Touchend     -> on e Events.touchEnd
  Touchcancel  -> on e Events.touchCancel

-- | Register a per-'EventName' listener on the window and return a
-- 'JSM' unsubscribe action. Same dispatcher shape as
-- 'elementOnEventName', with a handful of clipboard/select events
-- returning a no-op unsubscriber because the window itself doesn't
-- expose them via ghcjs-dom's @on@-family. (Upstream marks these
-- with a @--TODO@ comment.)
windowOnEventName :: EventName en -> DOM.Window -> EventM DOM.Window (EventType en) () -> JSM (JSM ())
windowOnEventName en e = case en of
  Abort        -> on e Events.abort
  Blur         -> on e Events.blur
  Change       -> on e Events.change
  Click        -> on e Events.click
  Contextmenu  -> on e Events.contextMenu
  Dblclick     -> on e Events.dblClick
  Drag         -> on e Events.drag
  Dragend      -> on e Events.dragEnd
  Dragenter    -> on e Events.dragEnter
  Dragleave    -> on e Events.dragLeave
  Dragover     -> on e Events.dragOver
  Dragstart    -> on e Events.dragStart
  Drop         -> on e Events.drop
  Error        -> on e Events.error
  Focus        -> on e Events.focus
  Input        -> on e Events.input
  Invalid      -> on e Events.invalid
  Keydown      -> on e Events.keyDown
  Keypress     -> on e Events.keyPress
  Keyup        -> on e Events.keyUp
  Load         -> on e Events.load
  Mousedown    -> on e Events.mouseDown
  Mouseenter   -> on e Events.mouseEnter
  Mouseleave   -> on e Events.mouseLeave
  Mousemove    -> on e Events.mouseMove
  Mouseout     -> on e Events.mouseOut
  Mouseover    -> on e Events.mouseOver
  Mouseup      -> on e Events.mouseUp
  Mousewheel   -> on e Events.mouseWheel
  Scroll       -> on e Events.scroll
  Select       -> on e Events.select
  Submit       -> on e Events.submit
  Wheel        -> on e Events.wheel
  -- Upstream leaves these as no-ops; ghcjs-dom does not expose
  -- Window-scoped variants of these clipboard events. Marking with
  -- pure . pure to hand back a valid no-op unsubscribe action.
  Beforecut    -> const $ return $ return ()
  Cut          -> const $ return $ return ()
  Beforecopy   -> const $ return $ return ()
  Copy         -> const $ return $ return ()
  Beforepaste  -> const $ return $ return ()
  Paste        -> const $ return $ return ()
  Reset        -> on e Events.reset
  Search       -> on e Events.search
  -- Same: no window-scoped selectstart in ghcjs-dom.
  Selectstart  -> const $ return $ return ()
  Touchstart   -> on e Events.touchStart
  Touchmove    -> on e Events.touchMove
  Touchend     -> on e Events.touchEnd
  Touchcancel  -> on e Events.touchCancel
