{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- DOM building, hydration and static rendering on the Effectful Reflex host.
-- Includes DOM event types, effect interpreters, MTL compatibility instances,
-- and widget runners. Typify widgets can use MTL classes over these effects.
module Reflex.Effectful.DomHost
  ( -- * The leaf
    runDomHost
    -- * Events surface
  , EventTag (..)
  , EventName (..)
  , EventResult (..)
  , EventResultType
  , DeltaMode (..)
  , WheelEventResult (..)
  , TouchEventResult (..)
  , TouchResult (..)
    -- * DomSpace + core types
  , DomSpace (..)
  , Namespace
  , DomFragment
  , TextNodeConfig (..)
  , textNodeConfig_initialContents
  , textNodeConfig_setContents
  , TextNode (..)
  , CommentNodeConfig (..)
  , commentNodeConfig_initialContents
  , commentNodeConfig_setContents
  , CommentNode (..)
  , AttributeName (..)
  , mapKeysToAttributeName
  , Propagation (..)
  , EventFlags (..)
  , preventDefault
  , stopPropagation
    -- * Element configs + results
  , ElementConfig (..)
  , elementConfig_namespace
  , elementConfig_initialAttributes
  , elementConfig_eventSpec
  , Element (..)
  , InputElementConfig (..)
  , inputElementConfig_initialValue
  , inputElementConfig_initialChecked
  , inputElementConfig_elementConfig
  , InputElement (..)
  , TextAreaElementConfig (..)
  , textAreaElementConfig_initialValue
  , textAreaElementConfig_elementConfig
  , TextAreaElement (..)
  , RawElementConfig (..)
  , rawElementConfig_eventSpec
  , extractRawElementConfig
  , SelectElementConfig (..)
  , selectElementConfig_initialValue
  , selectElementConfig_elementConfig
  , SelectElement (..)
    -- * DomBuilderE effect
  , DomBuilderE (..)
  , DomBuilderSpaceE
  , DomBuilderTimelineE
  , textNode
  , commentNode
  , element
  , inputElement
  , textAreaElement
  , selectElement
  , placeRawElement
  , wrapRawElement
  , MountableDomBuilderE (..)
  , buildDomFragment
  , mountDomFragment
    -- * HasDocumentE
  , HasDocumentE (..)
  , HasDocumentSpace
  , askDocument
    -- * Event handlers
  , EventType
  , withIsEvent
  , defaultDomEventHandler
  , defaultDomWindowEventHandler
  , elementOnEventName
  , windowOnEventName
  , getKeyEvent
  , getMouseEventCoords
  , getPasteData
  , getTouchEvent
  , getWheelEvent
  , showEventName
    -- * DOM spaces
  , GhcjsDomHandler (..)
  , GhcjsDomHandler1 (..)
  , GhcjsDomEvent (..)
  , GhcjsDomSpace
  , HydrationDomSpace
  , GhcjsEventSpec (..)
  , GhcjsEventFilter (..)
  , GhcjsEventHandler (..)
  , ghcjsEventSpec_filters
  , ghcjsEventSpec_handler
  , Pair1 (..)
  , Maybe1 (..)
    -- * Hydration env + JSM' + DomRenderHookE
  , HydrationState (..)
  , HydrationMode (..)
  , HydrationEnv (..)
  , JSM' (..)
  , DomRenderHookE (..)
  , DomTimeline
  , DomHost
  , SpiderTag
  , WidgetE
  , HydrationRunnerE
  , RunnerAction (..)
  , DelayedHydration
  , askJSM
  , liftJSM
    -- * DomRenderHookE interpreter + send-wrappers
  , runDomRenderHook
  , withRenderHook
  , requestDomAction
  , requestDomAction_
    -- * JSM' interpreter
  , runJSM'
    -- * Wrap.hs: element-listener wiring
  , EventFilterTriggerRef (..)
  , AttachDomListenersE
  , triggerBody
  , triggerBodyDeferred
  , allocateFilterTriggers
  , attachDomListeners
  , wrap
  , wrapDomEvent
  , wrapDomEventMaybe
  , wrapDomEventsMaybe
    -- * Hydration.hs: mode + cursor + delayed queue + runner driver
  , getHydrationMode
  , askUnreadyChildren
  , askCommitAction
  , askParent
  , getParent
  , getPreviousNode
  , setPreviousNode
  , insertAfterPreviousNode
  , localEnv
  , append
  , addHydrationStep
  , addHydrationStepWithSetup
  , removeSubsequentNodes
  , runHydrationRunner
  , runHydrationRunnerWithFailure
    -- * Element.hs foundational block
  , ChildReadyState (..)
  , TraverseChildImmediate (..)
  , TraverseChildHydration (..)
  , TraverseChild (..)
  , drawChildUpdate
  , mapPatchDMap
  , mapPatchDMapWithMove
  , insertBefore
  , insertBefore_
  , deleteBetweenExclusive
  , deleteUpTo
  , collectUpTo
  , extractBetweenExclusive
  , extractUpTo
  , collectUpToGivenParent
  , makeElement
  , textNodeImmediate
  , commentNodeImmediate
    -- * Element.hs hydration walkers + node internals
  , hydrateTextNode
  , hydrateComment
  , skipToAndReplaceComment
  , skipToReplaceStart
  , skipToReplaceEnd
  , textNodeInternal
  , commentNodeInternal
  , skipHydrationAttribute
  , hydratableAttribute
    -- * Element.hs element cluster
  , elementImmediate
  , elementInternal
  , hydrateElement
  , shouldSkipHydration
  , driveChildRunner
    -- * Element.hs focus helper
  , mkHasFocus
  , focusChangeEv
    -- * Element.hs form-element cluster
  , inputElementImmediate
  , inputElementInternal
  , textAreaElementImmediate
  , textAreaElementInternal
  , selectElementImmediate
  , selectElementInternal
    -- * Element.hs adjust drivers
  , runWithReplaceEff
  , hoistTraverseWithKeyWithAdjust
  , hoistTraverseIntMapWithKeyWithAdjust
  , numberOccurrencesEff
    -- * Instances.hs — DomBuilderE Effect GADT interpreters
  , runHasDocumentGhcjs
  , runHasDocumentHydration
  , runDomBuilderImmediate
  , runDomBuilderHydration
  , updateIntMapChildUnreadiness
  , applyDomUpdateIntMap
  , updateDMapChildUnreadiness
  , applyDomUpdateDMap
  , updateDMapWithMoveChildUnreadiness
  , applyDomUpdateDMapWithMove
  , insertBeforeJSM
  , deleteUpToJSM
  , collectUpToJSM
    -- * Main.hs host bootstrap (immediate mode) — upstream-mirroring
  , withDomHost
  , mainWidget
  , mainWidget'
  , mainWidgetInElementById
  , mainWidgetWithCss
  , mainWidgetWithHead
  , mainWidgetWithHead'
  , attachWidget
  , attachWidget'
  , attachWidget''
  , attachImmediateWidget
  , runImmediateWidgetWithHeadAndBody
  , hostPerformEventT
  , newEventWithTriggerRef
  , processAsyncEvents
  , EventChannel
  , FireCommand (..)
  , ImmediateWidget
  , WidgetEff (..)
  , runWidgetEff
  , liftEff
  , replaceElementContents
    -- * Recursive-app bootstrap (upstream Reflex.Dom.Main:runApp')
  , runApp'
  , AppInput (..)
  , AppOutput (..)
  , Window (..)
  , WindowConfig (..)
  , wrapWindow
    -- * Explicit guest effect rows
  , GuestEff
  , WidgetRowE
  , WidgetE_AE
    -- * Prerender — HasPrerender class + bridge to upstream Reflex.Dom.Prerender.Prerender
  , HasPrerender (..)
  , prerender_
  , PrerenderBaseConstraintsE
  , PrerenderClientConstraintE
  , UnrunnableEs
  , unrunnable
  , startMarker
  , endMarker
  , deleteToPrerenderEnd
  , runUnrunnableDomBuilder
  , runUnrunnableNotReady
  , runUnrunnableAdjustable
  , runUnrunnablePerformEvent
  , runUnrunnablePerformEventH
  , runUnrunnableHasDocument
  , runUnrunnableTriggerEvent
  , runUnrunnableHold
  , runUnrunnableSample
  , runUnrunnablePostBuild
  , runUnrunnableDomRenderHook
  , runUnrunnableJSM
    -- * Main.hs host bootstrap (hydration mode) — upstream-mirroring
  , HydrationWidget
  , HydrationWidgetE
  , HydrationWidgetRow
  , HydrationWidgetSpec
  , FloatingWidgetE
  , ImmediateWidgetE
  , attachHydrationWidget
  , attachHydrationWidgetWithFailure
  , withAttachHydrationWidgetWithFailure
  , mainHydrationWidget
  , mainHydrationWidgetWithHead
  , mainHydrationWidgetWithHead'
  , mainHydrationWidgetWithSwitchoverAction
  , mainHydrationWidgetWithSwitchoverAction'
  , mainHydrationWidgetWithSwitchoverActionWithFailure'
  , runHydrationWidgetWithHeadAndBody
  , runHydrationWidgetWithHeadAndBodyWithFailure
    -- * JSM-per-event helpers
  , performJSMEvent
  , performJSMEvent_
    -- * Widget-vocabulary convenience wrappers
  , text
  , el
  , el'
  , elAttr
  , elAttr'
  , elClass
    -- * Static-rendering surface (Reflex.Dom.Core analog)
  , StaticDomBuilderEnv (..)
  , StaticDomSpace
  , StaticDomEvent
  , StaticDomHandler (..)
  , StaticEventSpec (..)
  , StaticDomTimeline
  , StaticE
  , runHasDocumentStatic
  , runDomBuilderStatic
  , runTriggerEventStatic
  , runNotReadyStatic
  , runAdjustableStatic
  , renderStatic
  , replaceStartStatic
  , replaceEndStatic
    -- * Elemental lenses used by Static.hs port + downstream
  , elementConfig_modifyAttributes
  , textAreaElementConfig_setValue
  , selectElementConfig_setValue
  , inputElementConfig_setValue
  , inputElementConfig_setChecked
    -- * Widget/Basic + Widget/Input surface
    -- ** Type-class attribute plumbing
  , InitialAttributes (..)
  , ModifyAttributes (..)
  , HasDomEvent (..)
  , HasSetValue (..)
  , HasAttributes (..)
  , HasValue (..)
    -- ** Convenience
  , (=:)
  , blank
    -- ** Dynamic-attribute widget helpers (Widget/Basic)
  , dynText
  , display
  , dyn
  , dyn_
  , widgetHold
  , widgetHold_
  , networkView
  , networkHold
  , untilReady
  , dynamicAttributesToModifyAttributes
  , dynamicAttributesToModifyAttributesWithInitial
  , elDynAttr
  , elDynAttr'
  , elDynAttrNS
  , elDynAttrNS'
  , elDynClass
  , Link (..)
  , link
  , linkClass
  , divClass
  , dtdd
    -- ** Widget/Input widgets
  , TextInput (..)
  , _textInput_element
  , TextInputConfig (..)
  , textInput
  , textInputGetEnter
  , keypress
  , keydown
  , keyup
  , keycodeEnter
  , keycodeEscape
  , RangeInput (..)
  , RangeInputConfig (..)
  , rangeInput
  , TextArea (..)
  , TextAreaConfig (..)
  , textArea
  , Checkbox (..)
  , CheckboxConfig (..)
  , checkbox
  , FileInput (..)
  , FileInputConfig (..)
  , fileInput
    -- * Collection combinators (Reflex.Collection re-exports)
  , listWithKey
  , listHoldWithKey
  , listWithKeyShallowDiff
  , listViewWithKey
  , selectViewListWithKey
  , selectViewListWithKey_
  , list
  , simpleList
    -- * Dropdown
  , Dropdown (..)
  , DropdownConfig (..)
  , dropdown
    -- * Table + tab display
  , tableDynAttr
  , tabDisplay
    -- * WebSocket surface
  , WebSocketConfig (..)
  , WebSocket_
  , RawWebSocket (..)
  , webSocket
  , webSocket'
  , textWebSocket
  , jsonWebSocket
  , JSWebSocket (..)
  , IsWebSocketMessage (..)
  , closeWebSocket
  , newWebSocket
  , onBSMessage
  , forkJSM
  , webSocketConfig_send
  , webSocketConfig_close
  , webSocketConfig_reconnect
  , webSocketConfig_protocols
  , webSocket_recv
  , webSocket_open
  , webSocket_error
  , webSocket_close
    -- * JSON / bytestring helpers used by WebSocket
  , jsonDecode
  , js_jsonParse
  , bsToArrayBuffer
  , bsFromMutableArrayBuffer
    -- * Xhr surface
  , XhrException (..)
  , XhrResponseType (..)
  , XhrResponseBody (..)
  , XhrRequest (..)
  , XhrRequestConfig (..)
  , XhrResponse (..)
  , XhrResponseHeaders (..)
  , IsXhrPayload (..)
  , XMLHttpRequest
  , xhrRequest
  , runXhrJSM
  , parseAllHeadersString
  , newXMLHttpRequest
  , newXMLHttpRequestWithError
  , newXMLHttpRequestIO
  , newXMLHttpRequestWithErrorIO
  , performRequestAsync
  , performRequestAsyncWithError
  , performRequestAsync'
  , performMkRequestAsync
  , performRequestsAsync
  , performRequestsAsyncWithError
  , performRequestsAsync'
  , performMkRequestsAsync
  , getAndDecode
  , postJson
  , getMay
  , decodeText
  , decodeXhrResponse
    -- ** Xhr.Foreign wrappers
  , xmlHttpRequestNew
  , xmlHttpRequestOpen
  , xmlHttpRequestSend
  , xmlHttpRequestSetRequestHeader
  , xmlHttpRequestAbort
  , xmlHttpRequestGetAllResponseHeaders
  , xmlHttpRequestGetResponseHeader
  , xmlHttpRequestOverrideMimeType
  , xmlHttpRequestOnreadystatechange
  , xmlHttpRequestSetTimeout
  , xmlHttpRequestGetTimeout
  , xmlHttpRequestGetReadyState
  , xmlHttpRequestSetWithCredentials
  , xmlHttpRequestGetWithCredentials
  , xmlHttpRequestGetResponseText
  , xmlHttpRequestSetResponseType
  , xmlHttpRequestGetResponseType
  , xmlHttpRequestGetStatus
  , xmlHttpRequestGetStatusText
  , xmlHttpRequestGetResponse
  , fromResponseType
  , toResponseType
    -- ** Xhr record lenses
  , xhrRequest_method
  , xhrRequest_url
  , xhrRequest_config
  , xhrRequestConfig_headers
  , xhrRequestConfig_user
  , xhrRequestConfig_password
  , xhrRequestConfig_responseType
  , xhrRequestConfig_sendData
  , xhrRequestConfig_withCredentials
  , xhrRequestConfig_responseHeaders
  , xhrResponse_status
  , xhrResponse_statusText
  , xhrResponse_response
  , xhrResponse_responseText
  , xhrResponse_headers
    -- ** Xhr.FormData
  , FormValue (..)
  , postForms
  , postForms'
  , fileToFormValue
    -- * Widget.Lazy
  , virtualList
  , virtualListBuffered
  , virtualListWithSelection
    -- * Widget.Resize
  , resizeDetector
  , resizeDetectorWithStyle
  , resizeDetectorWithAttrs
    -- * Location
  , browserHistoryWith
  , getLocationAfterHost
  , getLocationFragment
  , getLocationHost
  , getLocationPath
  , getLocationProtocol
  , getLocationUrl
  , manageHistory
  , manageHistory'
  , manageHistoryExposingExternalUpdates
  , HistoryCommand (..)
  , HistoryStateUpdate (..)
  , HistoryItem (..)
  , getLocationUri
  , popHistoryState
    -- * Convenience re-exports (from Reflex.Class / Data.Default)
  , R.Reflex, R.Event, R.Behavior, R.Dynamic, R.ffor, R.leftmost
  , def
    -- * No-suffix aliases for Effectful DomBuilder families (backwards-compat)
  , DomBuilder, DomBuilderSpace, DomBuilderTimeline
  , elClass'
  ) where

import qualified Prelude                                     as P
import           Unsafe.Coerce                               (unsafeCoerce)
import           Prelude                                     (Applicative (..),
                                                              Bool (..),
                                                              Double, Either (..),
                                                              Eq,
                                                              Float,
                                                              Functor (..),
                                                              IO, Int, Maybe (..),
                                                              Monad (..),
                                                              Monoid (..), Ord,
                                                              Read,
                                                              Semigroup (..),
                                                              Show, String, Word,
                                                              (||), (.), ($),
                                                              ($!),
                                                              (=<<), (<$>),
                                                              (/=), (==),
                                                              (>>), (+),
                                                              const, either,
                                                              error, fst,
                                                              fromIntegral,
                                                              maybe, putStrLn,
                                                              snd)

-- eff-reflex (package name: `reflex`)
import qualified Reflex.Class                                as R
import qualified Reflex.Host.Class                           as RH
import qualified Reflex.Host.HeadlessEffectful               as HH
import qualified Reflex.Spider.Internal                      as R.SI

import           JSDOM.Extras.JSM                            (JSM' (..), askJSM, liftJSM)

-- Events-surface deps
import           Data.Kind                                   (Type)
import           Data.Type.Equality                          (type (~))
import           Data.Text                                   (Text)

-- DomSpace + core types deps
import           Control.Lens                                (Lens, Lens')
import           Data.Default                                (Default (..))
import qualified Data.Dependent.Map                          as DMap
import           Data.Dependent.Map                          (DMap)
import           Data.Functor.Misc                           (WrapArg (..))
import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map
import           Data.Maybe                                  (fromMaybe)
import           Data.String                                 (IsString (..))

-- Element-configs deps
import qualified GHCJS.DOM.Types                             as DOM

-- Effectful deps
import           Effectful                                   (Effect,
                                                              DispatchOf,
                                                              Dispatch (Dynamic),
                                                              (:>), Eff, IOE,
                                                              runEff,
                                                              raise)
import qualified Effectful                                   as E
import           Effectful.Dispatch.Dynamic                  (send, reinterpret,
                                                              interpret,
                                                              interpose,
                                                              localLend,
                                                              localUnlift,
                                                              localUnliftIO,
                                                              localSeqUnliftIO)
import           Effectful.Reader.Static                     (Reader, ask, local,
                                                              runReader)
import           Effectful.State.Static.Local                (State, evalState,
                                                              get, gets, modify,
                                                              put, runState)

-- RenderHook interpreter deps
import           Control.Monad                               (void, forever)
import           Control.Monad.IO.Class                      (MonadIO, liftIO)
import qualified Reflex.Class                                as RC
import           Language.Javascript.JSaddle                 (runJSM)

-- Wrap.hs deps
import           Control.Concurrent                          (forkIO)
import           Control.Concurrent.Chan                     (Chan, newChan,
                                                              readChan,
                                                              writeChan)
import           Control.Concurrent.MVar                     (MVar,
                                                              newEmptyMVar,
                                                              putMVar, readMVar,
                                                              takeMVar)
import           Control.Lens                                (imapM_, (^.), (&), (.~), (%~))
import           Control.Monad                               (forM, when)
import           Data.Dependent.Sum                          (DSum ((:=>)))
import           Data.Foldable                               (forM_, for_,
                                                              traverse_)
import           Data.Traversable                            (Traversable (..), for)
import           Data.IORef                                  (atomicModifyIORef',
                                                              modifyIORef',
                                                              newIORef,
                                                              readIORef,
                                                              writeIORef)
import           Effectful.Reader.Static                     (asks)
import qualified GHCJS.DOM.EventM                            as EventM
import qualified GHCJS.DOM.Node                              as Node
import           GHCJS.DOM.Types                             (IsNode, toNode)
import qualified Language.Javascript.JSaddle                 as JSaddle
import           Language.Javascript.JSaddle                 (ghcjsPure,
                                                              isTruthy)

-- Element.hs foundational deps
import           Data.Functor.Compose                        (Compose (..))
import           Data.Patch.DMap                             (mapPatchDMap)
import           Data.Patch.DMapWithMove                     (mapPatchDMapWithMove)
import qualified GHCJS.DOM.Document                          as Document
import qualified GHCJS.DOM.Element                           as Element
import           GHCJS.DOM.Types                             (DocumentFragment)

import qualified Data.Text                                   as T
import qualified GHCJS.DOM.Text                              as Text
import qualified GHCJS.DOM.DocumentOrShadowRoot              as DocShadow

-- Form elements
import qualified GHCJS.DOM.HTMLInputElement                  as Input
import qualified GHCJS.DOM.HTMLSelectElement                 as Select
import qualified GHCJS.DOM.HTMLTextAreaElement               as TextArea
import qualified GHCJS.DOM.FileList                          as FileList
import qualified Reflex.Dynamic                              as RD
import           Data.Maybe                                  (catMaybes, isNothing, listToMaybe)
import           Control.Monad.Fix                           (MonadFix)

-- Prerendering
import           Effectful                                   (withEffToIO)
import           Effectful.Internal.Unlift                   (UnliftStrategy (..),
                                                              Persistence (..),
                                                              Limit (..))

-- Adjustable DOM collections
import qualified Data.Patch                                  as Patch
import           Data.Patch                                  (Patch, PatchTarget)
import           Data.Functor.Constant                       (Constant (..))
import           Data.Functor.Misc                           (ComposeMaybe (..),
                                                              getComposeMaybe,
                                                              weakenDMapWith)
import           Data.Functor.Identity                       (Identity (..),
                                                              runIdentity)
import           Data.Functor.Product                        (Product (Pair))
import           Data.Some                                   (Some (..))
import           Data.GADT.Compare                           (GCompare)
import qualified Data.IntMap.Strict                          as IntMap
import           Data.IntMap.Strict                          (IntMap)
import qualified Data.Map.Strict                             as MapS
import           Data.Patch.DMap                             (PatchDMap (..))
import qualified Data.Patch.DMap                             as PatchDMap
import           Data.Patch.DMapWithMove                     (PatchDMapWithMove)
import qualified Data.Patch.DMapWithMove                     as PatchDMapWithMove
import           Data.Patch.IntMap                           (PatchIntMap (..))
import           Data.Patch.Map                              (PatchMap (..))
import           Data.Patch.MapWithMove                      (PatchMapWithMove (..))
import qualified Data.Patch.MapWithMove                      as PatchMapWithMove

import           Effectful.Dispatch.Dynamic                  (localSeqUnlift)
import           Data.Maybe                                  (isJust)
import qualified Language.Javascript.JSaddle                 as JS
import qualified Data.FastMutableIntMap                      as FastMutableIntMap

-- Application hosting
import qualified Language.Javascript.JSaddle.Warp            as Warp
import qualified GHCJS.DOM                                   as GHCJS.DOM
import           GHCJS.DOM.Document                          (getBodyUnchecked, getDefaultViewUnchecked)
import qualified GHCJS.DOM.NonElementParentNode              as NonElementParentNode
import           GHCJS.DOM.Types                             (toDocument)
import           System.Environment                          (lookupEnv)
import           Text.Read                                   (readMaybe)
import qualified Text.Read                                   as TR
import           Data.ByteString                             (ByteString)
import           Data.Text.Encoding                          (decodeUtf8, encodeUtf8)

-- Static.hs port deps
import           Blaze.ByteString.Builder.Html.Utf8          (fromHtmlEscapedText)
import qualified Data.ByteString.Builder                     as BSB
import           Data.ByteString.Builder                     (Builder)
import qualified Data.ByteString.Lazy                        as LBS
import qualified Data.Set                                    as Set
import qualified Data.Map.Misc                               as Map.Misc
import qualified Control.Monad                               as Monad
import qualified Reflex.Adjustable.Class                     as RA
import qualified Reflex.Network                              as RNet
import qualified Reflex.Collection                           as RColl
import qualified Data.Bimap                                  as Bimap
-- WebSocket port deps
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as WSLBS
import qualified GHCJS.Buffer                                as JSBuf
import qualified JavaScript.TypedArray.ArrayBuffer           as JSTAB
import qualified GHCJS.DOM.WebSocket                         as DOMWS
import           GHCJS.DOM.WebSocket                         (getReadyState)
import qualified GHCJS.DOM.CloseEvent                        as DOMCloseEvent
import qualified GHCJS.DOM.MessageEvent                      as DOMMessageEvent
import           GHCJS.DOM.Types                             (ArrayBuffer (..), WebSocket (..))
import qualified GHCJS.DOM.Types                             as DOMTypes (fromJSValUnchecked)
import qualified GHCJS.Foreign                               as GHCJSFor
import           Language.Javascript.JSaddle                 (fun, jsg, js2, jss)
import qualified Language.Javascript.JSaddle                 as JSaddleWS
import           Language.Javascript.JSaddle                 (MonadJSM)
import           Language.Javascript.JSaddle.Helper          (mutableArrayBufferFromJSVal)
import           Language.Javascript.JSaddle.Types           (JSString, JSVal, jsval)
import qualified Language.Javascript.JSaddle.Monad           as JSMonad
import           Data.JSString.Text                          (textToJSString)
import           Data.Bifoldable                             (bitraverse_)
import qualified Control.Concurrent.STM                      as STM
import qualified Data.Aeson                                  as Aeson
import           Data.Aeson                                  (FromJSON, ToJSON, encode)
import qualified Control.Exception                           as CE
import           Control.Concurrent                          (threadDelay)
import           Control.Monad                               ((>=>), unless, (<=<))
-- Xhr port deps
import qualified Data.CaseInsensitive                        as CI
import           Data.CaseInsensitive                        (CI)
import qualified Data.List                                   as XhrList
import qualified Data.Set                                    as XhrSet
import qualified Data.Text.Lazy                              as XhrLT
import qualified Data.Text.Lazy.Builder                      as XhrLB
import           Data.Aeson.Text                             (encodeToTextBuilder)
import           Control.Exception                           (handle)
import qualified GHCJS.DOM.XMLHttpRequest                    as DOMXhr
import           GHCJS.DOM.XMLHttpRequest                    (XMLHttpRequest, XHRError (..))
import           GHCJS.DOM.Enums                             (XMLHttpRequestResponseType (..))
import qualified GHCJS.DOM.EventM                            as XhrEventM
import           GHCJS.DOM.Types                             (Blob (..), ArrayBufferView (..), FormData, castTo)
import qualified GHCJS.DOM.Types                             as DOMTypesXhr (Event)
import qualified GHCJS.DOM.FormData                          as DOMFD
import           GHCJS.DOM.File                              (getName)
import           GHCJS.DOM.Types                             (File, IsBlob)
import qualified Reflex.PerformEvent.Class                   as RPE
import qualified Reflex.TriggerEvent.Class                   as RTE
import qualified Reflex.PostBuild.Class                      as RPB
import qualified Control.Monad.Ref                            as MRef
-- Widget/Input port deps
import qualified Web.KeyCode                                 as WebKC
import qualified Data.Align                                  as Align
import           Data.These                                  (These (..))
import           GHCJS.DOM.HTMLInputElement                  (HTMLInputElement)
import           GHCJS.DOM.HTMLTextAreaElement               (HTMLTextAreaElement)

-- Types.hs deps
import           Data.IORef                                  (IORef)
import           GHCJS.DOM.Types                             (Document,
                                                              JSContextRef, Node)

-- EventHandlers deps
import qualified GHCJS.DOM.ClipboardEvent                    as ClipboardEvent
import qualified GHCJS.DOM.DataTransfer                      as DataTransfer
import           GHCJS.DOM.EventM                            (EventM, event, on)
import qualified GHCJS.DOM.GlobalEventHandlers               as Events
import           GHCJS.DOM.Types                             (ClipboardEvent,
                                                              JSM,
                                                              WheelEvent)
import qualified GHCJS.DOM.UIEvent                           as UIEvent ()
import qualified GHCJS.DOM.WheelEvent                        as WheelEvent
import qualified GHCJS.DOM.Window                            as Window

-- Space deps
import           GHCJS.DOM.Types                             (uncheckedCastTo)

-- Retrofit imports: shared pure types from variant B's reflex-dom-core.
-- Their names are identical to what we previously inlined, so downstream
-- code in DomHost.hs continues to compile without further change.
import           Reflex.Dom.Builder.Class                    (Element (..), ElementConfig (..),
                                                              TextNode (..), TextNodeConfig (..),
                                                              CommentNode (..), CommentNodeConfig (..),
                                                              InputElement (..), InputElementConfig (..),
                                                              TextAreaElement (..), TextAreaElementConfig (..),
                                                              SelectElement (..), SelectElementConfig (..),
                                                              RawElementConfig (..),
                                                              AttributeName (..), Namespace,
                                                              Propagation (..), EventFlags (..),
                                                              DomSpace (..),
                                                              InitialAttributes (..),
                                                              ModifyAttributes (..),
                                                              HasDomEvent (..),
                                                              HasSetValue (..),
                                                              elementConfig_namespace,
                                                              elementConfig_initialAttributes,
                                                              elementConfig_modifyAttributes,
                                                              elementConfig_eventSpec,
                                                              inputElementConfig_initialValue,
                                                              inputElementConfig_setValue,
                                                              inputElementConfig_initialChecked,
                                                              inputElementConfig_setChecked,
                                                              inputElementConfig_elementConfig,
                                                              textAreaElementConfig_initialValue,
                                                              textAreaElementConfig_setValue,
                                                              textAreaElementConfig_elementConfig,
                                                              selectElementConfig_initialValue,
                                                              selectElementConfig_setValue,
                                                              selectElementConfig_elementConfig,
                                                              rawElementConfig_eventSpec,
                                                              stopPropagation,
                                                              preventDefault,
                                                              textNodeConfig_initialContents,
                                                              textNodeConfig_setContents,
                                                              commentNodeConfig_initialContents,
                                                              commentNodeConfig_setContents,
                                                              mapKeysToAttributeName,
                                                              extractRawElementConfig)
import           Reflex.Dom.Builder.Class.Events             (EventTag (..),
                                                              EventName (..),
                                                              EventResult (..),
                                                              EventResultType,
                                                              DeltaMode (..),
                                                              WheelEventResult (..),
                                                              TouchEventResult (..),
                                                              TouchResult (..))
-- Space types + event-handler machinery (Blocker-1 retrofit).
import           Reflex.Dom.Builder.Immediate                (GhcjsDomSpace,
                                                              HydrationDomSpace,
                                                              GhcjsDomHandler (..),
                                                              GhcjsDomHandler1 (..),
                                                              GhcjsDomEvent (..),
                                                              GhcjsEventSpec (..),
                                                              GhcjsEventFilter (..),
                                                              GhcjsEventHandler (..),
                                                              ghcjsEventSpec_filters,
                                                              ghcjsEventSpec_handler,
                                                              EventType,
                                                              withIsEvent,
                                                              defaultDomEventHandler,
                                                              defaultDomWindowEventHandler,
                                                              elementOnEventName,
                                                              windowOnEventName,
                                                              getKeyEvent,
                                                              getMouseEventCoords,
                                                              getTouchEvent,
                                                              showEventName)
import           Reflex.Dom.Builder.Static                   (StaticDomSpace,
                                                              StaticDomEvent,
                                                              StaticDomHandler (..),
                                                              StaticEventSpec (..))

-- Widget.Lazy + Widget.Resize deps
import           Data.Fixed                                  (divMod')
import qualified GHCJS.DOM.HTMLElement                       as DOMHTMLElement
import qualified GHCJS.DOM.Types                             as DOMTypes' (HTMLElement (..))
import qualified Reflex.Time                                 as RTime

-- Location deps
import qualified GHCJS.DOM.Location                          as DOMLocation
import qualified GHCJS.DOM.History                           as DOMHistory
import qualified GHCJS.DOM.PopStateEvent                     as PopStateEvent
import qualified GHCJS.DOM.WindowEventHandlers               as WinEvtHandlers
import           GHCJS.DOM.Types                             (Location, History, SerializedScriptValue (..))
import           Language.Javascript.JSaddle                 (FromJSString, ToJSString, ToJSVal (..), FromJSVal (..), fromJSValUnchecked, js1)
import           Network.URI                                 (URI (..), URIAuth (..))
import qualified Reflex.Class                                as RClass (mapAccumMaybeB, fanEither)

-- MTL-class instance imports (bilingual pattern like eff-reflex).
import qualified Reflex.Dom.Builder.Class                    as RDBC
import qualified Reflex.Dom.Prerender                        as RDPR
import qualified Reflex.Adjustable.Class                     as RAdj
import           Control.Monad.Primitive                     (PrimMonad (..))
import qualified Effectful.Prim                              as Effectful.Prim
import           Control.Monad.Trans.Class                   (lift)
import           Control.Monad.Trans.Reader                  (ReaderT (..))
import           Data.Void                                   (Void)
-- The magic. Every widget monad in this module is wrapped in
-- WithJSContextSingleton x, giving 'MonadJSM (Performable m)' and every
-- other MTL instance variant B's widget code requires — for free.
import           Foreign.JavaScript.TH                       (WithJSContextSingleton (..),
                                                              JSContextSingleton (..),
                                                              withJSContextSingleton,
                                                              runWithJSContextSingleton)
import qualified Reflex.NotReady.Class                       as RNR
import qualified Reflex.EventWriter.Class                    as REWC
import qualified Reflex.EventWriter.Base                     as REWB
import qualified Reflex.DynamicWriter.Class                  as RDWC
import qualified Reflex.BehaviorWriter.Class                 as RBWC
import qualified Language.Javascript.JSaddle.Types           as JSaddleTypes

-- =====================================================================
-- the leaf.
-- =====================================================================

runDomHost
  :: R.SI.HasSpiderTimeline R.SI.Global
  => HH.EffectfulHost R.SI.Global a
  -> P.IO a
runDomHost = HH.runEffectfulHost

-- =====================================================================
data DomBuilderE (t :: Type) (space :: Type) :: Effect where
  DB_TextNode
    :: TextNodeConfig t
    -> DomBuilderE t space m (TextNode space t)
  DB_CommentNode
    :: CommentNodeConfig t
    -> DomBuilderE t space m (CommentNode space t)
  DB_Element
    :: Text
    -> ElementConfig er t space
    -> m a
    -> DomBuilderE t space m (Element er space t, a)
  DB_InputElement
    :: InputElementConfig er t space
    -> DomBuilderE t space m (InputElement er space t)
  DB_TextAreaElement
    :: TextAreaElementConfig er t space
    -> DomBuilderE t space m (TextAreaElement er space t)
  DB_SelectElement
    :: SelectElementConfig er t space
    -> m a
    -> DomBuilderE t space m (SelectElement er space t, a)
  DB_PlaceRawElement
    :: RawElement space
    -> DomBuilderE t space m ()
  DB_WrapRawElement
    :: RawElement space
    -> RawElementConfig er t space
    -> DomBuilderE t space m (Element er space t)

type instance DispatchOf (DomBuilderE t space) = 'Dynamic

-- | Given an effect row, resolve to the 'space' phantom of the row's
-- 'DomBuilderE' effect. Recurses through the row until it finds the
-- first 'DomBuilderE t space' constructor.
type family DomBuilderSpaceE (es :: [Effect]) :: Type where
  DomBuilderSpaceE (DomBuilderE _ space : _)    = space
  DomBuilderSpaceE (_                  : rest) = DomBuilderSpaceE rest

-- | Companion to 'DomBuilderSpaceE' — recovers the 'Reflex' timeline @t@
-- from an effect row's 'DomBuilderE' entry.
type family DomBuilderTimelineE (es :: [Effect]) :: Type where
  DomBuilderTimelineE (DomBuilderE t _ : _)    = t
  DomBuilderTimelineE (_              : rest) = DomBuilderTimelineE rest

-- | Downstream-convenience aliases (no-E suffix) for consumers that used
-- the older upstream-MTL-style names before the -E rename settled.
type DomBuilderSpace   es = DomBuilderSpaceE   es
type DomBuilderTimeline es = DomBuilderTimelineE es
-- | 'DomBuilder' alias for 'DomBuilderE' (the Effectful builder effect).
type DomBuilder t space = DomBuilderE t space

-- =====================================================================
-- MTL-class instance for variant B's 'RDBC.DomBuilder' on @Eff es@.
--
-- Follows eff-reflex's bilingual pattern: variant B's
-- MTL-shape widget code (Reflex.Dom.Widget.Basic, .Input, .Lazy, .Resize,
-- Reflex.Dom.Location, .WebSocket, .Xhr) becomes directly callable on
-- @Eff es@ whenever 'DomBuilderE' is in the row — no re-implementation
-- needed on our side.
--
-- Each method sends the corresponding constructor of our 'DomBuilderE'
-- Effect GADT. The associated 'DomBuilderSpace' type family reduces to
-- our row-recovering 'DomBuilderSpaceE'.
-- =====================================================================
instance
  ( R.Reflex t
  , t ~ DomBuilderTimelineE es
  , DomBuilderE t (DomBuilderSpaceE es) :> es
  , DomSpace (DomBuilderSpaceE es)
  , RAdj.Adjustable t (Eff es)
  , RNR.NotReady t (Eff es)
  ) => RDBC.DomBuilder t (Eff es) where
  type DomBuilderSpace (Eff es) = DomBuilderSpaceE es
  textNode        cfg          = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_TextNode        cfg         )
  commentNode     cfg          = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_CommentNode     cfg         )
  element         tag cfg body = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_Element         tag cfg body)
  inputElement    cfg          = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_InputElement    cfg         )
  textAreaElement cfg          = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_TextAreaElement cfg         )
  selectElement   cfg body     = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_SelectElement   cfg body    )
  placeRawElement raw          = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_PlaceRawElement raw         )
  wrapRawElement  raw cfg      = send @(DomBuilderE t (DomBuilderSpaceE es)) (DB_WrapRawElement  raw cfg     )
  {-# INLINABLE textNode        #-}
  {-# INLINABLE commentNode     #-}
  {-# INLINABLE element         #-}
  {-# INLINABLE inputElement    #-}
  {-# INLINABLE textAreaElement #-}
  {-# INLINABLE selectElement   #-}
  {-# INLINABLE placeRawElement #-}
  {-# INLINABLE wrapRawElement  #-}

-- | Lifting instance: 'RNR.NotReady' through 'WithJSContextSingleton'.
-- variant B provides most 'WithJSContextSingleton' lifting instances but
-- omitted NotReady (upstream didn't need it because their widget stacks
-- have NotReady-carrying transformers ABOVE WithJSContextSingleton).
instance (RNR.NotReady t m, Monad m) => RNR.NotReady t (WithJSContextSingleton x m) where
  notReadyUntil e = WithJSContextSingleton (RNR.notReadyUntil e)
  notReady        = WithJSContextSingleton RNR.notReady
  {-# INLINABLE notReadyUntil #-}
  {-# INLINABLE notReady      #-}

-- | Lifting instance: 'RDBC.DomBuilder' through 'WithJSContextSingleton'.
-- All methods use the class's default 'MonadTransControl' machinery
-- (WithJSContextSingleton has a MonadTransControl instance in variant B),
-- so no method bodies here.
--
-- Needed because our widget monad is 'WithJSContextSingleton x (Eff es)'
-- (variant B's DomBuilder-carrying transformer sits UNDER
-- WithJSContextSingleton in their stack; ours has WithJSContextSingleton
-- at the top, so we owe the lift).
instance RDBC.DomBuilder t m => RDBC.DomBuilder t (WithJSContextSingleton x m) where
  type DomBuilderSpace (WithJSContextSingleton x m) = RDBC.DomBuilderSpace m

textNode
  :: forall t es
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => TextNodeConfig t
  -> Eff es (TextNode (DomBuilderSpaceE es) t)
textNode cfg = send (DB_TextNode cfg)
{-# INLINABLE textNode #-}

commentNode
  :: forall t es
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => CommentNodeConfig t
  -> Eff es (CommentNode (DomBuilderSpaceE es) t)
commentNode cfg = send (DB_CommentNode cfg)
{-# INLINABLE commentNode #-}

element
  :: forall t er es a
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => Text
  -> ElementConfig er t (DomBuilderSpaceE es)
  -> Eff es a
  -> Eff es (Element er (DomBuilderSpaceE es) t, a)
element elementTag cfg child = send (DB_Element elementTag cfg child)
{-# INLINABLE element #-}

inputElement
  :: forall t er es
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => InputElementConfig er t (DomBuilderSpaceE es)
  -> Eff es (InputElement er (DomBuilderSpaceE es) t)
inputElement cfg = send (DB_InputElement cfg)
{-# INLINABLE inputElement #-}

textAreaElement
  :: forall t er es
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => TextAreaElementConfig er t (DomBuilderSpaceE es)
  -> Eff es (TextAreaElement er (DomBuilderSpaceE es) t)
textAreaElement cfg = send (DB_TextAreaElement cfg)
{-# INLINABLE textAreaElement #-}

selectElement
  :: forall t er es a
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => SelectElementConfig er t (DomBuilderSpaceE es)
  -> Eff es a
  -> Eff es (SelectElement er (DomBuilderSpaceE es) t, a)
selectElement cfg child = send (DB_SelectElement cfg child)
{-# INLINABLE selectElement #-}

-- ---------------------------------------------------------------------
-- Widget-vocabulary convenience wrappers (upstream
-- Reflex.Dom.Widget.Basic — verbatim porting).
-- ---------------------------------------------------------------------

-- | Emit static text. Analog of upstream
-- @text :: DomBuilderE t m => Text -> m ()@.
--
-- The timeline @t@ is derived from the effect row via
-- 'DomBuilderTimelineE' so callers with an ambient
-- 'ImmediateWidgetE' / 'HydrationWidgetE' don't need explicit
-- type applications.
text
  :: forall es
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     )
  => Text -> Eff es ()
text t = void $ textNode @(DomBuilderTimelineE es) $
  (def :: TextNodeConfig (DomBuilderTimelineE es)) & textNodeConfig_initialContents .~ t
{-# INLINABLE text #-}

-- | Create a DOM element with no attributes, returning the child result.
-- Analog of upstream @el :: DomBuilderE t m => Text -> m a -> m a@.
--
-- >>> el "div" (text "Hello World")  --> <div>Hello World</div>
el
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Eff es a -> Eff es a
el elementTag child = P.snd <$> el' elementTag child
{-# INLINABLE el #-}

-- | 'el' variant that returns the built 'Element' alongside the child
-- result. Analog of upstream @el'@.
el'
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text
  -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpaceE es) (DomBuilderTimelineE es), a)
el' elementTag =
  element @(DomBuilderTimelineE es) elementTag
    (def :: ElementConfig EventResult (DomBuilderTimelineE es) (DomBuilderSpaceE es))
{-# INLINABLE el' #-}

-- | Create a DOM element with attributes, returning the child result.
-- Analog of upstream
-- @elAttr :: DomBuilderE t m => Text -> Map Text Text -> m a -> m a@.
--
-- >>> elAttr "a" ("href" =: "https://reflex-frp.org") (text "Reflex-FRP!")
-- <a href="https://reflex-frp.org">Reflex-FRP!</a>
elAttr
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Map Text Text -> Eff es a -> Eff es a
elAttr elementTag attrs child = P.snd <$> elAttr' elementTag attrs child
{-# INLINABLE elAttr #-}

-- | 'elAttr' variant that returns the built 'Element'. Analog of
-- upstream @elAttr'@.
elAttr'
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text
  -> Map Text Text
  -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpaceE es) (DomBuilderTimelineE es), a)
elAttr' elementTag attrs = element @(DomBuilderTimelineE es) elementTag cfg
  where
    cfg :: ElementConfig EventResult (DomBuilderTimelineE es) (DomBuilderSpaceE es)
    cfg = def & elementConfig_initialAttributes .~ mapKeysToAttributeName attrs
{-# INLINABLE elAttr' #-}

-- | Create a DOM element with a single class attribute. Analog of
-- upstream @elClass :: DomBuilderE t m => Text -> Text -> m a -> m a@.
elClass
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Text -> Eff es a -> Eff es a
elClass elementTag c = elAttr elementTag (Map.singleton "class" c)
{-# INLINABLE elClass #-}

-- | 'elClass' variant that returns the built 'Element'. Analog of
-- upstream @elClass'@.
elClass'
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Text -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpaceE es) (DomBuilderTimelineE es), a)
elClass' elementTag c = elAttr' elementTag (Map.singleton "class" c)
{-# INLINABLE elClass' #-}

placeRawElement
  :: forall es
   . (DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es)
  => RawElement (DomBuilderSpaceE es)
  -> Eff es ()
placeRawElement raw =
  send @(DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es)) (DB_PlaceRawElement raw)
{-# INLINABLE placeRawElement #-}

wrapRawElement
  :: forall t er es
   . (DomBuilderE t (DomBuilderSpaceE es) :> es)
  => RawElement (DomBuilderSpaceE es)
  -> RawElementConfig er t (DomBuilderSpaceE es)
  -> Eff es (Element er (DomBuilderSpaceE es) t)
wrapRawElement raw cfg = send (DB_WrapRawElement raw cfg)
{-# INLINABLE wrapRawElement #-}

-- | Per-space DOM fragment type. Variant B's MTL 'MountableDomBuilder'
-- makes this an associated type of the class (per-monad); our Effect
-- shape parameterises on @space@ instead, so we keep the family
-- top-level.
type family DomFragment (space :: Type) :: Type

-- | @'MountableDomBuilderE' t space@ — build and mount 'DomFragment's
-- into the running DOM. Fragment shape comes from the 'DomFragment'
-- type family so the effect head has no associated type of its own.
data MountableDomBuilderE (t :: Type) (space :: Type) :: Effect where
  BuildDomFragment
    :: m a
    -> MountableDomBuilderE t space m (DomFragment space, a)
  MountDomFragment
    :: DomFragment space
    -> R.Event t (DomFragment space)
    -> MountableDomBuilderE t space m ()

type instance DispatchOf (MountableDomBuilderE t space) = 'Dynamic

-- MTL-class instance for variant B's 'RDBC.MountableDomBuilder' on @Eff es@.
-- depends on the 'DomBuilder' instance.
instance
  ( R.Reflex t
  , RDBC.DomBuilder t (Eff es)
  , MountableDomBuilderE t (DomBuilderSpaceE es) :> es
  , DomBuilderSpaceE es ~ RDBC.DomBuilderSpace (Eff es)
  ) => RDBC.MountableDomBuilder t (Eff es) where
  type DomFragment (Eff es) = DomFragment (DomBuilderSpaceE es)
  buildDomFragment body     = send @(MountableDomBuilderE t (DomBuilderSpaceE es)) (BuildDomFragment body)
  mountDomFragment f0 f'    = send @(MountableDomBuilderE t (DomBuilderSpaceE es)) (MountDomFragment f0 f')
  {-# INLINABLE buildDomFragment #-}
  {-# INLINABLE mountDomFragment #-}

buildDomFragment
  :: forall es a
   . (MountableDomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es)
  => Eff es a
  -> Eff es (DomFragment (DomBuilderSpaceE es), a)
buildDomFragment m =
  send @(MountableDomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es)) (BuildDomFragment m)
{-# INLINABLE buildDomFragment #-}

mountDomFragment
  :: forall es
   . (MountableDomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es)
  => DomFragment (DomBuilderSpaceE es)
  -> R.Event (DomBuilderTimelineE es) (DomFragment (DomBuilderSpaceE es))
  -> Eff es ()
mountDomFragment df ev =
  send @(MountableDomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es)) (MountDomFragment df ev)
{-# INLINABLE mountDomFragment #-}

-- | @'HasDocumentE' space@ — access the ambient 'RawDocument' for @space@.
data HasDocumentE (space :: Type) :: Effect where
  AskDocument :: HasDocumentE space m (RawDocument space)

type instance DispatchOf (HasDocumentE space) = 'Dynamic

type family HasDocumentSpace (es :: [Effect]) :: Type where
  HasDocumentSpace (HasDocumentE space : _)    = space
  HasDocumentSpace (_                 : rest) = HasDocumentSpace rest

askDocument
  :: forall es
   . (HasDocumentE (HasDocumentSpace es) :> es)
  => Eff es (RawDocument (HasDocumentSpace es))
askDocument = send @(HasDocumentE (HasDocumentSpace es)) AskDocument
{-# INLINABLE askDocument #-}

-- MTL-class instance for variant B's 'RDBC.HasDocument' on @Eff es@.
-- needs to agree with the 'DomBuilder' instance's
-- 'DomBuilderSpace' choice.
instance
  ( HasDocumentE (HasDocumentSpace es) :> es
  , HasDocumentSpace es ~ DomBuilderSpaceE es
  , RDBC.DomBuilder t (Eff es)
  , DomBuilderSpaceE es ~ RDBC.DomBuilderSpace (Eff es)
  ) => RDBC.HasDocument (Eff es) where
  askDocument = send @(HasDocumentE (HasDocumentSpace es)) AskDocument
  {-# INLINABLE askDocument #-}

-- =====================================================================
-- Types.hs: JSM' Effect + DomRenderHookE Effect +
-- HydrationEnv + HydrationState + HydrationMode + WidgetE /
-- HydrationRunnerE constraint synonyms + RunnerAction + DelayedHydration.
--
-- Ported from Reflex.Dom.Core with
-- imports pointed at Reflex.Host.HeadlessEffectful (eff-reflex).
-- performJSMEvent + performJSMEvent_ are wired against
-- HH.performEventE / HH.performEventE_ (see the two functions
-- immediately below liftJSM).
-- =====================================================================

-- 'JSM'' / 'askJSM' / 'liftJSM' relocated to 'JSDOM.Extras.JSM' in
-- jsaddle-dom-extras. Imported at top of module.

-- MTL-class instance: 'MonadJSM' on @Eff es@ whenever @JSM' :> es@.
-- Enables variant B's widget code with @MonadJSM m@ constraints (Xhr,
-- WebSocket, Location, etc.) to be called directly on our effect row.
instance (JSM' :> es, IOE :> es) => JSaddleTypes.MonadJSM (Eff es) where
  liftJSM' = liftJSM
  {-# INLINABLE liftJSM' #-}

-- | MTL bridge: 'MonadRef' on @Eff es@ whenever @IOE :> es@.
-- Required by upstream 'PrerenderBaseConstraints' and by any widget
-- helper that carries @MonadRef m@ / @Ref m ~ IORef@ constraints.
--
-- 'PrimMonad (Eff es)' is already provided by effectful itself (via
-- 'Effectful.Internal.Monad.Prim') — no bridge needed there.
instance IOE :> es => MRef.MonadRef (Eff es) where
  type Ref (Eff es) = IORef
  newRef            = liftIO . MRef.newRef
  readRef           = liftIO . MRef.readRef
  writeRef ref      = liftIO . MRef.writeRef ref
  {-# INLINABLE newRef   #-}
  {-# INLINABLE readRef  #-}
  {-# INLINABLE writeRef #-}

-- NOTE: no orphan 'MonadJSM' instance on 'SpiderHostFrame', and none is
-- needed. eff-reflex stays DOM-agnostic; the JS context is threaded
-- through the widget effect row via 'WithJSContextSingleton' (from
-- @Foreign.JavaScript.TH@), and every 'performEvent' callback captures
-- the ambient 'JSContextRef' via 'askJSM' at construction time — see
-- 'performJSMEvent_' / 'performJSMEvent' below. The callback body then
-- runs the captured @JSM ()@ under that context with @runJSM jsm ctx@,
-- which is what 'MonadJSM' would provide anyway.

-- | Fire-and-forget @JSM@-per-event helper. Analog of variant A's
-- @performJSMEvent_@ and morally equivalent to upstream reflex-dom's
-- @performEvent_ (fmap liftJSM evt)@. Grabs the ambient JSContextRef
-- via 'askJSM', then schedules the JSM action to run inside the
-- 'SpiderHostFrame' via 'HH.performEventE_' — matches the ambient
-- context that the surrounding 'runJSM'' installed.
performJSMEvent_
  :: forall t es
   . ( R.Reflex t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , JSM' :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     )
  => R.Event t (JSM ())
  -> Eff es ()
performJSMEvent_ evt = do
  ctx <- askJSM
  HH.performEventME_ @(RH.HostFrame t) @t $
    R.ffor evt $ \jsm -> liftIO (runJSM jsm ctx)

-- | Value-returning variant of 'performJSMEvent_'. Runs the JSM
-- action per fire and delivers each result as an occurrence of the
-- returned 'R.Event'. Same ambient-context semantics.
performJSMEvent
  :: forall t es a
   . ( R.Reflex t
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , JSM' :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     )
  => R.Event t (JSM a)
  -> Eff es (R.Event t a)
performJSMEvent evt = do
  ctx <- askJSM
  HH.performEventME @(RH.HostFrame t) @t $
    R.ffor evt $ \jsm -> liftIO (runJSM jsm ctx)

-- | Effect replacing the @DomRenderHookT@ transformer.
data DomRenderHookE (t :: Type) :: Effect where
  WithRenderHook    :: (forall x. JSM x -> JSM x) -> m a -> DomRenderHookE t m a
  RequestDomAction  :: R.Event t (JSM a) -> DomRenderHookE t m (R.Event t a)
  RequestDomAction_ :: R.Event t (JSM ()) -> DomRenderHookE t m ()

type instance DispatchOf (DomRenderHookE t) = 'Dynamic

-- | Recover the timeline @t@ from an effect row containing a
-- 'DomRenderHookE t' entry — parallel to 'DomBuilderTimelineE'.
type family DomRenderHookTimelineE (es :: [Effect]) :: Type where
  DomRenderHookTimelineE (DomRenderHookE t : _)    = t
  DomRenderHookTimelineE (_               : rest) = DomRenderHookTimelineE rest

-- MTL-class instance for variant B's 'RDBC.DomRenderHook' on @Eff es@.
-- Fundep @m -> t@ resolved via an equality
-- constraint pinning @t@ to 'DomRenderHookTimelineE es'.
instance
  ( R.Reflex t
  , t ~ DomRenderHookTimelineE es
  , DomRenderHookE t :> es
  ) => RDBC.DomRenderHook t (Eff es) where
  withRenderHook hook body = send @(DomRenderHookE t) (WithRenderHook hook body)
  requestDomAction  evt    = send @(DomRenderHookE t) (RequestDomAction  evt)
  requestDomAction_ evt    = send @(DomRenderHookE t) (RequestDomAction_ (P.fmap (P.>> pure ()) evt))
  {-# INLINABLE withRenderHook    #-}
  {-# INLINABLE requestDomAction  #-}
  {-# INLINABLE requestDomAction_ #-}

-- Orphan MonadTrans-shape instances on 'WithJSContextSingleton x m' for
-- the MTL classes reflex-dom-core's own 'Foreign.JavaScript.TH' doesn't
-- ship. Delegate via 'lift' (WithJSContextSingleton derives MonadTrans).
-- These close the WidgetEff t x es → WithJSContextSingleton x (Eff es) →
-- Eff es delegation chain so widget bodies polymorphic in the 'TestWidget'
-- constraint bundle can typecheck at 'WidgetEff t x es'.

instance RPB.PostBuild t m => RPB.PostBuild t (WithJSContextSingleton x m) where
  getPostBuild = lift RPB.getPostBuild
  {-# INLINABLE getPostBuild #-}

instance RTE.TriggerEvent t m => RTE.TriggerEvent t (WithJSContextSingleton x m) where
  newTriggerEvent                          = lift RTE.newTriggerEvent
  newTriggerEventWithOnComplete            = lift RTE.newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f  = lift (RTE.newEventWithLazyTriggerWithOnComplete f)
  {-# INLINABLE newTriggerEvent                       #-}
  {-# INLINABLE newTriggerEventWithOnComplete         #-}
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}

-- 'PrimMonad' bridge on 'WidgetEff' — route through 'liftIO' via IOE.
-- Avoids requiring effectful's 'Prim' effect (which would force every
-- attach-site to layer 'runPrim' on the interpreter stack). 'IOE :> es'
-- is already in HydrationWidgetE / FloatingWidgetE, so this instance
-- doesn't add a new obligation on callers.
instance IOE :> es => PrimMonad (WidgetEff t x es) where
  type PrimState (WidgetEff t x es) = PrimState IO
  primitive f = WidgetEff (WithJSContextSingleton (ReaderT (\_ -> liftIO (primitive f))))
  {-# INLINABLE primitive #-}

-- NotReady already provided at line 1002.

-- | Whether the builder is in the initial hydration pass (matching a
-- statically prerendered DOM) or the post-hydration steady-state where
-- new nodes are appended immediately.
data HydrationMode
  = HydrationMode_Hydrating
  | HydrationMode_Immediate
  deriving (Eq, Ord, Show)

-- | Structured record of a single hydration divergence — what the
-- walker EXPECTED to find vs. what it actually did (or didn't) find,
-- and the source line inside 'DomHost.hs' where the divergence was
-- observed. Populated by every walker case that flips the failed flag.
data HydrationFailure = HydrationFailure
  { _hydrationFailure_site   :: !Text  -- ^ Short label for the walker fn (e.g. "goWalk hydrateElement", "hydrateTextNode goHydrate")
  , _hydrationFailure_expected :: !Text -- ^ What the walker expected (tag name, text prefix, etc.)
  , _hydrationFailure_found  :: !Text  -- ^ What the walker actually saw at that DOM cursor position ("no more siblings", "<div>", "text 'foo'")
  , _hydrationFailure_srcLoc :: !Text  -- ^ File:line marker in DomHost.hs (approximate — hand-recorded)
  }
  deriving Show

-- | State threaded through 'HydrationRunner' actions.
data HydrationState = HydrationState
  { _hydrationState_previousNode :: !(Maybe Node)
  , _hydrationState_failed       :: !Bool
  , _hydrationState_failures     :: ![HydrationFailure]
    -- ^ Ordered list of divergences, in walker-encounter order. Empty
    -- unless '_hydrationState_failed' is True. Emitted verbatim by the
    -- final failure warning at 'runHydrationRunnerWithFailure' so a
    -- developer can see exactly WHERE and WHY the DOM diverged instead
    -- of the useless generic warning it used to print.
  }

-- | Ambient env for the 'WidgetE' scope.
data HydrationEnv t = HydrationEnv
  { hydrationEnv_document         :: {-# UNPACK #-} !Document
  , hydrationEnv_parent           :: !(Either Node (IORef Node))
  , hydrationEnv_unreadyChildren  :: {-# UNPACK #-} !(IORef Word)
  , hydrationEnv_commitAction     :: !(JSM ())
  , hydrationEnv_hydrationMode    :: {-# UNPACK #-} !(IORef HydrationMode)
  , hydrationEnv_switchover       :: !(R.Event t ())
  , hydrationEnv_delayed          :: {-# UNPACK #-} !(IORef (DelayedHydration t))
  , hydrationEnv_events           :: !(Chan [DSum (HH.EventTriggerRef t) HH.TriggerInvocation])
  }

-- | The Reflex timeline for interacting with the DOM. Single-source-of-truth
-- for the widget-side timeline. Mirrors upstream MTL reflex-dom's
-- 'Reflex.Dom.Main.DomTimeline'. Every widget signature in this module that
-- ran on a specific-Spider-Global timeline now refers to 'DomTimeline'
-- instead of writing out @'R.SI.SpiderTimeline' 'R.SI.Global'@ inline —
-- swapping the timeline at some future point is a one-line change here.
type DomTimeline = R.SI.SpiderTimeline R.SI.Global

-- | The ReflexHost the DOM lives in. Mirrors upstream MTL reflex-dom's
-- 'Reflex.Dom.Main.DomHost'.
type DomHost = R.SI.SpiderHost R.SI.Global

-- | Compute the Spider timeline TAG from a timeline. eff-reflex
-- parameterises 'HH.PerformEvent' on the tag @x@ (not the timeline
-- @t@); every other effect from 'HH' takes @t@ directly. This family
-- bridges the two so 'WidgetE' can name both consistently from a single
-- @t@ parameter.
type family SpiderTag (t :: Type) :: Type where
  SpiderTag (R.SI.SpiderTimeline x) = x

-- | The full effect row required by widget-scope code.
type WidgetE (t :: Type) (s :: Type) es =
  ( Reader (HydrationEnv t) :> es
  , DomRenderHookE t :> es
  , HH.PostBuild t :> es
  , HH.PerformEventM (RH.HostFrame t) t :> es
  , HH.TriggerEvent t :> es
  , HH.CreateTrigger t :> es
  , HH.NotReady t :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , HH.Adjustable t :> es
  , JSM' :> es
  , IOE :> es
  )

-- =====================================================================

-- Explicit guest rows are available for effect-based callers. MTL
-- Adjustable instances use the same-monad Adjustable effect, preserving
-- arbitrary widget nesting without requiring a recursive effect row.
-- | The guest widget effect row. What runs inside each cohort of
-- 'runWithReplace'/'traverseXxxWithAdjust' — the "guest" of the
-- AdjustableE handler. Each caller of 'runAdjustableE' provides a
-- matching runner that discharges these effects.
--
-- Design — option (b) merged Requester + PerformEvent:
--   * 'Requester' lives HERE (in sub), NOT at outer. Sub-widget code
--     that calls 'requesting' directly hits sub's Requester.
--   * 'PerformEvent' also HERE. Its interpreter uses ambient (sub's)
--     Requester via 'interpretPerformEventAsRequester' — so 'performEvent'
--     and 'requesting' share ONE Requester per cohort.
--   * 'PostBuild', 'TriggerEvent' — leaf FRP effects.
--   * 'BaseEffectfulHostRow' at tail — Hold/Sample/CreateTrigger/etc.
--   * DomBuilder etc. NOT included here yet (still lives at outer);
--     phase 3 folds them in if we want per-cohort DOM scope.
--
-- Ordering matters: composition
-- 'runRequester . interpretPerformEventAsRequester . runPostBuild . runTriggerEvent'
-- (right-to-left) peels innermost first. Each interpreter needs its
-- effect at HEAD after prior peels. So order (head to tail):
-- TriggerEvent, PostBuild, PerformEvent, Requester, Base.
type GuestEff (t :: Type) (x :: Type) =
  HH.TriggerEvent t
    ': HH.PostBuild t
    ': HH.PerformEventEff '[IOE] (R.SI.SpiderTimeline x)
    ': HH.Requester t (R.SI.SpiderHostFrame x) Identity
    ': HH.BaseEffectfulHostRow x

-- | Hydration environment and JS context over the base host effects.
-- This row alone does not provide DOM building or adjustment operations.
type WidgetRowE (t :: Type) (x :: Type) =
  Reader (HydrationEnv t)
    ': JSM'
    ': HH.BaseEffectfulHostRow x

-- | Constraint synonym form of 'WidgetRowE' — usable in any 'es' that
-- carries the required members. Widget helper signatures use this so
-- they compose over user rows that add extra effects.
type WidgetE_AE (t :: Type) (x :: Type) es =
  (
    Reader (HydrationEnv t) :> es
  , JSM' :> es
  , IOE :> es
  )

-- | The effect row required by runner-scope code (delayed hydration
-- actions). This is the row the queued 'RunnerAction' body runs in —
-- it needs @State HydrationState@ + @Reader Node@ (installed by
-- 'runHydrationRunnerWithFailure' internally) plus all the shared FRP
-- + JSM effects.
type HydrationRunnerE (t :: Type) es =
  ( State HydrationState :> es
  , Reader Node :> es
  , HydrationRunnerOuterE t es
  )

-- | The effect row a CALLER of 'runHydrationRunnerWithFailure' must
-- have. Same as 'HydrationRunnerE' minus @State HydrationState@ and
-- @Reader Node@ — those two are installed fresh inside
-- 'runHydrationRunnerWithFailure' (via @runReader parent (runState
-- (HydrationState prev False) action)@), so the caller's row need not
-- carry them.
--
-- Splitting this out avoids the dummy @evalState . runReader@
-- scaffolding a caller would otherwise need to satisfy an over-declared
-- 'HydrationRunnerE' outer constraint.
type HydrationRunnerOuterE (t :: Type) es =
  ( Reader (HydrationEnv t) :> es
  , DomRenderHookE t :> es
  , HH.PostBuild t :> es
  , HH.PerformEventM (RH.HostFrame t) t :> es
  , HH.TriggerEvent t :> es
  , HH.CreateTrigger t :> es
  , HH.NotReady t :> es
  , HH.Adjustable t :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , JSM' :> es
  , IOE :> es
  )

-- | Rank-N queued runner action.
newtype RunnerAction t a = RunnerAction
  { unRunnerAction :: forall es. HydrationRunnerE t es => Eff es a
  }

type DelayedHydration t = RunnerAction t ()

-- =====================================================================
-- DomRenderHookE interpreter + send-wrappers.
--
-- Semantics (matches upstream's DomRenderHookT + Requester composition):
--   * WithRenderHook composes a JSM-transform onto a hook stack (via
--     Effectful State). Any RequestDomAction fired INSIDE the wrapper
--     uses the composed transform. Nesting: outer applied LAST
--     (nearest the raw JSM) — matches upstream.
--   * RequestDomAction: on each event fire, run the JSM (under the
--     current hook) via runJSM against the ambient JSContextRef, and
--     push the result out as an occurrence of the returned event.
--   * RequestDomAction_: same, fire-and-forget.
--
-- The eff-reflex-native shape (vs. variant A's direct-subscribe using
-- Prim.subscribeEvent + EventME): use `performEventE_` to schedule the
-- JSM effect on event fire, and `newTriggerEventE` to obtain the
-- output-event / push-callback pair. `performEventE_` handles frame
-- ordering + Requester routing via eff-reflex's own machinery — no
-- unsafeCoerce Event-newtype crossings, no direct subscribeEvent
-- calls, no EventME wrapping.
-- =====================================================================

-- | Ambient state for the interpreter: the composed hook function.
-- Starts as 'id'; each WithRenderHook composes its hook on the LEFT
-- so upstream's "outer applied last" semantics hold.
newtype HookState = HookState (forall x. JSM x -> JSM x)

-- | Send-wrapper for 'WithRenderHook'.
withRenderHook
  :: forall t es a. (DomRenderHookE t :> es)
  => (forall x. JSM x -> JSM x)
  -> Eff es a
  -> Eff es a
withRenderHook hook inner = send @(DomRenderHookE t) (WithRenderHook hook inner)

-- | Send-wrapper for 'RequestDomAction'.
requestDomAction
  :: forall t es a. (DomRenderHookE t :> es)
  => R.Event t (JSM a)
  -> Eff es (R.Event t a)
requestDomAction = send @(DomRenderHookE t) . RequestDomAction

-- | Send-wrapper for 'RequestDomAction_'.
requestDomAction_
  :: forall t es. (DomRenderHookE t :> es)
  => R.Event t (JSM ())
  -> Eff es ()
requestDomAction_ = send @(DomRenderHookE t) . RequestDomAction_

-- | Interpret 'DomRenderHookE (SpiderTimeline x)' on top of an effect
-- row that already carries 'TriggerEvent (SpiderTimeline x)',
-- 'PerformEvent x', 'JSM'', and 'IOE'. The interpreter is
-- polymorphic in @x@ so the same body serves the test-scope
-- ('Global') and any future non-Global timeline instances.
--
-- Requires:
--   * TriggerEvent (SpiderTimeline x) — for newTriggerEventE (the
--     output event + push-callback pair for RequestDomAction).
--   * PerformEvent x — to schedule the JSM effect per event fire.
--   * JSM' — to look up the ambient JSContextRef via askJSM.
--   * IOE — runJSM is IO-typed.
--
-- Design note: the 'reinterpret' adds an internal @State HookState@
-- effect that lives strictly inside the interpreter's scope; it is
-- evaluated by 'evalState' before returning, so the surrounding row
-- never sees it.
runDomRenderHook
  :: forall x es a.
     ( R.SI.HasSpiderTimeline x
     , HH.TriggerEvent (R.SI.SpiderTimeline x) :> es
     , HH.PerformEventM (RH.HostFrame (R.SI.SpiderTimeline x)) (R.SI.SpiderTimeline x) :> es
     , JSM' :> es
     , IOE :> es
     )
  => Eff (DomRenderHookE (R.SI.SpiderTimeline x) : es) a
  -> Eff es a
runDomRenderHook act = do
  ctx <- askJSM
  reinterpret
    (evalState (HookState (P.id :: forall y. JSM y -> JSM y)))
    (\env cmd -> case cmd of
      WithRenderHook newHook inner -> do
        HookState old <- get @HookState
        modify @HookState (\_ -> HookState (\jsm -> old (newHook jsm)))
        r <- localSeqUnliftIO env $ \unlift -> liftIO (unlift inner)
        modify @HookState (\_ -> HookState old)
        pure r
      RequestDomAction ev -> do
        HookState hook <- get @HookState
        (evOut, pushOut) <- HH.newTriggerEventE @(R.SI.SpiderTimeline x)
        HH.performEventME_ @(RH.HostFrame (R.SI.SpiderTimeline x)) @(R.SI.SpiderTimeline x) $ RC.fmapCheap
          (\jsm -> liftIO $ do
             result <- runJSM (hook jsm) ctx
             pushOut result)
          ev
        pure evOut
      RequestDomAction_ ev -> do
        HookState hook <- get @HookState
        HH.performEventME_ @(RH.HostFrame (R.SI.SpiderTimeline x)) @(R.SI.SpiderTimeline x) $ RC.fmapCheap
          (\jsm -> liftIO $ void $ runJSM (hook jsm) ctx)
          ev)
    act

-- =====================================================================
-- runJSM' interpreter for the JSM' effect.
--
-- Semantics: LiftJSM runs the JSM body under the captured ctx via
-- runJSM; WithJSContext hands the ctx to a raw-IO callback;
-- AskJSContext returns the ctx directly.
--
-- The ctx is captured once at interpreter entry and closed over for
-- the interpreter's lifetime — matches the "grab once, reuse across
-- every subsequent send" pattern in runDomRenderHook.
-- =====================================================================

runJSM'
  :: (IOE :> es)
  => JSContextRef
  -> Eff (JSM' : es) a
  -> Eff es a
runJSM' ctx = interpret $ \_ cmd -> case cmd of
  LiftJSM jsm     -> liftIO (runJSM jsm ctx)
  WithJSContext k -> liftIO (k ctx)
  AskJSContext    -> pure ctx

-- =====================================================================
-- Wrap.hs port: element event-listener wiring.
--
-- Contains:
--   * EventFilterTriggerRef newtype — per-EventName slot filled at
--     first subscribe.
--   * AttachDomListenersE constraint synonym — narrow row for
--     attachDomListeners callable from both widget and runner scopes.
--   * triggerBody / triggerBodyDeferred — per-key setup callbacks
--     passed to newFanEventWithTriggerE.
--   * allocateFilterTriggers — allocate refs + build fan-event, DOM
--     element not yet required (hydration walker phase).
--   * attachDomListeners — install listeners on a live element, fill
--     the deferred-element MVar.
--   * wrap — fused immediate-mode: modify-attrs subscription,
--     filter listeners, fan-event, assembled Element in one shot.
--   * wrapDomEvent / wrapDomEventMaybe / wrapDomEventsMaybe /
--     subscribeDomEventJSM — convenience wrappers around
--     newTriggerEventWithOnCompleteE / newFanEventWithTriggerE for
--     external DOM event sources (window/document/foreign).
--
-- Verbatim from Reflex.Dom.Core with
-- imports pointed at eff-reflex-native primitives:
--   * newFanEventWithTrigger  → HH.newFanEventWithTriggerE
--   * newTriggerEventWithOnComplete → HH.newTriggerEventWithOnCompleteE
--   * EventTrigger / EventTriggerRef — HH-qualified
--   * ffor → RC.ffor
--
-- Timeline constraint pinned to `t ~ R.SI.SpiderTimeline (SpiderTag t)`
-- (was `t ~ EffTimeline Global` in variant A) so the port is
-- polymorphic in the Spider tag @x@ while still discharging the
-- CreateTrigger + fan-event requirements.
-- =====================================================================

-- | Per-EventName trigger slot allocated by 'allocateFilterTriggers'
-- (or inline in 'wrap') and consumed by 'triggerBody'.
--
-- The IORef holds 'Nothing' until the fan-event's setup callback
-- fires (in response to the first downstream subscriber for that
-- EventName), at which point it is populated with a live
-- 'EventTriggerRef'. The DOM event handler installed by 'wrap' /
-- 'attachDomListeners' checks the ref on each fire and, if set,
-- pushes a 'TriggerInvocation' into the ambient firing chan.
newtype EventFilterTriggerRef t er (en :: EventTag) = EventFilterTriggerRef
  (IORef (Maybe (HH.EventTriggerRef t (er en))))

-- | Narrow effect row required by 'attachDomListeners'. Contrast with
-- 'WidgetE': no 'Reader (HydrationEnv t)' (events chan is passed
-- explicitly), no 'PostBuild', no 'NotReady', no 'DomRenderHookE',
-- no 'Hold'/'Sample'. Just what's actually used inside the body.
type AttachDomListenersE t es =
  ( IOE :> es
  , JSM' :> es
  , RH.ReflexHost t
  , HH.PerformEventM (RH.HostFrame t) t :> es
  , HH.TriggerEvent t :> es
  )

-- | Internal helper: lift a raw 'JSM' into the effect row via 'JSM''.
liftJSM_ :: JSM' :> es => JSM a -> Eff es a
liftJSM_ = send . LiftJSM
{-# INLINE liftJSM_ #-}

-- | Attach the modify-attributes subscription + per-event DOM
-- listeners to @rawEl@, allocate a fan-event over the resulting
-- 'EventFilterTriggerRef's, and return the assembled 'Element'.
--
-- Fused-mode: DOM element is already live so we allocate filter
-- trigger refs and build the fan directly against 'triggerBody' (no
-- forkIO + MVar deferred install). Matches upstream reflex-dom's
-- immediate-caller shape (variant A comment: "wrap events e cfg
-- body" + newFanEventWithTrigger fused).
{-# INLINABLE wrap #-}
wrap
  :: forall er t s es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     )
  => (DOM.Element -> RawElement s)
     -- ^ Discriminator from concrete DOM.Element to space's raw
     -- element type. For GhcjsDomSpace this is id; for
     -- HydrationDomSpace (RawElement = ()) this is @const ()@.
  -> DOM.Element
  -> RawElementConfig er t s
  -> Eff es (Element er s t)
wrap toRaw e cfg = do
  forM_ (_rawElementConfig_modifyAttributes cfg) $ \modifyAttrs ->
    requestDomAction_ @t $ RC.ffor modifyAttrs $ imapM_ $
      \(AttributeName mAttrNs n) mv -> case mAttrNs of
        Nothing -> maybe (Element.removeAttribute e n) (Element.setAttribute e n) mv
        Just ns -> maybe (Element.removeAttributeNS e (Just ns) n) (Element.setAttributeNS e (Just ns) n) mv
  events <- asks @(HydrationEnv t) hydrationEnv_events
  ctx    <- askJSM
  filterTriggers <- liftIO $ fmap DMap.fromList $ forM
    (DMap.toList $ _ghcjsEventSpec_filters $ _rawElementConfig_eventSpec cfg) $
      \(en :=> GhcjsEventFilter _) -> do
        triggerRef <- newIORef Nothing
        pure $ en :=> EventFilterTriggerRef triggerRef
  _teardown <- liftJSM_ $ forM
    (DMap.toList $ _ghcjsEventSpec_filters $ _rawElementConfig_eventSpec cfg) $
      \(en :=> GhcjsEventFilter f) -> do
        let triggerRef = case DMap.lookup en filterTriggers of
              Just (EventFilterTriggerRef r) -> r
              Nothing -> error "wrap: filter DMap missing key just allocated — impossible unless DMap.fromList/toList disagree on the spec's filter keys, which would be a library bug."
        elementOnEventName en e $ do
          evt <- event
          (flags, k) <- JSaddle.liftJSM $ f $ GhcjsDomEvent evt
          when (_eventFlags_preventDefault flags) $ withIsEvent en EventM.preventDefault
          case _eventFlags_propagation flags of
            Propagation_Continue      -> pure ()
            Propagation_Stop          -> withIsEvent en EventM.stopPropagation
            Propagation_StopImmediate -> withIsEvent en EventM.stopImmediatePropagation
          mv <- JSaddle.liftJSM k
          liftIO $ forM_ mv $ \v -> do
            mTrig <- readIORef triggerRef
            forM_ mTrig $ \trig ->
              writeChan events [trig :=> HH.TriggerInvocation v (pure ())]
  esel <- HH.newFanEventWithTriggerE $ \wa t' ->
    triggerBody ctx cfg events filterTriggers e wa t'
  pure Element
    { _element_events = esel
    , _element_raw    = toRaw e
    }

-- | Allocate per-EventName trigger refs AND build the fan-event whose
-- setup callback threads 'triggerBodyDeferred'. Does NOT touch the
-- DOM: the @DOM.Element@ is not required until 'attachDomListeners'
-- runs.
--
-- Returned 'MVar' MUST be filled (exactly once) by
-- 'attachDomListeners' with the resolved 'DOM.Element'. The MVar is
-- returned rather than kept opaque so hydration split call sites
-- can share it explicitly.
allocateFilterTriggers
  :: forall er t s es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     )
  => RawElementConfig er t s
  -> Eff es
       ( RC.EventSelector t (WrapArg er EventName)
       , DMap EventName (EventFilterTriggerRef t er)
       , MVar DOM.Element
       )
allocateFilterTriggers cfg = do
  events <- asks @(HydrationEnv t) hydrationEnv_events
  ctx    <- askJSM
  filterTriggers <- liftIO $ fmap DMap.fromList $ forM
    (DMap.toList $ _ghcjsEventSpec_filters $ _rawElementConfig_eventSpec cfg) $
      \(en :=> GhcjsEventFilter _) -> do
        triggerRef <- newIORef Nothing
        pure $ en :=> EventFilterTriggerRef triggerRef
  elMVar <- liftIO newEmptyMVar
  esel <- HH.newFanEventWithTriggerE $ \wa t' ->
    triggerBodyDeferred ctx cfg events filterTriggers elMVar wa t'
  pure (esel, filterTriggers, elMVar)

-- | Attach DOM listeners to a raw element for the given trigger refs
-- and fill the deferred-element MVar. Returns a teardown IO.
attachDomListeners
  :: forall er t s es
   . ( AttachDomListenersE t es
     , EventSpec s ~ GhcjsEventSpec
     )
  => Chan [DSum (HH.EventTriggerRef t) HH.TriggerInvocation]
  -> DOM.Element
  -> RawElementConfig er t s
  -> DMap EventName (EventFilterTriggerRef t er)
  -> MVar DOM.Element
  -> Eff es (IO ())
attachDomListeners events e cfg filterTriggers elMVar = do
  liftIO $ putMVar elMVar e
  teardowns <- liftJSM_ $ forM
    (DMap.toList $ _ghcjsEventSpec_filters $ _rawElementConfig_eventSpec cfg) $
      \(en :=> GhcjsEventFilter f) -> do
        let triggerRef = case DMap.lookup en filterTriggers of
              Just (EventFilterTriggerRef r) -> r
              Nothing -> error "attachDomListeners: filter DMap missing key allocated by allocateFilterTriggers — the two functions were called with different RawElementConfigs, which is a caller bug."
        elementOnEventName en e $ do
          evt <- event
          (flags, k) <- JSaddle.liftJSM $ f $ GhcjsDomEvent evt
          when (_eventFlags_preventDefault flags) $ withIsEvent en EventM.preventDefault
          case _eventFlags_propagation flags of
            Propagation_Continue      -> pure ()
            Propagation_Stop          -> withIsEvent en EventM.stopPropagation
            Propagation_StopImmediate -> withIsEvent en EventM.stopImmediatePropagation
          mv <- JSaddle.liftJSM k
          liftIO $ forM_ mv $ \v -> do
            mTrig <- readIORef triggerRef
            forM_ mTrig $ \trig ->
              writeChan events [trig :=> HH.TriggerInvocation v (pure ())]
  ctx <- askJSM
  pure $ do
    forM_ teardowns $ \td -> runJSM td ctx

-- | Per-key setup callback passed to 'HH.newFanEventWithTriggerE'.
--
--   * If an 'EventFilterTriggerRef' exists for this key: populate the
--     ref with the fresh trigger. The DOM listener installed by
--     'wrap' / 'attachDomListeners' consults the ref on each fire.
--   * Otherwise: install a fresh 'elementOnEventName' listener that
--     applies the spec's fallback handler and, if it yields a value,
--     pushes a 'TriggerInvocation' into the chan.
--
-- The teardown IO returned from each branch is what
-- 'newFanEventWithTriggerE' invokes when the last subscriber for
-- this key drops.
triggerBody
  :: forall er t s x
   . (EventSpec s ~ GhcjsEventSpec)
  => JSContextRef
  -> RawElementConfig er t s
  -> Chan [DSum (HH.EventTriggerRef t) HH.TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef t er)
  -> DOM.Element
  -> WrapArg er EventName x
  -> RH.EventTrigger t x
  -> IO (IO ())
triggerBody ctx cfg events eventTriggerRefs e (WrapArg en) trig =
  case DMap.lookup en eventTriggerRefs of
    Just (EventFilterTriggerRef r) -> do
      triggerRef <- HH.EventTriggerRef <$> newIORef (Just trig)
      writeIORef r $ Just triggerRef
      pure $ writeIORef r Nothing
    Nothing -> (`runJSM` ctx) <$> (`runJSM` ctx) (elementOnEventName en e $ do
      evt <- event
      mv <- JSaddle.liftJSM $ unGhcjsEventHandler handler (en, GhcjsDomEvent evt)
      case mv of
        Nothing -> pure ()
        Just v  -> liftIO $ do
          triggerRef <- HH.EventTriggerRef <$> newIORef (Just trig)
          writeChan events [triggerRef :=> HH.TriggerInvocation v (pure ())])
  where
    handler :: GhcjsEventHandler er
    !handler = _ghcjsEventSpec_handler $ _rawElementConfig_eventSpec cfg

-- | Deferred-element variant of 'triggerBody' — same filtered path;
-- unfiltered path forks a thread that blocks on 'MVar' until
-- 'attachDomListeners' fills it.
triggerBodyDeferred
  :: forall er t s x
   . (EventSpec s ~ GhcjsEventSpec)
  => JSContextRef
  -> RawElementConfig er t s
  -> Chan [DSum (HH.EventTriggerRef t) HH.TriggerInvocation]
  -> DMap EventName (EventFilterTriggerRef t er)
  -> MVar DOM.Element
  -> WrapArg er EventName x
  -> RH.EventTrigger t x
  -> IO (IO ())
triggerBodyDeferred ctx cfg events eventTriggerRefs elMVar wa@(WrapArg en) trig =
  case DMap.lookup en eventTriggerRefs of
    Just (EventFilterTriggerRef r) -> do
      triggerRef <- HH.EventTriggerRef <$> newIORef (Just trig)
      writeIORef r $ Just triggerRef
      pure $ writeIORef r Nothing
    Nothing -> do
      stateRef <- newIORef @(Maybe (Either () (IO ()))) Nothing
      _ <- forkIO $ do
        e <- readMVar elMVar
        realTeardown <- triggerBody ctx cfg events eventTriggerRefs e wa trig
        cancelled <- atomicModifyIORef' stateRef $ \case
          Just (Left ()) -> (Just (Right realTeardown), True)
          _              -> (Just (Right realTeardown), False)
        when cancelled realTeardown
      pure $ do
        prior <- atomicModifyIORef' stateRef $ \case
          Nothing          -> (Just (Left ()), Nothing)
          Just (Left ())   -> (Just (Left ()), Nothing)
          Just (Right td)  -> (Just (Right td), Just td)
        case prior of
          Just td -> td
          Nothing -> pure ()

-- | Ambient-JSM 'TriggerEvent'-backed convenience wrapper.
{-# INLINABLE wrapDomEvent #-}
wrapDomEvent
  :: forall t es elt evt a
   . (HH.TriggerEvent t :> es, JSM' :> es, IOE :> es)
  => elt
  -> (elt -> EventM elt evt () -> JSM (JSM ()))
  -> EventM elt evt a
  -> Eff es (R.Event t a)
wrapDomEvent elt elementOnevent getValue =
  wrapDomEventMaybe @t elt elementOnevent (fmap Just getValue)

-- | Maybe-yielding variant: skip firing on 'Nothing'.
{-# INLINABLE wrapDomEventMaybe #-}
wrapDomEventMaybe
  :: forall t es elt evt a
   . (HH.TriggerEvent t :> es, JSM' :> es, IOE :> es)
  => elt
  -> (elt -> EventM elt evt () -> JSM (JSM ()))
  -> EventM elt evt (Maybe a)
  -> Eff es (R.Event t a)
wrapDomEventMaybe elt elementOnevent getValue = do
  ctx <- askJSM
  (ev, fire) <- HH.newTriggerEventWithOnCompleteE @t
  _ <- liftJSM_ $ (`runJSM` ctx) $ elementOnevent elt $ do
    mv <- getValue
    forM_ mv $ \v -> liftIO $ fire v (pure ())
  pure ev

-- | Install one listener per EventName listed in the @onEventName@
-- family, dispatching through a single fan-event.
{-# INLINABLE wrapDomEventsMaybe #-}
wrapDomEventsMaybe
  :: forall t es el f
   . ( WidgetE t GhcjsDomSpace es
     , RH.ReflexHost t
     )
  => el
  -> (forall en. DOM.IsEvent (EventType en) => EventName en -> EventM el (EventType en) (Maybe (f en)))
  -> (forall en. EventName en -> el -> EventM el (EventType en) () -> JSM (JSM ()))
  -> Eff es (RC.EventSelector t (WrapArg f EventName))
wrapDomEventsMaybe target handlers onEventName_ = do
  ctx <- askJSM
  events <- asks @(HydrationEnv t) hydrationEnv_events
  HH.newFanEventWithTriggerE $ \(WrapArg en) trig ->
    withIsEvent en $
      ((`runJSM` ctx) <$>) . (`runJSM` ctx) $
        subscribeDomEventJSM (onEventName_ en target) (handlers en) events trig

-- =====================================================================
-- Window / AppInput / AppOutput — recursive-app bootstrap surface.
-- Ports upstream 'Reflex.Dom.Builder.Immediate' + 'Reflex.Dom.Main'
-- Window/AppInput/AppOutput/wrapWindow/runApp' (Immediate.hs:2743-2759,
-- Main.hs:352-370). Widget observes window events via 'Window'; app
-- output currently has no config-visible fields (upstream WindowConfig
-- is 'data WindowConfig t = WindowConfig -- No config options yet' —
-- when it grows fields, the outer 'rec' knot upstream uses will need to
-- be restored here via MonadFix.
-- =====================================================================

data WindowConfig t = WindowConfig  -- No config options yet

instance Default (WindowConfig t) where
  def = WindowConfig

data Window t = Window
  { _window_events :: RC.EventSelector t (WrapArg EventResult EventName)
  , _window_raw    :: DOM.Window
  }

-- | Attach window-level listeners; return a 'Window' fan-selector.
-- Mirrors upstream 'wrapWindow' (Immediate.hs:2753-2759).
wrapWindow
  :: forall t es
   . ( WidgetE t GhcjsDomSpace es
     , RH.ReflexHost t
     )
  => DOM.Window
  -> WindowConfig t
  -> Eff es (Window t)
wrapWindow wv _ = do
  events <- wrapDomEventsMaybe wv (defaultDomWindowEventHandler wv) windowOnEventName
  pure Window { _window_events = events, _window_raw = wv }

newtype AppInput t = AppInput
  { _appInput_window :: Window t
  }

newtype AppOutput t = AppOutput  -- TODO: Add quit event
  { _appOutput_windowConfig :: WindowConfig t
  }

-- | JSaddle helper for 'wrapDomEventsMaybe' — install a listener that
-- pushes a TriggerInvocation when the handler yields a value.
subscribeDomEventJSM
  :: forall t el evt a
   . (EventM el evt () -> JSM (JSM ()))
  -> EventM el evt (Maybe a)
  -> Chan [DSum (HH.EventTriggerRef t) HH.TriggerInvocation]
  -> RH.EventTrigger t a
  -> JSM (JSM ())
subscribeDomEventJSM elementOnevent getValue eventChan trig = elementOnevent $ do
  mv <- getValue
  forM_ mv $ \v -> liftIO $ do
    triggerRef <- HH.EventTriggerRef <$> newIORef (Just trig)
    writeChan eventChan [triggerRef :=> HH.TriggerInvocation v (pure ())]

-- =====================================================================
-- DOM spaces (verbatim from
-- Reflex.Dom.Core).
--
-- 'GhcjsDomSpace' + 'HydrationDomSpace' phantom types + their
-- 'DomSpace' instances; 'GhcjsEventSpec' + filter + handler; the
-- 'Pair1' / 'Maybe1' utility types.
-- =====================================================================

-- Space types (GhcjsDomSpace, HydrationDomSpace) and the associated
-- event-handler machinery are now imported from variant B's
-- Reflex.Dom.Builder.Immediate. Only 'Pair1' and 'Maybe1' remain
-- inlined here — they're port-only utility types not in variant B.

-- | @'Pair1' f g@ is a heterogeneous pair indexed by a common kind.
data Pair1 (f :: k -> Type) (g :: k -> Type) (a :: k) = Pair1 (f a) (g a)

-- | Kind-indexed 'Maybe'.
data Maybe1 f a = Nothing1 | Just1 (f a)

-- =====================================================================
-- Hydration.hs: mode + counters + cursor helpers +
-- delayed-step queue + runner driver.
--
-- Ported verbatim from Reflex.Dom.Core
-- 'driveChildRunner' re-enters the runner with the child hydration cursor.
-- =====================================================================

{-# INLINABLE getHydrationMode #-}
getHydrationMode
  :: forall t es
   . (Reader (HydrationEnv t) :> es, IOE :> es)
  => Eff es HydrationMode
getHydrationMode = do
  ref <- asks @(HydrationEnv t) hydrationEnv_hydrationMode
  liftIO $ readIORef ref

{-# INLINABLE askUnreadyChildren #-}
askUnreadyChildren
  :: forall t es
   . Reader (HydrationEnv t) :> es
  => Eff es (IORef Word)
askUnreadyChildren = asks @(HydrationEnv t) hydrationEnv_unreadyChildren

{-# INLINABLE askCommitAction #-}
askCommitAction
  :: forall t es
   . Reader (HydrationEnv t) :> es
  => Eff es (JSM ())
askCommitAction = asks @(HydrationEnv t) hydrationEnv_commitAction

{-# INLINABLE askParent #-}
askParent
  :: Reader Node :> es
  => Eff es Node
askParent = ask

{-# INLINABLE getParent #-}
getParent
  :: forall t es
   . (Reader (HydrationEnv t) :> es, IOE :> es)
  => Eff es Node
getParent = do
  p <- asks @(HydrationEnv t) hydrationEnv_parent
  either pure (liftIO . readIORef) p

{-# INLINABLE getPreviousNode #-}
getPreviousNode
  :: State HydrationState :> es
  => Eff es (Maybe Node)
getPreviousNode = gets _hydrationState_previousNode

{-# INLINABLE setPreviousNode #-}
setPreviousNode
  :: State HydrationState :> es
  => Maybe Node -> Eff es ()
setPreviousNode n =
  modify (\hs -> hs { _hydrationState_previousNode = n })

{-# INLINABLE insertAfterPreviousNode #-}
insertAfterPreviousNode
  :: forall es node
   . ( Reader Node :> es
     , State HydrationState :> es
     , JSM' :> es
     , IsNode node
     )
  => node -> Eff es ()
insertAfterPreviousNode node = do
  parent <- askParent
  prev <- getPreviousNode
  nextNode <- liftJSM_ $ maybe (Node.getFirstChild parent) Node.getNextSibling prev
  liftJSM_ $ Node.insertBefore_ parent node nextNode
  setPreviousNode $ Just $ toNode node

{-# INLINABLE localEnv #-}
localEnv
  :: forall t es a
   . Reader (HydrationEnv t) :> es
  => (HydrationEnv t -> HydrationEnv t)
  -> Eff es a
  -> Eff es a
localEnv f = local @(HydrationEnv t) (f $!)

{-# INLINABLE append #-}
append
  :: forall t es
   . (Reader (HydrationEnv t) :> es, JSM' :> es, IOE :> es)
  => Node -> Eff es ()
append n = do
  p <- getParent @t
  liftJSM_ $ Node.appendChild_ p n

{-# INLINABLE addHydrationStep #-}
addHydrationStep
  :: forall t es
   . (Reader (HydrationEnv t) :> es, IOE :> es)
  => (forall es'. HydrationRunnerE t es' => Eff es' ())
  -> Eff es ()
addHydrationStep step = do
  delayedRef <- asks @(HydrationEnv t) hydrationEnv_delayed
  liftIO $ modifyIORef' delayedRef $ \(RunnerAction old) ->
    RunnerAction (old >> step)

{-# INLINABLE addHydrationStepWithSetup #-}
addHydrationStepWithSetup
  :: forall t es a
   . (Reader (HydrationEnv t) :> es, IOE :> es)
  => Eff es a
  -> (a -> (forall es'. HydrationRunnerE t es' => Eff es' ()))
  -> Eff es ()
addHydrationStepWithSetup setup f = getHydrationMode @t >>= \case
  HydrationMode_Immediate -> pure ()
  HydrationMode_Hydrating -> do
    s <- setup
    addHydrationStep @t (f s)

{-# INLINABLE removeSubsequentNodes #-}
removeSubsequentNodes
  :: forall n es
   . (IsNode n, JSM' :> es)
  => n -> Eff es ()
removeSubsequentNodes n = liftJSM_ $ go (toNode n)
  where
    go :: Node -> JSM ()
    go x = do
      nxt <- x ^. JSaddle.js ("nextSibling" :: Text)
      truthy <- ghcjsPure (isTruthy nxt)
      when truthy $ do
        _ <- x ^. JSaddle.js ("parentNode" :: Text) . JSaddle.js1 ("removeChild" :: Text) nxt
        go x

{-# INLINABLE runHydrationRunner #-}
runHydrationRunner
  :: forall t es.
     HydrationRunnerOuterE t es
  => DelayedHydration t
  -> Maybe Node                -- ^ Initial previous-node hint (runner cursor seed).
  -> Node                      -- ^ Parent node the runner walks under.
  -> Eff es ()
runHydrationRunner action prev parent =
  runHydrationRunnerWithFailure action (pure ()) prev parent

{-# INLINABLE runHydrationRunnerWithFailure #-}
runHydrationRunnerWithFailure
  :: forall t es.
     HydrationRunnerOuterE t es
  => DelayedHydration t
  -> IO ()                     -- ^ Fired if hydration diverges from the prerendered DOM.
  -> Maybe Node                -- ^ Initial previous-node hint (runner cursor seed).
  -> Node                      -- ^ Parent node the runner walks under.
  -> Eff es ()
runHydrationRunnerWithFailure (RunnerAction action) onFailure prev parent = do
  (_, hs) <-
    runReader parent
      (runState (HydrationState prev False []) action)
  traverse_ removeSubsequentNodes (_hydrationState_previousNode hs)
  when (_hydrationState_failed hs) $ do
    liftIO $ do
      P.putStrLn "=========================================================================="
      P.putStrLn "reflex-dom-effectful: HYDRATION DIVERGENCE at switchover time"
      P.putStrLn "=========================================================================="
      P.putStrLn $ "Number of divergences observed: " <> P.show (P.length (_hydrationState_failures hs))
      P.putStrLn ""
      P.mapM_ (\(i, HydrationFailure site expected found srcLoc) -> do
                 P.putStrLn $ "  #" <> P.show i <> " at " <> T.unpack site <> "  (" <> T.unpack srcLoc <> ")"
                 P.putStrLn $ "     expected: " <> T.unpack expected
                 P.putStrLn $ "     found:    " <> T.unpack found
                 P.putStrLn "")
              (P.zip [1 :: Int ..] (P.reverse (_hydrationState_failures hs)))
      P.putStrLn "General causes: invalid HTML altered by the browser at parse time,"
      P.putStrLn "external JS mutating the DOM before hydration runs, an outdated"
      P.putStrLn "server-side render, or a bug in the widget-tree divergence between"
      P.putStrLn "static-render and hydration modes."
      P.putStrLn "=========================================================================="
    liftIO onFailure

-- =====================================================================
-- Element.hs foundational block:
--   * Adjust-helper vocabulary: ChildReadyState / TraverseChildImmediate
--     / TraverseChildHydration / TraverseChild + drawChildUpdate.
--   * Patch re-exports: mapPatchDMap, mapPatchDMapWithMove.
--   * DOM manipulation: insertBefore / insertBefore_ /
--     deleteBetweenExclusive / deleteUpTo / collectUpTo /
--     extractBetweenExclusive / extractUpTo / collectUpToGivenParent.
--   * Pure element factory: makeElement.
--   * Immediate-mode node factories: textNodeImmediate,
--     commentNodeImmediate.
--
-- Verbatim from Element.hs (lines 316-613). Rank-N RunnerAction usage
-- keeps the child-hydration story polymorphic in the runner row.
-- =====================================================================

data ChildReadyState a
  = ChildReadyState_Ready
  | ChildReadyState_Unready !(Maybe a)
  deriving (Show, Read, Eq, Ord)

data TraverseChildImmediate k = TraverseChildImmediate
  { _traverseChildImmediate_fragment        :: {-# UNPACK #-} !DocumentFragment
  , _traverseChildImmediate_placeholder     :: {-# UNPACK #-} !DOM.Text
  , _traverseChildImmediate_childReadyState :: {-# UNPACK #-} !(IORef (ChildReadyState k))
  }

newtype TraverseChildHydration t = TraverseChildHydration
  { _traverseChildHydration_delayed :: RunnerAction t DOM.Text
  }

data TraverseChild t k a = TraverseChild
  { _traverseChild_mode   :: !(Either (TraverseChildHydration t) (TraverseChildImmediate k))
  , _traverseChild_result :: !a
  } deriving Functor

{-# INLINABLE drawChildUpdate #-}
drawChildUpdate
  :: forall t s k f a es
   . WidgetE t s es
  => HydrationEnv t
  -> (IORef (ChildReadyState k) -> JSM ())
  -> Eff es (f a)
  -> Eff es (Compose (TraverseChild t k) f a)
drawChildUpdate initialEnv markReady child = do
  let doc :: Document
      doc = hydrationEnv_document initialEnv
  unreadyChildren <- liftIO $ newIORef 0
  mode <- liftIO $ readIORef $ hydrationEnv_hydrationMode initialEnv
  case mode of
    HydrationMode_Hydrating -> do
      childDelayedRef <- liftIO $ newIORef (RunnerAction (pure ()) :: DelayedHydration t)
      let hydratingEnv = initialEnv
            { hydrationEnv_unreadyChildren = unreadyChildren
            , hydrationEnv_delayed         = childDelayedRef
            }
      result <- local @(HydrationEnv t) (const hydratingEnv) child
      childDelayed <- liftIO $ readIORef childDelayedRef
      pure $ Compose $ TraverseChild
        { _traverseChild_result = result
        , _traverseChild_mode = Left $ TraverseChildHydration
            { _traverseChildHydration_delayed = RunnerAction $ do
                placeholder <- send $ LiftJSM $ Document.createTextNode doc ("" :: Text)
                insertAfterPreviousNode placeholder
                unRunnerAction childDelayed
                pure placeholder
            }
        }
    HydrationMode_Immediate -> do
      childReadyState <- liftIO $ newIORef $ ChildReadyState_Unready Nothing
      df <- send $ LiftJSM $ Document.createDocumentFragment doc
      placeholder <- send $ LiftJSM $ Document.createTextNode doc ("" :: Text)
      send $ LiftJSM $ Node.appendChild_ df placeholder
      let immediateEnv = initialEnv
            { hydrationEnv_parent          = Left (toNode df)
            , hydrationEnv_unreadyChildren = unreadyChildren
            , hydrationEnv_commitAction    = markReady childReadyState
            }
      result <- local @(HydrationEnv t) (const immediateEnv) child
      u <- liftIO $ readIORef unreadyChildren
      when (u == 0) $
        liftIO $ writeIORef childReadyState ChildReadyState_Ready
      pure $ Compose $ TraverseChild
        { _traverseChild_result = result
        , _traverseChild_mode = Right TraverseChildImmediate
            { _traverseChildImmediate_fragment        = df
            , _traverseChildImmediate_placeholder     = placeholder
            , _traverseChildImmediate_childReadyState = childReadyState
            }
        }

-- ---------------------------------------------------------------------
-- DOM manipulation helpers
-- ---------------------------------------------------------------------

insertBefore
  :: (IsNode new, IsNode existing, JSM' :> es)
  => new -> existing -> Eff es ()
insertBefore new existing = liftJSM_ $ do
  p <- Node.getParentNodeUnchecked existing
  Node.insertBefore_ p new (Just existing)

insertBefore_
  :: (IsNode new, IsNode existing, JSM' :> es)
  => new -> existing -> Eff es ()
insertBefore_ = insertBefore

deleteBetweenExclusive
  :: (JSM' :> es, IsNode start, IsNode end)
  => start -> end -> Eff es ()
deleteBetweenExclusive s e = liftJSM_ $ do
  df <- Document.createDocumentFragment =<< Node.getOwnerDocumentUnchecked s
  extractBetweenExclusiveJSM df (toNode s) (toNode e)

extractBetweenExclusive
  :: (JSM' :> es, IsNode start, IsNode end)
  => DocumentFragment -> start -> end -> Eff es ()
extractBetweenExclusive df s e =
  liftJSM_ $ extractBetweenExclusiveJSM df (toNode s) (toNode e)

extractBetweenExclusiveJSM :: DocumentFragment -> Node -> Node -> JSM ()
extractBetweenExclusiveJSM df s e = go
  where
    go = do
      x <- s ^. JSaddle.js ("nextSibling" :: Text)
      JSaddle.strictEqual e x >>= \case
        True  -> pure ()
        False -> do
          _ <- df ^. JSaddle.js1 ("appendChild" :: Text) x
          go

deleteUpTo
  :: (JSM' :> es, IsNode start, IsNode end)
  => start -> end -> Eff es ()
deleteUpTo s e = liftJSM_ $ do
  df <- Document.createDocumentFragment =<< Node.getOwnerDocumentUnchecked s
  extractUpToJSM df (toNode s) (toNode e)

extractUpTo
  :: (JSM' :> es, IsNode start, IsNode end)
  => DocumentFragment -> start -> end -> Eff es ()
extractUpTo df s e = liftJSM_ $ extractUpToJSM df (toNode s) (toNode e)

extractUpToJSM :: DocumentFragment -> Node -> Node -> JSM ()
extractUpToJSM df s0 e = go (JSaddle.toJSVal s0) (JSaddle.toJSVal e)
  where
    go x eVal = JSaddle.strictEqual x eVal >>= \case
      True  -> pure ()
      False -> do
        y <- x ^. JSaddle.js ("nextSibling" :: Text)
        _ <- df ^. JSaddle.js1 ("appendChild" :: Text) x
        go (pure y) eVal

collectUpTo
  :: (JSM' :> es, IsNode start, IsNode end)
  => start -> end -> Eff es DocumentFragment
collectUpTo s e = liftJSM_ $ do
  currentParent <- Node.getParentNodeUnchecked e
  collectUpToGivenParentJSM currentParent (toNode s) (toNode e)

collectUpToGivenParent
  :: (JSM' :> es, IsNode parent, IsNode start, IsNode end)
  => parent -> start -> end -> Eff es DocumentFragment
collectUpToGivenParent currentParent s e =
  liftJSM_ $ collectUpToGivenParentJSM (toNode currentParent) (toNode s) (toNode e)

collectUpToGivenParentJSM :: Node -> Node -> Node -> JSM DocumentFragment
collectUpToGivenParentJSM currentParent s e = do
  doc <- Node.getOwnerDocumentUnchecked currentParent
  df <- Document.createDocumentFragment doc
  extractUpToJSM df s e
  pure df

-- ---------------------------------------------------------------------
-- Pure element factory
-- ---------------------------------------------------------------------

{-# INLINE makeElement #-}
makeElement
  :: forall er t s es. JSM' :> es
  => Document -> Text -> ElementConfig er t s -> Eff es DOM.Element
makeElement doc elementTag cfg = liftJSM_ $ do
  e <- uncheckedCastTo DOM.Element <$> case cfg ^. elementConfig_namespace of
    Nothing  -> Document.createElement doc elementTag
    Just ens -> Document.createElementNS doc (Just ens) elementTag
  let attrs = cfg ^. elementConfig_initialAttributes
  Map.foldrWithKey
    (\(AttributeName mAttrNs n) v acc -> do
        case mAttrNs of
          Nothing  -> Element.setAttribute e n v
          Just ans -> Element.setAttributeNS e (Just ans) n v
        acc)
    (pure ())
    attrs
  pure e

-- ---------------------------------------------------------------------
-- Node factories (immediate mode)
-- ---------------------------------------------------------------------

{-# INLINABLE textNodeImmediate #-}
textNodeImmediate
  :: forall t s es
   . (WidgetE t s es, R.Reflex t)
  => TextNodeConfig t -> Eff es DOM.Text
textNodeImmediate (TextNodeConfig t mSetContents) = do
  p <- getParent @t
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  n <- liftJSM_ $ Document.createTextNode doc t
  liftJSM_ $ Node.appendChild_ p n
  for_ mSetContents $ \ev ->
    requestDomAction_ @t (fmap (Node.setNodeValue n . Just) ev)
  pure n

{-# INLINABLE commentNodeImmediate #-}
commentNodeImmediate
  :: forall t s es
   . (WidgetE t s es, R.Reflex t)
  => CommentNodeConfig t -> Eff es DOM.Comment
commentNodeImmediate (CommentNodeConfig t mSetContents) = do
  p <- getParent @t
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  n <- liftJSM_ $ Document.createComment doc t
  liftJSM_ $ Node.appendChild_ p n
  for_ mSetContents $ \ev ->
    requestDomAction_ @t (fmap (Node.setNodeValue n . Just) ev)
  pure n

-- =====================================================================
-- Element.hs: hydration text/comment walkers + dispatch-
-- mode text/comment factories + hydration-skip / hydratable attribute
-- markers.
--
-- Uses HH.holdE / HH.sampleE (eff-reflex) in place of variant A's
-- unqualified hold / sample.
-- =====================================================================

{-# INLINABLE hydrateTextNode #-}
hydrateTextNode
  :: forall t es
   . HydrationRunnerE t es
  => Document -> Text -> Eff es DOM.Text
hydrateTextNode doc "" = do
  tn <- liftJSM_ $ Document.createTextNode doc ("" :: Text)
  insertAfterPreviousNode tn
  pure tn
hydrateTextNode doc t = do
  parent <- askParent
  mPrev  <- getPreviousNode
  n <- go parent mPrev
  setPreviousNode $ Just $ toNode n
  pure n
  where
    go :: Node -> Maybe Node -> Eff es DOM.Text
    go parent mLastNode = do
      mNext <- liftJSM_ $ maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode
      case mNext of
        Nothing -> do
          modify @HydrationState $ \s -> s
            { _hydrationState_failed   = True
            , _hydrationState_failures = HydrationFailure
                { _hydrationFailure_site     = "hydrateStaticTextNode.go"
                , _hydrationFailure_expected = "text node: " <> T.pack (P.show t)
                , _hydrationFailure_found    = "no next sibling under parent (DOM ran out of nodes)"
                , _hydrationFailure_srcLoc   = "DomHost.hs:~2450"
                } : _hydrationState_failures s
            }
          n <- liftJSM_ $ Document.createTextNode doc t
          insertAfterPreviousNode n
          pure n
        Just node -> do
          mText <- liftJSM_ $ DOM.castTo DOM.Text node
          case mText of
            Nothing -> go parent (Just node)
            Just originalNode -> do
              originalText <- liftJSM_ $ Node.getTextContentUnchecked originalNode
              case T.stripPrefix t originalText of
                Just "" -> pure originalNode
                Just _  -> do
                  liftJSM_ $ Text.splitText_ originalNode $ fromIntegral $ T.length t
                  pure originalNode
                Nothing -> do
                  modify @HydrationState $ \s -> s
                    { _hydrationState_failed   = True
                    , _hydrationState_failures = HydrationFailure
                        { _hydrationFailure_site     = "hydrateStaticTextNode.go (text mismatch)"
                        , _hydrationFailure_expected = "text starting with: " <> T.pack (P.show t)
                        , _hydrationFailure_found    = "text: " <> T.pack (P.show originalText)
                        , _hydrationFailure_srcLoc   = "DomHost.hs:~2470"
                        } : _hydrationState_failures s
                    }
                  n <- liftJSM_ $ Document.createTextNode doc t
                  insertAfterPreviousNode n
                  pure n

{-# INLINABLE hydrateComment #-}
hydrateComment
  :: forall t es
   . (HydrationRunnerE t es, R.Reflex t)
  => Document
  -> Text
  -> Maybe (R.Event t Text)
  -> Eff es DOM.Comment
hydrateComment doc t mSetContents = do
  parent <- askParent
  let go :: Maybe Node -> Eff es DOM.Comment
      go mLastNode = do
        mNext <- liftJSM_ $ maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode
        case mNext of
          Nothing -> do
            c <- liftJSM_ $ Document.createComment doc t
            insertAfterPreviousNode c
            pure c
          Just node -> do
            mC <- liftJSM_ $ DOM.castTo DOM.Comment node
            case mC of
              Nothing -> go (Just node)
              Just c  -> do
                t' <- liftJSM_ $ Node.getTextContentUnchecked c
                if t == t'
                  then pure c
                  else do
                    c' <- liftJSM_ $ Document.createComment doc t
                    insertAfterPreviousNode c'
                    pure c'
  n <- go =<< getPreviousNode
  setPreviousNode $ Just $ toNode n
  for_ mSetContents $ \ev ->
    requestDomAction_ @t (fmap (Node.setNodeValue n . Just) ev)
  pure n

{-# INLINABLE skipToAndReplaceComment #-}
skipToAndReplaceComment
  :: forall t s es
   . (WidgetE t s es, R.Reflex t)
  => Text
  -> IORef (Maybe Text)
  -> Eff es (DelayedHydration t, IORef DOM.Text, IORef (Maybe Text))
skipToAndReplaceComment prefix key0Ref = do
  mode <- liftIO . readIORef =<< asks @(HydrationEnv t) hydrationEnv_hydrationMode
  case mode of
    HydrationMode_Immediate -> do
      tn <- textNodeImmediate @t @s (TextNodeConfig ("" :: Text) Nothing)
      append @t (toNode tn)
      textNodeRef <- liftIO $ newIORef tn
      keyRef      <- liftIO $ newIORef Nothing
      pure (RunnerAction (pure ()), textNodeRef, keyRef)
    HydrationMode_Hydrating -> do
      doc <- asks @(HydrationEnv t) hydrationEnv_document
      textNodeRef <- liftIO $ newIORef $ error "skipToAndReplaceComment: textNodeRef read before switchover"
      keyRef      <- liftIO $ newIORef $ error "skipToAndReplaceComment: keyRef read before switchover"
      let switchComment :: DelayedHydration t
          switchComment = RunnerAction $ do
            key0 <- liftIO $ readIORef key0Ref
            mPrev <- getPreviousNode
            (tn, key) <- goHydrate @t doc prefix key0 mPrev
            setPreviousNode $ Just $ toNode tn
            liftIO $ do
              writeIORef textNodeRef tn
              writeIORef keyRef key
      pure (switchComment, textNodeRef, keyRef)

-- | Runner-scope hydration walker for 'skipToAndReplaceComment'.
goHydrate
  :: forall t es
   . HydrationRunnerE t es
  => Document
  -> Text
  -> Maybe Text
  -> Maybe Node
  -> Eff es (DOM.Text, Maybe Text)
goHydrate doc _prefix Nothing _mLastNode = do
  tn <- liftJSM_ $ Document.createTextNode doc ("" :: Text)
  insertAfterPreviousNode tn
  modify @HydrationState $ \s -> s
    { _hydrationState_failed   = True
    , _hydrationState_failures = HydrationFailure
        { _hydrationFailure_site     = "hydrateTextNode.goHydrate (Nothing prefix)"
        , _hydrationFailure_expected = "text node with initial value key"
        , _hydrationFailure_found    = "empty prefix — key0 was Nothing"
        , _hydrationFailure_srcLoc   = "DomHost.hs:~2555"
        } : _hydrationState_failures s
    }
  pure (tn, Nothing)
goHydrate doc prefix (Just key0) mLastNode = do
  parent <- askParent
  mNext <- liftJSM_ $ maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode
  case mNext of
    Nothing -> goHydrate @t doc prefix Nothing Nothing
    Just node -> do
      mComment <- liftJSM_ $ DOM.castTo DOM.Comment node
      case mComment of
        Just comment -> do
          commentText <- liftJSM_ $ fromMaybe (error "goHydrate: comment has no text") <$> Node.getTextContent comment
          case T.stripPrefix (prefix <> key0) commentText of
            Just key -> do
              tn <- liftJSM_ $ Document.createTextNode doc ("" :: Text)
              liftJSM_ $ Node.replaceChild_ parent tn comment
              pure (tn, Just key)
            Nothing ->
              goHydrate @t doc prefix (Just key0) (Just node)
        Nothing ->
          goHydrate @t doc prefix (Just key0) (Just node)

{-# INLINABLE skipToReplaceStart #-}
skipToReplaceStart
  :: forall t s es
   . (WidgetE t s es, R.Reflex t)
  => Eff es (DelayedHydration t, IORef DOM.Text, IORef (Maybe Text))
skipToReplaceStart = do
  ref <- liftIO $ newIORef (Just "")
  skipToAndReplaceComment @t @s "replace-start" ref

{-# INLINABLE skipToReplaceEnd #-}
skipToReplaceEnd
  :: forall t s es
   . (WidgetE t s es, R.Reflex t)
  => IORef (Maybe Text) -> Eff es (DelayedHydration t, IORef DOM.Text)
skipToReplaceEnd key = do
  (m, e, _) <- skipToAndReplaceComment @t @s "replace-end" key
  pure (m, e)

{-# INLINABLE textNodeInternal #-}
textNodeInternal
  :: forall t es
   . (WidgetE t HydrationDomSpace es, R.Reflex t)
  => TextNodeConfig t
  -> Eff es (TextNode HydrationDomSpace t)
textNodeInternal tc@(TextNodeConfig t mSetContents) = do
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  getHydrationMode @t >>= \case
    HydrationMode_Immediate -> void (textNodeImmediate @t @HydrationDomSpace tc)
    HydrationMode_Hydrating ->
      addHydrationStepWithSetup @t
        (maybe (pure (pure t)) (HH.holdE @t t) mSetContents)
        (\currentText -> do
            currentT <- HH.sampleE @t currentText
            n <- hydrateTextNode @t doc currentT
            for_ mSetContents $ \ev ->
              requestDomAction_ @t (fmap (Node.setNodeValue n . Just) ev))
  pure $ TextNode ()

{-# INLINABLE commentNodeInternal #-}
commentNodeInternal
  :: forall t es
   . (WidgetE t HydrationDomSpace es, R.Reflex t)
  => CommentNodeConfig t
  -> Eff es (CommentNode HydrationDomSpace t)
commentNodeInternal tc@(CommentNodeConfig t0 mSetContents) = do
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  getHydrationMode @t >>= \case
    -- NOTE (upstream divergence, intentional bug-fix — see variant A
    -- comment): upstream recurses into commentNodeInternal, producing
    -- unbounded recursion + never subscribing mSetContents. Patched
    -- here by delegating to commentNodeImmediate.
    HydrationMode_Immediate -> void (commentNodeImmediate @t @HydrationDomSpace tc)
    HydrationMode_Hydrating ->
      addHydrationStepWithSetup @t
        (maybe (pure (pure t0)) (HH.holdE @t t0) mSetContents)
        (\bt -> do
            t <- HH.sampleE @t bt
            void $ hydrateComment @t doc t mSetContents)
  pure $ CommentNode ()

skipHydrationAttribute :: IsString s => s
skipHydrationAttribute = "data-hydration-skip"

hydratableAttribute :: IsString s => s
hydratableAttribute = "data-ssr"

-- =====================================================================
-- Element.hs element cluster:
--   * elementImmediate — immediate-mode element factory (uses wrap).
--   * elementInternal  — dispatch-mode factory over HydrationDomSpace.
--   * hydrateElement   — hydration-walker factory (uses
--     allocateFilterTriggers + attachDomListeners).
--   * goWalk           — recursive walker used by hydrateElement.
--   * shouldSkipHydration — JSM helper (skip flag OR missing ssr marker).
--   * driveChildRunner — runner-scope re-entry with fresh parent + cursor.
--
-- The timeline equality matches the trigger and DOM listener operations.
-- =====================================================================

{-# INLINABLE elementImmediate #-}
elementImmediate
  :: forall er t s a es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     )
  => Text
  -> ElementConfig er t s
  -> Eff es a
  -> Eff es (Element er GhcjsDomSpace t, a)
elementImmediate elementTag cfg child = do
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  parent <- getParent @t
  e <- makeElement doc elementTag cfg
  liftJSM_ $ Node.appendChild_ parent e
  result <- localEnv @t
    (\env -> env { hydrationEnv_parent = Left (toNode e) })
    child
  let rawCfg = extractRawElementConfig cfg
      rawCfgGhcjs :: RawElementConfig er t GhcjsDomSpace
      rawCfgGhcjs = RawElementConfig
        { _rawElementConfig_modifyAttributes = _rawElementConfig_modifyAttributes rawCfg
        , _rawElementConfig_eventSpec        = _rawElementConfig_eventSpec rawCfg
        }
  elt <- wrap @er @t @GhcjsDomSpace P.id e rawCfgGhcjs
  pure (elt, result)

{-# INLINABLE elementInternal #-}
elementInternal
  :: forall er t a es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     )
  => Text
  -> ElementConfig er t HydrationDomSpace
  -> Eff es a
  -> Eff es (Element er HydrationDomSpace t, a)
elementInternal elementTag cfg child = getHydrationMode @t >>= \case
  HydrationMode_Immediate -> do
    (Element es _, result) <- elementImmediate @er @t @HydrationDomSpace elementTag cfg child
    pure (Element es (), result)
  HydrationMode_Hydrating -> do
    ((elt, result), _e') <- hydrateElement @er @t elementTag cfg child
    pure (elt, result)

{-# INLINABLE hydrateElement #-}
hydrateElement
  :: forall er t a es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     )
  => Text
  -> ElementConfig er t HydrationDomSpace
  -> Eff es a
  -> Eff es ((Element er HydrationDomSpace t, a), IORef DOM.Element)
hydrateElement elementTag cfg child = do
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  events <- asks @(HydrationEnv t) hydrationEnv_events
  parentRef <- liftIO $ newIORef $ error "hydrateElement: parent ref read before runner populated it (this indicates a bug in the hydration driver: a child widget tried to consume the parent before the walker reached its scope)"
  eRef <- liftIO $ newIORef $ error "hydrateElement: element ref read before runner populated it"
  childDelayedRef <- liftIO $ newIORef (RunnerAction (pure ()) :: DelayedHydration t)
  result <- localEnv @t
    (\env -> env
      { hydrationEnv_parent  = Right parentRef
      , hydrationEnv_delayed = childDelayedRef
      })
    child
  childDom <- liftIO $ readIORef childDelayedRef
  (esel, filterTriggers, elMVar)
    <- allocateFilterTriggers @er @t @HydrationDomSpace (extractRawElementConfig cfg)
  -- Wire modifyAttributes at BUILD time (mirrors upstream's 'wrap events e rawCfg'
  -- at reflex-dom-core/Immediate.hs:881). The reactive subscription is
  -- established now; the JSM action reads 'eRef' at drain-time (populated by
  -- the walker inside addHydrationStep below). Without this, hydration-mode
  -- 'element' with 'modifyAttributes' silently drops all attribute patches —
  -- the immediate-mode path (elementImmediate → wrap) handles it, but the
  -- hydration path had no equivalent, breaking 'can add/update/remove
  -- attributes' and any downstream widget using modifyAttributes in hydration.
  let rawCfg = extractRawElementConfig cfg
  forM_ (_rawElementConfig_modifyAttributes rawCfg) $ \modifyAttrs ->
    requestDomAction_ @t $ RC.ffor modifyAttrs $ \attrs -> do
      e <- liftIO $ readIORef eRef
      imapM_
        (\(AttributeName mAttrNs n) mv -> case mAttrNs of
           Nothing -> maybe (Element.removeAttribute e n) (Element.setAttribute e n) mv
           Just ns -> maybe (Element.removeAttributeNS e (Just ns) n) (Element.setAttributeNS e (Just ns) n) mv)
        attrs
  addHydrationStep @t $ do
    parent <- askParent
    lastHydrationNode <- getPreviousNode
    e <- goWalk @t doc elementTag cfg parent lastHydrationNode
    setPreviousNode $ Just $ toNode e
    liftIO $ do
      writeIORef parentRef $ toNode e
      writeIORef eRef e
    _teardown <- attachDomListeners @er @t @HydrationDomSpace events e
                   (extractRawElementConfig cfg) filterTriggers elMVar
    driveChildRunner @t childDom (toNode e)
  pure ((Element esel (), result), eRef)

-- | Runner-scope walker for 'hydrateElement'. Recursion descends
-- through siblings until it finds a matching element or runs out.
goWalk
  :: forall t er es
   . HydrationRunnerE t es
  => Document
  -> Text
  -> ElementConfig er t HydrationDomSpace
  -> Node
  -> Maybe Node
  -> Eff es DOM.Element
goWalk doc elementTag cfg parent = go
  where
    go :: Maybe Node -> Eff es DOM.Element
    go mLastNode = do
      mNext <- liftJSM_ $ maybe (Node.getFirstChild parent) Node.getNextSibling mLastNode
      case mNext of
        Nothing -> do
          modify @HydrationState $ \s -> s
            { _hydrationState_failed   = True
            , _hydrationState_failures = HydrationFailure
                { _hydrationFailure_site     = "hydrateElement.goWalk"
                , _hydrationFailure_expected = "<" <> elementTag <> "> element"
                , _hydrationFailure_found    = "no next sibling under parent (DOM ran out of nodes)"
                , _hydrationFailure_srcLoc   = "DomHost.hs:~2770"
                } : _hydrationState_failures s
            }
          e <- makeElement doc elementTag cfg
          insertAfterPreviousNode e
          pure e
        Just node -> do
          mEl <- liftJSM_ $ DOM.castTo DOM.Element node
          case mEl of
            Nothing -> go (Just node)
            Just candidate -> do
              skip <- liftJSM_ $ shouldSkipHydration candidate
              if skip
                then go (Just node)
                else do
                  t <- liftJSM_ $ Element.getTagName candidate
                  if T.toCaseFold elementTag == T.toCaseFold t
                    then pure candidate
                    else do
                      modify @HydrationState $ \s -> s
                        { _hydrationState_failed   = True
                        , _hydrationState_failures = HydrationFailure
                            { _hydrationFailure_site     = "hydrateElement.goWalk (tag mismatch)"
                            , _hydrationFailure_expected = "<" <> elementTag <> ">"
                            , _hydrationFailure_found    = "<" <> t <> ">"
                            , _hydrationFailure_srcLoc   = "DomHost.hs:~2810"
                            } : _hydrationState_failures s
                        }
                      n <- makeElement doc elementTag cfg
                      insertAfterPreviousNode n
                      pure n

-- | JSM helper: @True@ if the element carries the skip-hydration
-- attribute OR lacks the hydratable marker.
shouldSkipHydration :: DOM.Element -> JSM Bool
shouldSkipHydration e = do
  skip       <- Element.hasAttribute e (skipHydrationAttribute :: DOM.JSString)
  hydratable <- Element.hasAttribute e (hydratableAttribute    :: DOM.JSString)
  pure $ skip || P.not hydratable

-- | Drive a child's queued 'HydrationRunner' block with a fresh parent
-- + cleared previous-node cursor. Port of upstream's
-- @localRunner childDom Nothing newParent@.
driveChildRunner
  :: forall t es
   . HydrationRunnerE t es
  => DelayedHydration t
  -> Node
  -> Eff es ()
driveChildRunner (RunnerAction childDom) newParent = do
  outerState <- get @HydrationState
  let seed = outerState { _hydrationState_previousNode = Nothing }
  (_, childState) <-
    runReader newParent
      (runState seed childDom)
  traverse_ removeSubsequentNodes (_hydrationState_previousNode childState)
  modify @HydrationState $ \hs ->
    hs { _hydrationState_failed   = _hydrationState_failed hs || _hydrationState_failed childState
       , _hydrationState_failures = _hydrationState_failures childState `mappend` _hydrationState_failures hs
       }

-- =====================================================================
-- Focus helper: mkHasFocus + focusChangeEv.
-- Used by form-element factories (input/textarea/select) to compute
-- the element's initial focus state + subsequent focus changes.
-- =====================================================================

{-# INLINABLE mkHasFocus #-}
mkHasFocus
  :: forall er d t es
   . ( WidgetE t GhcjsDomSpace es
     , IsNode (RawElement d)
     , RH.ReflexHost t
     )
  => Element er d t
  -> Eff es (R.Dynamic t Bool)
mkHasFocus e = do
  doc <- asks @(HydrationEnv t) hydrationEnv_document
  initialFocus <- liftJSM_ $
    Node.isSameNode (toNode $ _element_raw e)
      . fmap toNode =<< DocShadow.getActiveElement doc
  HH.holdDynE @t initialFocus (focusChangeEv e)

-- | The focus-change event for an Element: True on Focus, False on Blur.
focusChangeEv
  :: forall er d t
   . R.Reflex t
  => Element er d t
  -> R.Event t Bool
focusChangeEv e = R.leftmost
  [ False <$ R.select (_element_events e) (WrapArg Blur)
  , True  <$ R.select (_element_events e) (WrapArg Focus)
  ]

-- =====================================================================
-- Input element (immediate + internal + hydrate).
-- =====================================================================

{-# INLINABLE inputElementImmediate #-}
inputElementImmediate
  :: forall er t s es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     , MonadFix (Eff es)
     )
  => InputElementConfig er t s
  -> Eff es (InputElement er GhcjsDomSpace t)
inputElementImmediate cfg = do
  (e, ()) <- elementImmediate @er @t @s "input"
    (_inputElementConfig_elementConfig cfg) (pure ())
  let dom = uncheckedCastTo DOM.HTMLInputElement (_element_raw e)
  liftJSM_ $ Input.setValue dom (_inputElementConfig_initialValue cfg)
  v0 <- liftJSM_ $ Input.getValue dom
  vByUI <- requestDomAction @t $
    Input.getValue dom
      <$ R.select (_element_events e) (WrapArg Input)
  vBySet <- inputWireSetValue @t dom (_inputElementConfig_setValue cfg)
  v <- HH.holdDynE @t v0 (R.leftmost [vBySet, vByUI])
  liftJSM_ $ Input.setChecked dom (_inputElementConfig_initialChecked cfg)
  cByUI <- wrapDomEvent @t dom (`on` Events.click) (Input.getChecked dom)
  cBySet <- inputWireSetChecked @t dom (_inputElementConfig_setChecked cfg)
  c <- HH.holdDynE @t (_inputElementConfig_initialChecked cfg)
         (R.leftmost [R.fmapMaybe P.id cBySet, cByUI])
  hasFocus <- mkHasFocus @er @GhcjsDomSpace @t e
  files <- inputFilesDyn @t dom
  checked <- RD.holdUniqDyn c
  pure InputElement
    { _inputElement_value         = v
    , _inputElement_checked       = checked
    , _inputElement_checkedChange = cByUI
    , _inputElement_input         = vByUI
    , _inputElement_hasFocus      = hasFocus
    , _inputElement_element       = e
    , _inputElement_raw           = dom
    , _inputElement_files         = files
    }

inputWireSetValue
  :: forall t es
   . ( DomRenderHookE t :> es, R.Reflex t, JSM' :> es )
  => DOM.HTMLInputElement
  -> Maybe (R.Event t Text)
  -> Eff es (R.Event t Text)
inputWireSetValue _   Nothing        = pure R.never
inputWireSetValue dom (Just eSetVal) =
  requestDomAction @t $ RC.ffor eSetVal $ \v' -> do
    Input.setValue dom v'
    Input.getValue dom

inputWireSetChecked
  :: forall t es
   . ( DomRenderHookE t :> es, R.Reflex t, JSM' :> es )
  => DOM.HTMLInputElement
  -> Maybe (R.Event t Bool)
  -> Eff es (R.Event t (Maybe Bool))
inputWireSetChecked _   Nothing         = pure R.never
inputWireSetChecked dom (Just eNewChk)  =
  requestDomAction @t $ RC.ffor eNewChk $ \newChecked -> do
    oldChecked <- Input.getChecked dom
    Input.setChecked dom newChecked
    pure $ if newChecked /= oldChecked then Just newChecked else Nothing

inputFilesDyn
  :: forall t es
   . ( WidgetE t GhcjsDomSpace es
     )
  => DOM.HTMLInputElement
  -> Eff es (R.Dynamic t [DOM.File])
inputFilesDyn dom = do
  fileChangeEv <- wrapDomEvent @t dom (`on` Events.change) $ do
    mfiles <- Input.getFiles dom
    let getMyFiles xs = fmap catMaybes
          . P.mapM (FileList.item xs)
          . P.flip P.take [0..]
          . fromIntegral
          =<< FileList.getLength xs
    maybe (pure []) getMyFiles mfiles
  HH.holdDynE @t P.mempty fileChangeEv

{-# INLINABLE inputElementInternal #-}
inputElementInternal
  :: forall er t es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => InputElementConfig er t HydrationDomSpace
  -> Eff es (InputElement er HydrationDomSpace t)
inputElementInternal cfg = getHydrationMode @t >>= \case
  HydrationMode_Immediate -> do
    r <- inputElementImmediate @er @t @HydrationDomSpace cfg
    pure r
      { _inputElement_element = Element (_element_events $ _inputElement_element r) ()
      , _inputElement_raw     = ()
      }
  HydrationMode_Hydrating -> inputElementHydrate cfg

inputElementHydrate
  :: forall er t es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => InputElementConfig er t HydrationDomSpace
  -> Eff es (InputElement er HydrationDomSpace t)
inputElementHydrate cfg = do
  ((e, _), domRef) <- hydrateElement @er @t "input"
    (_inputElementConfig_elementConfig cfg) (pure ())
  (vUI, trigVUI)   <- HH.newTriggerEventE @t
  (vSet, trigVSet) <- HH.newTriggerEventE @t
  (focus, trigFocus) <- HH.newTriggerEventE @t
  (cUI, trigCUI)   <- HH.newTriggerEventE @t
  (cSet, trigCSet) <- HH.newTriggerEventE @t
  (files, trigFiles) <- HH.newTriggerEventE @t
  let v0 = _inputElementConfig_initialValue cfg
      c0 = _inputElementConfig_initialChecked cfg
  addHydrationStepWithSetup @t (inputSwitchoverBehaviors cfg) $ \(vBeh, cBeh) ->
    inputHydrateRunner @er @t e domRef cfg vBeh cBeh
      InputHydrateTriggers { ihtValUI = trigVUI, ihtValSet = trigVSet
                           , ihtFocus = trigFocus, ihtChkUI = trigCUI
                           , ihtChkSet = trigCSet, ihtFiles = trigFiles }
  checked' <- HH.holdDynE @t c0 (R.leftmost [cSet, cUI])
  checked  <- RD.holdUniqDyn checked'
  hasFocusDyn <- RD.holdUniqDyn P.=<< HH.holdDynE @t False focus
  v <- HH.holdDynE @t v0 (R.leftmost [vSet, vUI])
  filesDyn <- HH.holdDynE @t P.mempty files
  pure InputElement
    { _inputElement_value         = v
    , _inputElement_checked       = checked
    , _inputElement_checkedChange = cUI
    , _inputElement_input         = vUI
    , _inputElement_hasFocus      = hasFocusDyn
    , _inputElement_element       = e
    , _inputElement_raw           = ()
    , _inputElement_files         = filesDyn
    }

inputSwitchoverBehaviors
  :: forall er t es
   . ( WidgetE t HydrationDomSpace es
     , R.Reflex t
     )
  => InputElementConfig er t HydrationDomSpace
  -> Eff es (R.Behavior t Text, R.Behavior t Bool)
inputSwitchoverBehaviors cfg = do
  let v0 = _inputElementConfig_initialValue cfg
      c0 = _inputElementConfig_initialChecked cfg
  vBeh <- maybe (pure (pure v0)) (HH.holdE @t v0) (_inputElementConfig_setValue cfg)
  cBeh <- maybe (pure (pure c0)) (HH.holdE @t c0) (_inputElementConfig_setChecked cfg)
  pure (vBeh, cBeh)

data InputHydrateTriggers = InputHydrateTriggers
  { ihtValUI  :: Text -> IO ()
  , ihtValSet :: Text -> IO ()
  , ihtFocus  :: Bool -> IO ()
  , ihtChkUI  :: Bool -> IO ()
  , ihtChkSet :: Bool -> IO ()
  , ihtFiles  :: [DOM.File] -> IO ()
  }

inputHydrateRunner
  :: forall er t es
   . ( HydrationRunnerE t es, R.Reflex t )
  => Element er HydrationDomSpace t
  -> IORef DOM.Element
  -> InputElementConfig er t HydrationDomSpace
  -> R.Behavior t Text
  -> R.Behavior t Bool
  -> InputHydrateTriggers
  -> Eff es ()
inputHydrateRunner e domRef cfg vBeh cBeh trigs = do
  switchoverValue   <- HH.sampleE @t vBeh
  switchoverChecked <- HH.sampleE @t cBeh
  domElement <- liftIO $ readIORef domRef
  let dom     = uncheckedCastTo DOM.HTMLInputElement domElement
      getVal  = Input.getValue dom
      v0      = _inputElementConfig_initialValue cfg
      c0      = _inputElementConfig_initialChecked cfg
  when (v0 /= switchoverValue) $
    liftIO $ ihtValSet trigs switchoverValue
  realValue <- liftJSM_ getVal
  when (realValue /= switchoverValue) $
    liftIO $ ihtValUI trigs realValue
  requestDomAction_ @t $
    (getVal P.>>= liftIO . ihtValUI trigs)
      <$ R.select (_element_events e) (WrapArg Input)
  for_ (_inputElementConfig_setValue cfg) $ \eSet ->
    requestDomAction_ @t $ RC.ffor eSet $ \v' -> do
      Input.setValue dom v'
      v <- getVal
      liftIO $ ihtValSet trigs v
  installFocusListener @t e domElement (ihtFocus trigs)
  when (c0 /= switchoverChecked) $
    liftIO $ ihtChkSet trigs switchoverChecked
  realChecked <- liftJSM_ $ Input.getChecked dom
  when (realChecked /= switchoverChecked) $
    liftIO $ ihtChkUI trigs realChecked
  _ <- liftJSM_ $ dom `on` Events.click $
    liftIO . ihtChkUI trigs P.=<< Input.getChecked dom
  for_ (_inputElementConfig_setChecked cfg) $ \eNewChk ->
    requestDomAction_ @t $ RC.ffor eNewChk $ \newChk -> do
      oldChk <- Input.getChecked dom
      Input.setChecked dom newChk
      when (newChk /= oldChk) $ liftIO $ ihtChkSet trigs newChk
  _ <- liftJSM_ $ dom `on` Events.change $ do
    mfiles <- Input.getFiles dom
    let readAll xs = fmap catMaybes
          . P.mapM (FileList.item xs)
          . P.flip P.take [0..]
          . fromIntegral
          =<< FileList.getLength xs
    liftIO . ihtFiles trigs P.=<< maybe (pure []) readAll mfiles
  pure ()

installFocusListener
  :: forall t er es
   . ( DomRenderHookE t :> es
     , JSM' :> es
     , IOE :> es
     , R.Reflex t
     )
  => Element er HydrationDomSpace t
  -> DOM.Element
  -> (Bool -> IO ())
  -> Eff es ()
installFocusListener e domElement trigFocus = do
  doc <- liftJSM_ $ Node.getOwnerDocumentUnchecked domElement
  isActive <- liftJSM_ $ do
    ma <- DocShadow.getActiveElement doc
    Node.isSameNode (toNode domElement) (toNode <$> ma)
  liftIO $ trigFocus isActive
  let focusChange = R.leftmost
        [ False <$ R.select (_element_events e) (WrapArg Blur)
        , True  <$ R.select (_element_events e) (WrapArg Focus)
        ]
  requestDomAction_ @t $ liftIO . trigFocus <$> focusChange

-- =====================================================================
-- TextArea element (immediate + internal + hydrate).
-- =====================================================================

{-# INLINABLE textAreaElementImmediate #-}
textAreaElementImmediate
  :: forall er t s es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     )
  => TextAreaElementConfig er t s
  -> Eff es (TextAreaElement er GhcjsDomSpace t)
textAreaElementImmediate cfg = do
  (e, ()) <- elementImmediate @er @t @s "textarea"
    (_textAreaElementConfig_elementConfig cfg) (pure ())
  let dom = uncheckedCastTo DOM.HTMLTextAreaElement (_element_raw e)
  liftJSM_ $ TextArea.setValue dom (_textAreaElementConfig_initialValue cfg)
  v0 <- liftJSM_ $ TextArea.getValue dom
  vByUI <- requestDomAction @t $
    TextArea.getValue dom
      <$ R.select (_element_events e) (WrapArg Input)
  vBySet <- textAreaWireSetValue @t dom (_textAreaElementConfig_setValue cfg)
  v <- HH.holdDynE @t v0 (R.leftmost [vBySet, vByUI])
  hasFocus <- mkHasFocus @er @GhcjsDomSpace @t e
  pure TextAreaElement
    { _textAreaElement_value    = v
    , _textAreaElement_input    = vByUI
    , _textAreaElement_hasFocus = hasFocus
    , _textAreaElement_element  = e
    , _textAreaElement_raw      = dom
    }

textAreaWireSetValue
  :: forall t es
   . ( DomRenderHookE t :> es, R.Reflex t, JSM' :> es )
  => DOM.HTMLTextAreaElement
  -> Maybe (R.Event t Text)
  -> Eff es (R.Event t Text)
textAreaWireSetValue _   Nothing        = pure R.never
textAreaWireSetValue dom (Just eSetVal) =
  requestDomAction @t $ RC.ffor eSetVal $ \v' -> do
    TextArea.setValue dom v'
    TextArea.getValue dom

{-# INLINABLE textAreaElementInternal #-}
textAreaElementInternal
  :: forall er t es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => TextAreaElementConfig er t HydrationDomSpace
  -> Eff es (TextAreaElement er HydrationDomSpace t)
textAreaElementInternal cfg = getHydrationMode @t >>= \case
  HydrationMode_Immediate -> do
    r <- textAreaElementImmediate @er @t @HydrationDomSpace cfg
    pure r
      { _textAreaElement_element = Element (_element_events $ _textAreaElement_element r) ()
      , _textAreaElement_raw     = ()
      }
  HydrationMode_Hydrating -> textAreaElementHydrate cfg

textAreaElementHydrate
  :: forall er t es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => TextAreaElementConfig er t HydrationDomSpace
  -> Eff es (TextAreaElement er HydrationDomSpace t)
textAreaElementHydrate cfg = do
  ((e, _), domRef) <- hydrateElement @er @t "textarea"
    (_textAreaElementConfig_elementConfig cfg) (pure ())
  (vUI, trigVUI)     <- HH.newTriggerEventE @t
  (vSet, trigVSet)   <- HH.newTriggerEventE @t
  (focus, trigFocus) <- HH.newTriggerEventE @t
  let v0 = _textAreaElementConfig_initialValue cfg
      valSetup = maybe (pure (pure v0)) (HH.holdE @t v0) (_textAreaElementConfig_setValue cfg)
  addHydrationStepWithSetup @t valSetup $ \switchoverBeh ->
    textAreaHydrateRunner @er @t e domRef cfg switchoverBeh
      TextAreaHydrateTriggers { thtValUI = trigVUI, thtValSet = trigVSet
                              , thtFocus = trigFocus }
  hasFocusDyn <- RD.holdUniqDyn P.=<< HH.holdDynE @t False focus
  v <- HH.holdDynE @t v0 (R.leftmost [vSet, vUI])
  pure TextAreaElement
    { _textAreaElement_value    = v
    , _textAreaElement_input    = vUI
    , _textAreaElement_hasFocus = hasFocusDyn
    , _textAreaElement_element  = e
    , _textAreaElement_raw      = ()
    }

data TextAreaHydrateTriggers = TextAreaHydrateTriggers
  { thtValUI  :: Text -> IO ()
  , thtValSet :: Text -> IO ()
  , thtFocus  :: Bool -> IO ()
  }

textAreaHydrateRunner
  :: forall er t es
   . ( HydrationRunnerE t es, R.Reflex t )
  => Element er HydrationDomSpace t
  -> IORef DOM.Element
  -> TextAreaElementConfig er t HydrationDomSpace
  -> R.Behavior t Text
  -> TextAreaHydrateTriggers
  -> Eff es ()
textAreaHydrateRunner e domRef cfg vBeh trigs = do
  switchoverValue <- HH.sampleE @t vBeh
  domElement <- liftIO $ readIORef domRef
  let dom    = uncheckedCastTo DOM.HTMLTextAreaElement domElement
      getVal = TextArea.getValue dom
      v0     = _textAreaElementConfig_initialValue cfg
  when (v0 /= switchoverValue) $
    liftIO $ thtValSet trigs switchoverValue
  realValue <- liftJSM_ getVal
  when (realValue /= switchoverValue) $
    liftIO $ thtValUI trigs realValue
  requestDomAction_ @t $
    (getVal P.>>= liftIO . thtValUI trigs)
      <$ R.select (_element_events e) (WrapArg Input)
  for_ (_textAreaElementConfig_setValue cfg) $ \eSet ->
    requestDomAction_ @t $ RC.ffor eSet $ \v' -> do
      TextArea.setValue dom v'
      v <- getVal
      liftIO $ thtValSet trigs v
  installFocusListener @t e domElement (thtFocus trigs)

-- =====================================================================
-- Select element (immediate + internal + hydrate).
-- =====================================================================

{-# INLINABLE selectElementImmediate #-}
selectElementImmediate
  :: forall er t s a es
   . ( WidgetE t s es
     , EventSpec s ~ GhcjsEventSpec
     , RH.ReflexHost t
     )
  => SelectElementConfig er t s
  -> Eff es a
  -> Eff es (SelectElement er GhcjsDomSpace t, a)
selectElementImmediate cfg child = do
  (e, result) <- elementImmediate @er @t @s "select"
    (_selectElementConfig_elementConfig cfg) child
  let dom = uncheckedCastTo DOM.HTMLSelectElement (_element_raw e)
  liftJSM_ $ Select.setValue dom (_selectElementConfig_initialValue cfg)
  v0 <- liftJSM_ $ Select.getValue dom
  vByUI <- requestDomAction @t $
    Select.getValue dom
      <$ R.select (_element_events e) (WrapArg Change)
  vBySet <- selectWireSetValue @t dom (_selectElementConfig_setValue cfg)
  v <- HH.holdDynE @t v0 (R.leftmost [vBySet, vByUI])
  hasFocus <- mkHasFocus @er @GhcjsDomSpace @t e
  pure (SelectElement
    { _selectElement_value    = v
    , _selectElement_change   = vByUI
    , _selectElement_hasFocus = hasFocus
    , _selectElement_element  = e
    , _selectElement_raw      = dom
    }, result)

selectWireSetValue
  :: forall t es
   . ( DomRenderHookE t :> es, R.Reflex t, JSM' :> es )
  => DOM.HTMLSelectElement
  -> Maybe (R.Event t Text)
  -> Eff es (R.Event t Text)
selectWireSetValue _   Nothing        = pure R.never
selectWireSetValue dom (Just eSetVal) =
  requestDomAction @t $ RC.ffor eSetVal $ \v' -> do
    Select.setValue dom v'
    Select.getValue dom

{-# INLINABLE selectElementInternal #-}
selectElementInternal
  :: forall er t a es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => SelectElementConfig er t HydrationDomSpace
  -> Eff es a
  -> Eff es (SelectElement er HydrationDomSpace t, a)
selectElementInternal cfg child = getHydrationMode @t >>= \case
  HydrationMode_Immediate -> do
    (s, r) <- selectElementImmediate @er @t @HydrationDomSpace cfg child
    pure (s
      { _selectElement_element = Element (_element_events $ _selectElement_element s) ()
      , _selectElement_raw     = ()
      }, r)
  HydrationMode_Hydrating -> selectElementHydrate cfg child

selectElementHydrate
  :: forall er t a es
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     )
  => SelectElementConfig er t HydrationDomSpace
  -> Eff es a
  -> Eff es (SelectElement er HydrationDomSpace t, a)
selectElementHydrate cfg child = do
  ((e, result), domRef) <- hydrateElement @er @t "select"
    (_selectElementConfig_elementConfig cfg) child
  (vUI, trigVUI)     <- HH.newTriggerEventE @t
  (vSet, trigVSet)   <- HH.newTriggerEventE @t
  (focus, trigFocus) <- HH.newTriggerEventE @t
  addHydrationStep @t $
    selectHydrateRunner @er @t e domRef cfg
      SelectHydrateTriggers { shtValUI = trigVUI, shtValSet = trigVSet
                            , shtFocus = trigFocus }
  hasFocusDyn <- RD.holdUniqDyn P.=<< HH.holdDynE @t False focus
  v <- HH.holdDynE @t (_selectElementConfig_initialValue cfg) (R.leftmost [vSet, vUI])
  pure (SelectElement
    { _selectElement_value    = v
    , _selectElement_change   = vUI
    , _selectElement_hasFocus = hasFocusDyn
    , _selectElement_element  = e
    , _selectElement_raw      = ()
    }, result)

data SelectHydrateTriggers = SelectHydrateTriggers
  { shtValUI  :: Text -> IO ()
  , shtValSet :: Text -> IO ()
  , shtFocus  :: Bool -> IO ()
  }

selectHydrateRunner
  :: forall er t es
   . ( HydrationRunnerE t es, R.Reflex t )
  => Element er HydrationDomSpace t
  -> IORef DOM.Element
  -> SelectElementConfig er t HydrationDomSpace
  -> SelectHydrateTriggers
  -> Eff es ()
selectHydrateRunner e domRef cfg trigs = do
  domElement <- liftIO $ readIORef domRef
  let dom    = uncheckedCastTo DOM.HTMLSelectElement domElement
      getVal = Select.getValue dom
      v0     = _selectElementConfig_initialValue cfg
  realValue <- liftJSM_ getVal
  when (realValue /= v0) $ liftIO $ shtValUI trigs realValue
  requestDomAction_ @t $
    (getVal P.>>= liftIO . shtValUI trigs)
      <$ R.select (_element_events e) (WrapArg Change)
  for_ (_selectElementConfig_setValue cfg) $ \eSet ->
    requestDomAction_ @t $ RC.ffor eSet $ \v' -> do
      Select.setValue dom v'
      v <- getVal
      liftIO $ shtValSet trigs v
  installFocusListener @t e domElement (shtFocus trigs)

-- =====================================================================
-- Element.hs adjust drivers (runWithReplaceEff +
-- hoistTraverse* helpers).
--
-- runWithReplaceEff delegates the base traversal to HH.runWithReplaceE
-- (eff-reflex's Adjustable send-wrapper), preserving the cohort-version
-- semantics + parent-unready-child counter via IORefs.
--
-- The hoistTraverse* helpers are the DMap / IntMap analogs; they
-- accept the base traversal as an argument so both no-move and
-- with-move variants share the same wiring shape (upstream reuses
-- the same code path for both).
-- =====================================================================

-- | 'numberOccurrences' shaped for the Effectful port. Assigns each
-- occurrence a monotonically increasing 'Int' index, starting at 0.
{-# INLINABLE numberOccurrencesEff #-}
numberOccurrencesEff
  :: forall t es a
   . (HH.Hold t :> es, HH.Sample t :> es, MonadFix (Eff es), R.Reflex t)
  => R.Event t a -> Eff es (R.Event t (Int, a))
numberOccurrencesEff ev = do
  (_dyn, out) <- RC.mapAccumMaybeDyn
                   (\n a -> let !next = n + 1 in (Just next, Just (n, a)))
                   0
                   ev
  pure out

{-# INLINABLE runWithReplaceEff #-}
runWithReplaceEff
  :: forall t s a b es
   . ( WidgetE t s es
     , RH.ReflexHost t
     , MonadFix (Eff es)
     )
  => Eff es a
  -> R.Event t (Eff es b)
  -> Eff es (a, R.Event t b)
runWithReplaceEff a0 a' = do
  initialEnv <- ask @(HydrationEnv t)
  let hydrating              = hydrationEnv_hydrationMode  initialEnv
      parentUnreadyChildren  = hydrationEnv_unreadyChildren initialEnv
  (hydrateStart, before, beforeKey) <- skipToReplaceStart @t @s
  haveEverBeenReady <- liftIO $ newIORef False
  currentCohort     <- liftIO $ newIORef (-1 :: Int)
  let myCommitAction :: JSM ()
      myCommitAction = do
        liftIO (readIORef haveEverBeenReady) >>= \case
          True -> pure ()
          False -> do
            liftIO $ writeIORef haveEverBeenReady True
            old <- liftIO $ readIORef parentUnreadyChildren
            let !new = P.pred old
            liftIO $ writeIORef parentUnreadyChildren new
            when (new == 0) $ hydrationEnv_commitAction initialEnv
  doc    <- asks @(HydrationEnv t) hydrationEnv_document
  parent <- getParent @t
  (hydrateEnd, after) <- skipToReplaceEnd @t @s beforeKey
  let drawInitialChild :: Eff es (DelayedHydration t, a)
      drawInitialChild = do
        h <- liftIO $ readIORef hydrating
        p' <- case h of
          HydrationMode_Hydrating -> pure parent
          HydrationMode_Immediate -> toNode <$> liftJSM_ (Document.createDocumentFragment doc)
        unreadyChildren <- liftIO $ newIORef 0
        let a0Wrapped = case h of
              HydrationMode_Hydrating -> a0
              HydrationMode_Immediate -> do
                a <- a0
                afterN <- liftIO $ readIORef after
                _ <- insertBefore p' afterN
                pure a
        delayed <- case h of
          HydrationMode_Hydrating -> liftIO $ newIORef (RunnerAction (pure ()) :: DelayedHydration t)
          HydrationMode_Immediate -> pure $ hydrationEnv_delayed initialEnv
        result <- localEnv @t (\env -> env
              { hydrationEnv_unreadyChildren = unreadyChildren
              , hydrationEnv_commitAction    = myCommitAction
              , hydrationEnv_parent          = Left p'
              , hydrationEnv_delayed         = delayed
              }) a0Wrapped
        dom <- case h of
          HydrationMode_Hydrating -> liftIO $ readIORef delayed
          HydrationMode_Immediate -> pure (RunnerAction (pure ()))
        liftIO $ readIORef unreadyChildren >>= \case
          0 -> writeIORef haveEverBeenReady True
          _ -> modifyIORef' parentUnreadyChildren P.succ
        pure (dom, result)
  a'' <- numberOccurrencesEff @t a'
  let drawChild :: Int -> Eff es b -> Eff es (Either (DelayedHydration t) (Maybe (JSM ())), b)
      drawChild cohortId child = do
        h <- liftIO $ readIORef hydrating
        p' <- case h of
          HydrationMode_Hydrating -> pure parent
          HydrationMode_Immediate -> toNode <$> liftJSM_ (Document.createDocumentFragment doc)
        unreadyChildren <- liftIO $ newIORef 0
        let commitAction :: JSM ()
            commitAction = do
              c <- liftIO $ readIORef currentCohort
              when (c P.<= cohortId) $ do
                !beforeN <- liftIO $ readIORef before
                !afterN  <- liftIO $ readIORef after
                deleteBetweenExclusiveJSM' (toNode beforeN) (toNode afterN)
                insertBeforeJSM' p' afterN
                liftIO $ writeIORef currentCohort cohortId
                myCommitAction
        delayed <- case h of
          HydrationMode_Hydrating -> liftIO $ newIORef (RunnerAction (pure ()) :: DelayedHydration t)
          HydrationMode_Immediate -> pure $ hydrationEnv_delayed initialEnv
        -- Fresh postBuild event for THIS replacement widget. Upstream
        -- reflex-dom-core's HydrationDomBuilderT gives each replacement
        -- widget its own PostBuild that fires when THAT widget is built
        -- (via 'PostBuildT' per-scope). The port's 'HH.runPostBuild ev'
        -- returns the SAME app-level pb event for all consumers, which
        -- breaks widgets like:
        --
        --   runWithReplace (pure ()) $ ffor replace $ \\() -> do
        --     pb <- getPostBuild
        --     performEvent_ $ liftIO (putMVar lock ()) <$ pb
        --
        -- because 'getPostBuild' inside the replacement returns the
        -- long-since-fired app pb, and 'performEvent_' never fires.
        --
        -- Fix: allocate a per-replacement pb via 'newTriggerEventE'
        -- (chan-based fire, drained in a subsequent frame) + 'interpose'
        -- the PostBuild handler for the child's scope. After the child
        -- widget is built, fire the trigger — the fire lands in a
        -- later frame, after the widget's subscriptions are in place.
        (freshPb, firePb) <- HH.newTriggerEventE @t
        result <- localEnv @t (\env -> env
              { hydrationEnv_unreadyChildren = unreadyChildren
              , hydrationEnv_commitAction    = case h of
                  HydrationMode_Hydrating -> myCommitAction
                  HydrationMode_Immediate -> commitAction
              , hydrationEnv_parent          = Left p'
              , hydrationEnv_delayed         = delayed
              })
              (interpose @(HH.PostBuild t)
                (\_ HH.GetPostBuild -> pure freshPb)
                child)
        liftIO (firePb ())
        dom <- case h of
          HydrationMode_Hydrating -> liftIO $ readIORef delayed
          HydrationMode_Immediate -> pure (RunnerAction (pure ()))
        uc <- liftIO $ readIORef unreadyChildren
        let commitActionToRunNow
              | uc == 0   = Just commitAction
              | P.otherwise = Nothing
            actions = case h of
              HydrationMode_Hydrating -> Left dom
              HydrationMode_Immediate -> Right commitActionToRunNow
        pure (actions, result)
  ((hydrate0, result0), child') <-
    -- Route through 'HH.runWithReplaceE' — the send-based Adjustable
    -- effect helper. Fires 'send @(Adjustable t) (RunWithReplace a0 a')'
    -- which the composer's Adjustable interpreter catches. The current
    -- interpreter ('runPerformEventAndAdjustable' at HeadlessEffectful.hs:1268)
    -- wraps every cohort build in 'runInCohortIO cohortPtr cohortNSel' —
    -- the per-cohort scope management required for nested runWithReplace
    -- where the INNER runWithReplace lives inside the OUTER's replacement
    -- widget (fired by pb). Without that scope wrap the inner's
    -- pushCheap fires in a torn-down forked-thread env and the inner
    -- replacement's DOM update never propagates.
    HH.runWithReplaceE @t drawInitialChild
      (RC.ffor a'' $ \(cid, child) -> drawChild cid child)
  let (hydrate', commitEv) = R.fanEither $ fmap fst child'
  addHydrationStepWithSetup @t
    (HH.holdE @t hydrate0 hydrate')
    (\contents -> do
        unRunnerAction hydrateStart
        sampled <- HH.sampleE @t contents
        unRunnerAction sampled
        unRunnerAction hydrateEnd)
  requestDomAction_ @t (R.fmapMaybe P.id commitEv)
  pure (result0, snd <$> child')



-- | Raw-JSM DOM helpers (used inside commit actions fired from
-- requestDomAction_, which requires JSM bodies, not Eff bodies).
deleteBetweenExclusiveJSM' :: Node -> Node -> JSM ()
deleteBetweenExclusiveJSM' s e = do
  df <- Document.createDocumentFragment =<< Node.getOwnerDocumentUnchecked s
  extractBetweenExclusiveJSM df s e

insertBeforeJSM' :: (IsNode a, IsNode b) => a -> b -> JSM ()
insertBeforeJSM' new existing = do
  p <- Node.getParentNodeUnchecked existing
  Node.insertBefore_ p new (Just existing)

{-# INLINABLE hoistTraverseWithKeyWithAdjust #-}
hoistTraverseWithKeyWithAdjust
  :: forall t s k v v' p es
   . ( WidgetE t s es
     , RH.ReflexHost t
     , GCompare k
     , Patch (p k (Compose (TraverseChild t (Some k)) v'))
     , PatchTarget (p k (Compose (TraverseChild t (Some k)) v'))
         ~ DMap k (Compose (TraverseChild t (Some k)) v')
     , Monoid            (p k (Compose (TraverseChild t (Some k)) v'))
     )
  => ( (forall a. k a -> v a -> Eff es (Compose (TraverseChild t (Some k)) v' a))
       -> DMap k v
       -> R.Event t (p k v)
       -> Eff es ( DMap k (Compose (TraverseChild t (Some k)) v')
                 , R.Event t (p k (Compose (TraverseChild t (Some k)) v'))))
  -> (forall vv vv'2. (forall a. vv a -> vv'2 a) -> p k vv -> p k vv'2)
  -> ( p k (Compose (TraverseChild t (Some k)) v')
       -> DMap k (Constant (IORef (ChildReadyState (Some k))))
       -> IO (DMap k (Constant (IORef (ChildReadyState (Some k))))))
  -> ( IORef (MapS.Map (Some k) DOM.Text)
       -> DOM.Text
       -> p k (Compose (TraverseChild t (Some k)) v')
       -> JSM ())
  -> (forall a. k a -> v a -> Eff es (v' a))
  -> DMap k v
  -> R.Event t (p k v)
  -> Eff es (DMap k v', R.Event t (p k v'))
hoistTraverseWithKeyWithAdjust base mapPatch_ updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  doc         <- asks @(HydrationEnv t) hydrationEnv_document
  initialEnv  <- ask @(HydrationEnv t)
  let parentUnreadyChildren = hydrationEnv_unreadyChildren initialEnv
  pendingChange :: IORef ( DMap k (Constant (IORef (ChildReadyState (Some k))))
                         , p k (Compose (TraverseChild t (Some k)) v')
                         ) <- liftIO $ newIORef mempty
  haveEverBeenReady <- liftIO $ newIORef False
  placeholders      <- liftIO $ newIORef MapS.empty
  lastPlaceholder   <- liftJSM_ (Document.createTextNode doc ("" :: Text))
  let markSelfReady :: JSM ()
      markSelfReady = do
        liftIO (readIORef haveEverBeenReady) >>= \case
          True -> pure ()
          False -> do
            liftIO $ writeIORef haveEverBeenReady True
            old <- liftIO $ readIORef parentUnreadyChildren
            let !new = P.pred old
            liftIO $ writeIORef parentUnreadyChildren new
            when (new == 0) $ hydrationEnv_commitAction initialEnv
      applyDomUpdate :: p k (Compose (TraverseChild t (Some k)) v') -> JSM ()
      applyDomUpdate p = do
        applyDomUpdate_ placeholders lastPlaceholder p
        markSelfReady
        liftIO $ writeIORef pendingChange $! mempty
      markChildReady :: IORef (ChildReadyState (Some k)) -> JSM ()
      markChildReady childReadyState = do
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyState_Ready -> pure ()
          ChildReadyState_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyState_Ready
            case countedAt of
              Nothing -> pure ()
              Just (Some k) -> do
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (P.not $ DMap.null oldUnready) $ do
                  let newUnready = DMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (DMap.null newUnready) $
                    applyDomUpdate p
  let childFn :: forall a. k a -> v a -> Eff es (Compose (TraverseChild t (Some k)) v' a)
      childFn k v = drawChildUpdate @t @s initialEnv markChildReady (f k v)
  (children0, children') <- base childFn dm0 dm'
  let processChild :: forall a. k a -> Compose (TraverseChild t (Some k)) v' a -> IO (ComposeMaybe (Constant (IORef (ChildReadyState (Some k)))) a)
      processChild k (Compose (TraverseChild e _)) = case e of
        Left _ -> pure (ComposeMaybe Nothing)
        Right immediate -> ComposeMaybe <$> do
          readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
            ChildReadyState_Ready -> pure Nothing
            ChildReadyState_Unready _ -> do
              writeIORef (_traverseChildImmediate_childReadyState immediate) $
                ChildReadyState_Unready (Just (Some k))
              pure $ Just (Constant (_traverseChildImmediate_childReadyState immediate))
  initialUnready <- liftIO $
    DMap.mapMaybeWithKey (\_ -> getComposeMaybe) <$> DMap.traverseWithKey processChild children0
  liftIO $ if DMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren P.succ
      writeIORef pendingChange (initialUnready, mempty)
  getHydrationMode @t >>= \case
    HydrationMode_Hydrating ->
      addHydrationStepWithSetup @t
        (HH.holdIncrementalE @t children0 children')
        (\children -> do
            dm <- HH.sampleE @t (R.currentIncremental children)
            let getPh (Compose (TraverseChild mode _)) = case mode of
                  Left hyd  -> unRunnerAction (_traverseChildHydration_delayed hyd)
                  Right imm -> pure (_traverseChildImmediate_placeholder imm)
            phs <- P.sequenceA $ weakenDMapWith getPh dm
            liftIO $ writeIORef placeholders $! phs
            insertAfterPreviousNode lastPlaceholder)
    HydrationMode_Immediate -> do
      let activate imm = do
            append @t (toNode (_traverseChildImmediate_fragment imm))
            pure (_traverseChildImmediate_placeholder imm)
          getPh :: forall a. Compose (TraverseChild t (Some k)) v' a -> Eff es DOM.Text
          getPh (Compose (TraverseChild mode _)) = case mode of
            Left _   -> error "hoistTraverseWithKeyWithAdjust: hydration slot in immediate mode is impossible"
            Right imm -> activate imm
      phs <- P.sequenceA $ weakenDMapWith getPh children0
      liftIO $ writeIORef placeholders $! phs
      append @t (toNode lastPlaceholder)
  requestDomAction_ @t $ RC.ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (DMap.null newUnready) $ applyDomUpdate newP
  let result0 = DMap.map (_traverseChild_result . getCompose) children0
      result' = RC.ffor children' $ mapPatch_ $ _traverseChild_result . getCompose
  pure (result0, result')

{-# INLINABLE hoistTraverseIntMapWithKeyWithAdjust #-}
hoistTraverseIntMapWithKeyWithAdjust
  :: forall t s v v' p es
   . ( WidgetE t s es
     , RH.ReflexHost t
     , Patch (p (TraverseChild t Int v'))
     , PatchTarget (p (TraverseChild t Int v')) ~ IntMap (TraverseChild t Int v')
     , Monoid (p (TraverseChild t Int v'))
     , Functor p
     )
  => ( (Int -> v -> Eff es (TraverseChild t Int v'))
       -> IntMap v
       -> R.Event t (p v)
       -> Eff es (IntMap (TraverseChild t Int v'), R.Event t (p (TraverseChild t Int v'))))
  -> ( p (TraverseChild t Int v')
       -> IntMap (IORef (ChildReadyState Int))
       -> IO (IntMap (IORef (ChildReadyState Int))))
  -> ( IORef (IntMap DOM.Text)
       -> DOM.Text
       -> p (TraverseChild t Int v')
       -> JSM ())
  -> (Int -> v -> Eff es v')
  -> IntMap v
  -> R.Event t (p v)
  -> Eff es (IntMap v', R.Event t (p v'))
hoistTraverseIntMapWithKeyWithAdjust base updateChildUnreadiness applyDomUpdate_ f dm0 dm' = do
  doc         <- asks @(HydrationEnv t) hydrationEnv_document
  initialEnv  <- ask @(HydrationEnv t)
  let parentUnreadyChildren = hydrationEnv_unreadyChildren initialEnv
  pendingChange :: IORef ( IntMap (IORef (ChildReadyState Int))
                         , p (TraverseChild t Int v')
                         ) <- liftIO $ newIORef mempty
  haveEverBeenReady <- liftIO $ newIORef False
  placeholders      <- liftIO $ newIORef IntMap.empty
  lastPlaceholder   <- liftJSM_ (Document.createTextNode doc ("" :: Text))
  let markSelfReady :: JSM ()
      markSelfReady = do
        liftIO (readIORef haveEverBeenReady) >>= \case
          True -> pure ()
          False -> do
            liftIO $ writeIORef haveEverBeenReady True
            old <- liftIO $ readIORef parentUnreadyChildren
            let !new = P.pred old
            liftIO $ writeIORef parentUnreadyChildren new
            when (new == 0) $ hydrationEnv_commitAction initialEnv
      applyDomUpdate :: p (TraverseChild t Int v') -> JSM ()
      applyDomUpdate p = do
        applyDomUpdate_ placeholders lastPlaceholder p
        markSelfReady
        liftIO $ writeIORef pendingChange $! mempty
      markChildReady :: IORef (ChildReadyState Int) -> JSM ()
      markChildReady childReadyState =
        liftIO (readIORef childReadyState) >>= \case
          ChildReadyState_Ready -> pure ()
          ChildReadyState_Unready countedAt -> do
            liftIO $ writeIORef childReadyState ChildReadyState_Ready
            case countedAt of
              Nothing -> pure ()
              Just k -> do
                (oldUnready, p) <- liftIO $ readIORef pendingChange
                when (P.not $ IntMap.null oldUnready) $ do
                  let newUnready = IntMap.delete k oldUnready
                  liftIO $ writeIORef pendingChange (newUnready, p)
                  when (IntMap.null newUnready) $
                    applyDomUpdate p
  let childFn :: Int -> v -> Eff es (TraverseChild t Int v')
      childFn k v = do
        (Compose (TraverseChild mode a)) <- drawChildUpdate @t @s initialEnv markChildReady (Identity <$> f k v)
        pure (TraverseChild mode (runIdentity a))
  (children0, children') <- base childFn dm0 dm'
  let processChild :: Int -> TraverseChild t Int v' -> IO (Maybe (IORef (ChildReadyState Int)))
      processChild k (TraverseChild e _) = case e of
        Left _ -> pure Nothing
        Right immediate ->
          readIORef (_traverseChildImmediate_childReadyState immediate) >>= \case
            ChildReadyState_Ready -> pure Nothing
            ChildReadyState_Unready _ -> do
              writeIORef (_traverseChildImmediate_childReadyState immediate) $
                ChildReadyState_Unready (Just k)
              pure $ Just (_traverseChildImmediate_childReadyState immediate)
  initialUnready <- liftIO $ IntMap.mapMaybe P.id <$> IntMap.traverseWithKey processChild children0
  liftIO $ if IntMap.null initialUnready
    then writeIORef haveEverBeenReady True
    else do
      modifyIORef' parentUnreadyChildren P.succ
      writeIORef pendingChange (initialUnready, mempty)
  getHydrationMode @t >>= \case
    HydrationMode_Hydrating ->
      addHydrationStepWithSetup @t
        (HH.holdIncrementalE @t children0 children')
        (\children -> do
            dm <- HH.sampleE @t (R.currentIncremental children)
            let getPh (TraverseChild mode _) = case mode of
                  Left hyd  -> unRunnerAction (_traverseChildHydration_delayed hyd)
                  Right imm -> pure (_traverseChildImmediate_placeholder imm)
            phs <- P.traverse getPh dm
            liftIO $ writeIORef placeholders $! phs
            insertAfterPreviousNode lastPlaceholder)
    HydrationMode_Immediate -> do
      let activate imm = do
            append @t (toNode (_traverseChildImmediate_fragment imm))
            pure (_traverseChildImmediate_placeholder imm)
          getPh (TraverseChild mode _) = case mode of
            Left _   -> error "hoistTraverseIntMapWithKeyWithAdjust: hydration slot in immediate mode is impossible"
            Right imm -> activate imm
      phs <- P.traverse getPh children0
      liftIO $ writeIORef placeholders $! phs
      append @t (toNode lastPlaceholder)
  requestDomAction_ @t $ RC.ffor children' $ \p -> do
    (oldUnready, oldP) <- liftIO $ readIORef pendingChange
    newUnready <- liftIO $ updateChildUnreadiness p oldUnready
    let !newP = p <> oldP
    liftIO $ writeIORef pendingChange (newUnready, newP)
    when (IntMap.null newUnready) $ applyDomUpdate newP
  let result0 = IntMap.map _traverseChild_result children0
      result' = fmap (fmap _traverseChild_result) children'
  pure (result0, result')

-- =====================================================================
-- Instances.hs: DomBuilderE Effect GADT interpreters.
--
-- Wires DomBuilderE t GhcjsDomSpace + DomBuilderE t HydrationDomSpace
-- constructors to Element.hs / Wrap.hs factories via 'interpret' +
-- 'localSeqUnlift'. Also wires HasDocumentE Ghcjs / Hydration and
-- ports the 6 traverse-with-adjust child-readiness / DOM-update
-- closures (used by Adjustable interpreters).
-- =====================================================================

-- | Interpret HasDocumentE GhcjsDomSpace by reading the ambient
-- Document from hydrationEnv_document.
runHasDocumentGhcjs
  :: forall t es a
   . ( WidgetE t GhcjsDomSpace es
     )
  => Eff (HasDocumentE GhcjsDomSpace : es) a
  -> Eff es a
runHasDocumentGhcjs = interpret $ \_ cmd -> case cmd of
  AskDocument -> asks @(HydrationEnv t) hydrationEnv_document

-- | Interpret HasDocumentE HydrationDomSpace — same shape as
-- runHasDocumentGhcjs (upstream's @HasDocumentE (HydrationDomBuilderT
-- s t m)@ deriving-instance forwards to the base monad).
runHasDocumentHydration
  :: forall t es a
   . ( WidgetE t HydrationDomSpace es
     )
  => Eff (HasDocumentE HydrationDomSpace : es) a
  -> Eff es a
runHasDocumentHydration = interpret $ \_ cmd -> case cmd of
  AskDocument -> asks @(HydrationEnv t) hydrationEnv_document

-- | Interpret DomBuilderE t GhcjsDomSpace by delegating every ctor to
-- its *Immediate factory in Element.hs. Child continuations of
-- Element / SelectElement are unlifted via 'localSeqUnlift' so they
-- run in the same effect row the caller sent from.
runDomBuilderImmediate
  :: forall t es a
   . ( WidgetE t GhcjsDomSpace es
     , RH.ReflexHost t
     , MonadFix (Eff es)
     , IOE :> es
     )
  => Eff (DomBuilderE t GhcjsDomSpace : es) a
  -> Eff es a
runDomBuilderImmediate = interpret $ \env cmd -> case cmd of
  DB_TextNode        cfg          ->
    TextNode    <$> textNodeImmediate    @t @GhcjsDomSpace cfg
  DB_CommentNode     cfg          ->
    CommentNode <$> commentNodeImmediate @t @GhcjsDomSpace cfg
  DB_Element         tag cfg body ->
    localSeqUnlift env $ \unlift ->
      elementImmediate @_ @t @GhcjsDomSpace tag cfg (unlift body)
  DB_InputElement    cfg          ->
    inputElementImmediate    @_ @t @GhcjsDomSpace cfg
  DB_TextAreaElement cfg          ->
    textAreaElementImmediate @_ @t @GhcjsDomSpace cfg
  DB_SelectElement   cfg body     ->
    localSeqUnlift env $ \unlift ->
      selectElementImmediate @_ @t @GhcjsDomSpace cfg (unlift body)
  DB_PlaceRawElement raw          -> append @t (DOM.toNode raw)
  DB_WrapRawElement  raw rawCfg   -> wrap @_ @t @GhcjsDomSpace P.id raw rawCfg

-- | Interpret DomBuilderE t HydrationDomSpace by delegating every ctor
-- to its *Internal factory in Element.hs. The *Internal factories
-- dispatch on the ambient HydrationMode, serving both fresh-DOM and
-- hydrate-existing-DOM paths.
--
-- placeRawElement / wrapRawElement for HydrationDomSpace are trivial
-- no-ops (RawElement HydrationDomSpace = ()), matching upstream.
runDomBuilderHydration
  :: forall t es a
   . ( WidgetE t HydrationDomSpace es
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , MonadFix (Eff es)
     , IOE :> es
     )
  => Eff (DomBuilderE t HydrationDomSpace : es) a
  -> Eff es a
runDomBuilderHydration = interpret $ \env cmd -> case cmd of
  DB_TextNode        cfg          -> textNodeInternal    @t cfg
  DB_CommentNode     cfg          -> commentNodeInternal @t cfg
  DB_Element         tag cfg body ->
    localSeqUnlift env $ \unlift ->
      elementInternal @_ @t tag cfg (unlift body)
  DB_InputElement    cfg          -> inputElementInternal    @_ @t cfg
  DB_TextAreaElement cfg          -> textAreaElementInternal @_ @t cfg
  DB_SelectElement   cfg body     ->
    localSeqUnlift env $ \unlift ->
      selectElementInternal @_ @t cfg (unlift body)
  DB_PlaceRawElement ()           -> pure ()
  DB_WrapRawElement  () _rawCfg   -> pure $ Element (R.EventSelector $ P.const R.never) ()

-- ---------------------------------------------------------------------
-- IntMap child-readiness + DOM-update closures
-- ---------------------------------------------------------------------

updateIntMapChildUnreadiness
  :: forall t v'
   . PatchIntMap (TraverseChild t Int v')
  -> IntMap (IORef (ChildReadyState Int))
  -> IO (IntMap (IORef (ChildReadyState Int)))
updateIntMapChildUnreadiness p@(PatchIntMap pInner) old = do
  let new :: IntMap.Key -> Maybe (TraverseChild t Int v') -> IO (Maybe (IORef (ChildReadyState Int)))
      new k m = case m of
        Nothing -> pure Nothing
        Just (TraverseChild (Left _hydration) _) -> pure Nothing
        Just (TraverseChild (Right immediate) _) -> do
          let sRef = _traverseChildImmediate_childReadyState immediate
          readIORef sRef >>= \case
            ChildReadyState_Ready -> pure Nothing
            ChildReadyState_Unready _ -> do
              writeIORef sRef (ChildReadyState_Unready (Just k))
              pure (Just sRef)
      del _ sRef = do
        writeIORef sRef (ChildReadyState_Unready Nothing)
        pure ()
  p' <- PatchIntMap <$> IntMap.traverseWithKey new pInner
  _  <- IntMap.traverseWithKey del (FastMutableIntMap.getDeletions p old)
  pure (Patch.applyAlways p' old)

applyDomUpdateIntMap
  :: forall t v'
   . IORef (IntMap DOM.Text)
  -> DOM.Text
  -> PatchIntMap (TraverseChild t Int v')
  -> DOM.JSM ()
applyDomUpdateIntMap placeholders lastPlaceholder (PatchIntMap p) = do
  phs <- liftIO $ readIORef placeholders
  forM_ (IntMap.toList p) $ \(k, mv) -> do
    let nextPlaceholder = maybe lastPlaceholder snd (IntMap.lookupGT k phs)
    forM_ (IntMap.lookup k phs) $ \thisPlaceholder ->
      deleteUpToJSM thisPlaceholder nextPlaceholder
    forM_ mv $ \(TraverseChild e _) -> case e of
      Left _hydration -> pure ()
      Right immediate ->
        insertBeforeJSM (_traverseChildImmediate_fragment immediate) nextPlaceholder
  let filtered :: PatchIntMap DOM.Text
      filtered = PatchIntMap $ P.flip IntMap.mapMaybe p $ \case
        Nothing -> Just Nothing
        Just tc
          | Right immediate <- _traverseChild_mode tc ->
              Just (Just (_traverseChildImmediate_placeholder immediate))
          | P.otherwise -> Nothing
  liftIO $ writeIORef placeholders $! fromMaybe phs (Patch.apply filtered phs)

-- ---------------------------------------------------------------------
-- DMap (no-move) child-readiness + DOM-update closures
-- ---------------------------------------------------------------------

updateDMapChildUnreadiness
  :: forall t k v'. GCompare k
  => PatchDMap k (Compose (TraverseChild t (Some k)) v')
  -> DMap k (Constant (IORef (ChildReadyState (Some k))))
  -> IO (DMap k (Constant (IORef (ChildReadyState (Some k)))))
updateDMapChildUnreadiness p old = do
  let new :: forall a. k a
          -> ComposeMaybe (Compose (TraverseChild t (Some k)) v') a
          -> IO (ComposeMaybe (Constant (IORef (ChildReadyState (Some k)))) a)
      new k (ComposeMaybe m) = ComposeMaybe <$> case m of
        Nothing -> pure Nothing
        Just (Compose (TraverseChild (Left _hydration) _)) -> pure Nothing
        Just (Compose (TraverseChild (Right immediate) _)) -> do
          let sRef = _traverseChildImmediate_childReadyState immediate
          readIORef sRef >>= \case
            ChildReadyState_Ready -> pure Nothing
            ChildReadyState_Unready _ -> do
              writeIORef sRef (ChildReadyState_Unready (Just (Some k)))
              pure (Just (Constant sRef))
      del :: forall a. k a
          -> Constant (IORef (ChildReadyState (Some k))) a
          -> IO (Constant () a)
      del _ (Constant sRef) = do
        writeIORef sRef (ChildReadyState_Unready Nothing)
        pure (Constant ())
  p' <- fmap PatchDMap $ DMap.traverseWithKey new (unPatchDMap p)
  _  <- DMap.traverseWithKey del (PatchDMap.getDeletions p old)
  pure (Patch.applyAlways p' old)

applyDomUpdateDMap
  :: forall t k v'. GCompare k
  => IORef (MapS.Map (Some k) DOM.Text)
  -> DOM.Text
  -> PatchDMap k (Compose (TraverseChild t (Some k)) v')
  -> DOM.JSM ()
applyDomUpdateDMap placeholders lastPlaceholder (PatchDMap patch) = do
  phs <- liftIO $ readIORef placeholders
  forM_ (DMap.toList patch) $ \(k :=> ComposeMaybe mv) -> do
    let nextPlaceholder = maybe lastPlaceholder snd (MapS.lookupGT (Some k) phs)
    forM_ (MapS.lookup (Some k) phs) $ \thisPlaceholder ->
      deleteUpToJSM thisPlaceholder nextPlaceholder
    forM_ mv $ \(Compose (TraverseChild e _)) -> case e of
      Left _hydration -> pure ()
      Right immediate ->
        insertBeforeJSM (_traverseChildImmediate_fragment immediate) nextPlaceholder
  let weakened :: PatchMap (Some k) (Either (TraverseChildHydration t) (TraverseChildImmediate (Some k)))
      weakened = PatchDMap.weakenPatchDMapWith (_traverseChild_mode . getCompose) (PatchDMap patch)
      filtered :: PatchMap (Some k) DOM.Text
      filtered = PatchMap $ P.flip MapS.mapMaybe (unPatchMap weakened) $ \case
        Nothing -> Just Nothing
        Just (Left _hydration)  -> Nothing
        Just (Right immediate)  -> Just (Just (_traverseChildImmediate_placeholder immediate))
  liftIO $ writeIORef placeholders $! fromMaybe phs (Patch.apply filtered phs)

-- ---------------------------------------------------------------------
-- DMapWithMove child-readiness + DOM-update closures
-- ---------------------------------------------------------------------

updateDMapWithMoveChildUnreadiness
  :: forall t k v'. GCompare k
  => PatchDMapWithMove k (Compose (TraverseChild t (Some k)) v')
  -> DMap k (Constant (IORef (ChildReadyState (Some k))))
  -> IO (DMap k (Constant (IORef (ChildReadyState (Some k)))))
updateDMapWithMoveChildUnreadiness p old = do
  let new :: forall a. k a
          -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t (Some k)) v') a
          -> IO (PatchDMapWithMove.NodeInfo k (Constant (IORef (ChildReadyState (Some k)))) a)
      new k = PatchDMapWithMove.nodeInfoMapFromM $ \case
        PatchDMapWithMove.From_Insert (Compose (TraverseChild (Left _hydration) _)) ->
          pure PatchDMapWithMove.From_Delete
        PatchDMapWithMove.From_Insert (Compose (TraverseChild (Right immediate) _)) -> do
          let sRef = _traverseChildImmediate_childReadyState immediate
          readIORef sRef >>= \case
            ChildReadyState_Ready -> pure PatchDMapWithMove.From_Delete
            ChildReadyState_Unready _ -> do
              writeIORef sRef (ChildReadyState_Unready (Just (Some k)))
              pure (PatchDMapWithMove.From_Insert (Constant sRef))
        PatchDMapWithMove.From_Delete    -> pure PatchDMapWithMove.From_Delete
        PatchDMapWithMove.From_Move fromKey -> pure (PatchDMapWithMove.From_Move fromKey)
      deleteOrMove
        :: forall a. k a
        -> Product (Constant (IORef (ChildReadyState (Some k)))) (ComposeMaybe k) a
        -> IO (Constant () a)
      deleteOrMove _ (Pair (Constant sRef) (ComposeMaybe mToKey)) = do
        writeIORef sRef (ChildReadyState_Unready (Some <$> mToKey))
        pure (Constant ())
  p' <- fmap PatchDMapWithMove.unsafePatchDMapWithMove $
    DMap.traverseWithKey new (PatchDMapWithMove.unPatchDMapWithMove p)
  _  <- DMap.traverseWithKey deleteOrMove
    (PatchDMapWithMove.getDeletionsAndMoves p old)
  pure (Patch.applyAlways p' old)

applyDomUpdateDMapWithMove
  :: forall t k v'. GCompare k
  => IORef (MapS.Map (Some k) DOM.Text)
  -> DOM.Text
  -> PatchDMapWithMove k (Compose (TraverseChild t (Some k)) v')
  -> DOM.JSM ()
applyDomUpdateDMapWithMove placeholders lastPlaceholder p_ = do
  let p = PatchDMapWithMove.unPatchDMapWithMove p_
  phsBefore <- liftIO $ readIORef placeholders
  let collectIfMoved
        :: forall a. k a
        -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t (Some k)) v') a
        -> DOM.JSM (Constant (Maybe DOM.DocumentFragment) a)
      collectIfMoved k e = do
        let mThisPlaceholder = MapS.lookup (Some k) phsBefore
            nextPlaceholder  = maybe lastPlaceholder snd (MapS.lookupGT (Some k) phsBefore)
        case isJust (getComposeMaybe (PatchDMapWithMove._nodeInfo_to e)) of
          False -> do
            forM_ mThisPlaceholder $ \pl -> deleteUpToJSM pl nextPlaceholder
            pure (Constant Nothing)
          True ->
            Constant <$> P.traverse (\pl -> collectUpToJSM pl nextPlaceholder) mThisPlaceholder
  collected <- DMap.traverseWithKey collectIfMoved p
  let weakened :: PatchMapWithMove (Some k) (Either (TraverseChildHydration t) (TraverseChildImmediate (Some k)))
      weakened = PatchDMapWithMove.weakenPatchDMapWithMoveWith
        (_traverseChild_mode . getCompose) p_
      filtered :: PatchMapWithMove (Some k) DOM.Text
      filtered = PatchMapWithMove $ P.flip MapS.mapMaybe (PatchMapWithMove.unPatchMapWithMove weakened) $
        \(PatchMapWithMove.NodeInfo from to') ->
          P.flip PatchMapWithMove.NodeInfo to' <$> case from of
            PatchMapWithMove.From_Insert (Left _hydration) -> Nothing
            PatchMapWithMove.From_Insert (Right immediate) ->
              Just (PatchMapWithMove.From_Insert (_traverseChildImmediate_placeholder immediate))
            PatchMapWithMove.From_Delete    -> Just PatchMapWithMove.From_Delete
            PatchMapWithMove.From_Move k    -> Just (PatchMapWithMove.From_Move k)
      !phsAfter = fromMaybe phsBefore (Patch.apply filtered phsBefore)
      placeFragment
        :: forall a. k a
        -> PatchDMapWithMove.NodeInfo k (Compose (TraverseChild t (Some k)) v') a
        -> DOM.JSM (Constant () a)
      placeFragment k e = do
        let nextPlaceholder = maybe lastPlaceholder snd (MapS.lookupGT (Some k) phsAfter)
        case PatchDMapWithMove._nodeInfo_from e of
          PatchDMapWithMove.From_Insert (Compose (TraverseChild x _)) -> case x of
            Left _hydration -> pure ()
            Right immediate ->
              insertBeforeJSM (_traverseChildImmediate_fragment immediate) nextPlaceholder
          PatchDMapWithMove.From_Delete -> pure ()
          PatchDMapWithMove.From_Move fromKey -> case DMap.lookup fromKey collected of
            Just (Constant mdf) -> forM_ mdf $ \df -> insertBeforeJSM df nextPlaceholder
            Nothing -> error "applyDomUpdateDMapWithMove: 'From_Move' references a key not in the 'collected' map. This violates the 'PatchDMapWithMove' invariant that every 'From_Move fromKey' has a corresponding source entry — bug in the patch source."
        pure (Constant ())
  P.mapM_ (\(k :=> v) -> placeFragment k v P.>> pure ()) (DMap.toDescList p)
  liftIO $ writeIORef placeholders $! phsAfter

-- ---------------------------------------------------------------------
-- Local JSM-scope helpers (Element does not export the JSM variants)
-- ---------------------------------------------------------------------

insertBeforeJSM
  :: (DOM.IsNode a, DOM.IsNode b)
  => a -> b -> DOM.JSM ()
insertBeforeJSM new existing = do
  p <- Node.getParentNodeUnchecked existing
  Node.insertBefore_ p new (Just existing)

deleteUpToJSM
  :: (DOM.IsNode start, DOM.IsNode end)
  => start -> end -> DOM.JSM ()
deleteUpToJSM s0 e0 = do
  df   <- Document.createDocumentFragment =<< Node.getOwnerDocumentUnchecked s0
  eVal <- JS.toJSVal (DOM.toNode e0)
  go df (DOM.toNode s0) eVal
  where
    go df s eVal = do
      sVal <- JS.toJSVal s
      done <- JS.strictEqual sVal eVal
      when (P.not done) $ do
        mNext <- Node.getNextSibling s
        _ <- Node.appendChild df s
        case mNext of
          Nothing  -> pure ()
          Just nxt -> go df (DOM.toNode nxt) eVal

collectUpToJSM
  :: (DOM.IsNode start, DOM.IsNode end)
  => start -> end -> DOM.JSM DOM.DocumentFragment
collectUpToJSM s0 e0 = do
  df   <- Document.createDocumentFragment =<< Node.getOwnerDocumentUnchecked s0
  eVal <- JS.toJSVal (DOM.toNode e0)
  go df (DOM.toNode s0) eVal
  pure df
  where
    go df s eVal = do
      sVal <- JS.toJSVal s
      done <- JS.strictEqual sVal eVal
      when (P.not done) $ do
        mNext <- Node.getNextSibling s
        _ <- Node.appendChild df s
        case mNext of
          Nothing  -> pure ()
          Just nxt -> go df (DOM.toNode nxt) eVal

-- =====================================================================
-- Main.hs host bootstrap surface (immediate + hydration modes).
--
-- Covers upstream Reflex.Dom.Main's full public surface (see the
-- upstream-name-parity mapping table below): mainWidget, mainWidget',
-- mainWidgetInElementById, mainWidgetWithCss, mainWidgetWithHead,
-- mainWidgetWithHead', attachWidget, attachWidget', attachWidget'',
-- attachImmediateWidget, runImmediateWidgetWithHeadAndBody,
-- processAsyncEvents, replaceElementContents, hostPerformEventT,
-- newEventWithTriggerRef, FireCommand, EventChannel — plus the
-- hydration siblings (attachHydrationWidget(WithFailure),
-- mainHydrationWidget(WithHead(')|WithSwitchoverAction(')|
-- WithSwitchoverActionWithFailure'),
-- runHydrationWidgetWithHeadAndBody(WithFailure)).
--
-- Design: 'HH.runEffectfulHost' provides the BaseEffectfulHostRow (IO
-- + Timeline + HostFrame' + CreateTrigger + ReadEvent + SubscribeEvent
-- + Hold + Sample + NotReady). 'hostPerformEventT' layers PerformEvent
-- + Adjustable on top and returns a 'FireCommand'. The widget scope
-- adds TriggerEvent + PostBuild + Reader HydrationEnv + JSM' +
-- DomRenderHookE + HasDocumentE + DomBuilderE around each guest.
--
-- The bootstrap fires postBuild after the widget builds, then (in
-- hydration mode) fires syncEvent to trigger the delayed HydrationRunner
-- via 'runWithReplaceE'. 'processAsyncEvents' forks a drain loop that
-- consumes async trigger invocations off the shared events chan
-- (both 'hydrationEnv_events' and the TriggerEvent interpreter share
-- this single chan — element event handlers push to it, the drain
-- loop dispatches via the FireCommand).
-- =====================================================================

-- | Bring up jsaddle-warp on port from @JSADDLE_WARP_PORT@ (default
-- @3003@) and run the supplied JSM action inside the browser session.
--
-- Body matches upstream @Reflex.Dom.Internal.run@ verbatim on the
-- jsaddle-warp branch: partial 'read' semantics (matches upstream —
-- crashes on unparseable env value rather than silently defaulting),
-- log line preserved, default port 3003.
--
-- Cross-platform CPP dispatch is handled by @Reflex.Dom.Internal.run@
-- (in the @reflex-dom@ umbrella package). This fn is jsaddle-warp-only;
-- callers wanting GHCJS/WKWebView/Android/WASM should route through the
-- umbrella's @mainWidget*@ instead of this one.
withDomHost :: JSM () -> IO ()
withDomHost act = do
  port <- maybe 3003 TR.read <$> lookupEnv "JSADDLE_WARP_PORT"
  putStrLn $ "Running jsaddle-warp server on port " <> P.show port
  Warp.run port act

-- =====================================================================
-- mainWidget composer (immediate mode, Global timeline).
--
-- Interpreter stack layered per variant A's runInterpreterStack shape:
--
--   runEffectfulHost           -- IO base + BaseEffectfulHostRow discharge
--   . runTriggerEvent chan     -- OUTERMOST HeadlessRow: peels TriggerEvent
--   . runPostBuild pbEv        -- peels PostBuild
--   . runPerformEvent respEv   -- peels PerformEvent, returns tuple ((), aggActions)
--   . runAdjustable            -- peels Adjustable (needs PerformEvent :> es)
--   . runReader hEnv           -- DOM: HydrationEnv (used by all DOM interpreters)
--   . runJSM' ctx              -- DOM: JSaddle context
--   . runDomRenderHook         -- DOM: schedules JSM on Event fire
--   . runHasDocumentGhcjs      -- DOM: HasDocumentE
--   . runDomBuilderImmediate   -- DOM: DomBuilderE — innermost around widget
--
-- Design decisions:
--   * Pinned to SpiderTimeline Global (matches Instances.hs Global-
--     specific factories). To use a fresh timeline via
--     'withSpiderTimeline', a caller would need to write their own
--     composer against 'HH.runEffectfulHostFor' with a locally-
--     parameterised widget.
--   * The postBuild trigger is fired in the same 'HH.runEffectfulHost'
--     scope, right after the widget finishes building — see
--     'attachImmediateWidget'.
--   * 'hydrationEnv_events' and the TriggerEvent interpreter chan are
--     the SAME chan: 'attachImmediateWidget' + 'attachWidget'' /
--     'attachHydrationWidgetWithFailure' / 'runHydrationWidget-
--     WithHeadAndBodyWithFailure' allocate ONE chan and thread it
--     both into 'HH.runTriggerEvent' (so 'newTriggerEvent' fires
--     enqueue into it) AND into 'HydrationEnv' (so element event
--     handlers push to the same chan). 'processAsyncEvents' drains
--     both.
--
-- Every send in the widget lands on the SAME Global-tagged Spider
-- scheduler, so subscriptions from within the widget scope are
-- coherent with any outer subscribers set up in the same session.
-- =====================================================================

-- | Effect row satisfied by rows an 'ImmediateWidget' can run in.
-- Polymorphic in the timeline @t@; the entry points ('mainWidget' etc.)
-- pin @t = 'R.SI.SpiderTimeline' 'R.SI.Global'@, but subroutines that
-- carry this constraint may be written timeline-generic.
--
-- @'HH.PerformEvent' ('SpiderTag' t)@ and @'HH.HostFrame'' ('SpiderTag' t)@
-- and @'RPE.Performable' ('Eff' es) ~ 'R.SI.SpiderHostFrame' ('SpiderTag' t)@
-- recover the spider host tag @x@ from @t = 'R.SI.SpiderTimeline' x@; the
-- 'SpiderTag' type family stays stuck if @t@ is not a Spider timeline,
-- so the constraint refuses to solve for non-Spider timelines — matching
-- variant B's implicit @DomTimeline = Spider@ assumption.
type ImmediateWidgetE (t :: Type) es =
  ( MonadFix (Eff es)
  , HH.PerformableForRow es ~ RH.HostFrame t
  , DomBuilderSpaceE es ~ GhcjsDomSpace
  , DomBuilderTimelineE es ~ t
  , DomBuilderE t GhcjsDomSpace :> es
  , HasDocumentE GhcjsDomSpace :> es
  , DomRenderHookE t :> es
  , Reader (HydrationEnv t) :> es
  , HH.PostBuild t :> es
  , HH.TriggerEvent t :> es
  , HH.Adjustable t :> es
  , HH.PerformEventM (RH.HostFrame t) t :> es
  , HH.PerformEventH t :> es
  , HH.CreateTrigger t :> es
  , HH.NotReady t :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , JSM' :> es
  , IOE :> es
  )

-- | Widget monad — newtype wrapper around
-- @WithJSContextSingleton x (Eff es)@. Just a nicer name; carries the
-- same load-bearing @WithJSContextSingleton x@ layer that makes
-- @MonadJSM (Performable m)@ resolve for upstream @Prerender t m@.
--
-- All MTL bridge instances (DomBuilder / HasDocument / DomRenderHook /
-- MountableDomBuilder / Prerender / MonadJSM / MonadHold / MonadSample
-- / PostBuild / TriggerEvent / Adjustable / PerformEvent) are derived
-- via GeneralizedNewtypeDeriving from the wrapped monad.
-- | Timeline-polymorphic Widget monad newtype. The @t@ phantom parameter
-- appears on the LHS so that @Performable (WidgetEff t x es) = HostFrame t@
-- is legal (upstream-verbatim shape). At instantiation @t@ is pinned via
-- a @t ~ SpiderTimeline x@ constraint at the class instance level.
newtype WidgetEff (t :: Type) (x :: Type) (es :: [Effect]) (a :: Type) = WidgetEff
  { unWidgetEff :: WithJSContextSingleton x (Eff es) a }
  deriving newtype (Functor, P.Applicative, P.Monad)

deriving newtype instance IOE :> es => MonadIO (WidgetEff t x es)
deriving newtype instance IOE :> es => MonadFix (WidgetEff t x es)

-- ---------------------------------------------------------------------
-- MTL bridge instances — all one-liners via GeneralizedNewtypeDeriving.
-- Each lifts the corresponding instance on
-- @WithJSContextSingleton x (Eff es)@ (which either exists via
-- the port's own bridges or via jsaddle / MonadTransControl lifting).
-- ---------------------------------------------------------------------

deriving newtype instance
  (JSM' :> es, IOE :> es) => JSaddleTypes.MonadJSM (WidgetEff t x es)

-- ---------------------------------------------------------------------
-- NotReady — trivial delegation.
-- ---------------------------------------------------------------------
instance
  RNR.NotReady t (WithJSContextSingleton x (Eff es))
  => RNR.NotReady t (WidgetEff t x es) where
  notReadyUntil ev = WidgetEff (RNR.notReadyUntil ev)
  notReady         = WidgetEff RNR.notReady

-- ---------------------------------------------------------------------
-- Adjustable — 'Event t (m b)' arg types force us to hoist the
-- 'WidgetEff' newtype in the event via 'fmap unWidgetEff'.
-- ---------------------------------------------------------------------
-- Adjustable for 'WidgetEff t x es'. 'runWithReplace' dispatches through
-- the NEW 'AdjustableInRequesterT' effect (via 'runWithReplaceRequesterTE')
-- so that per-cohort fresh-Requester scoping engages
-- ('runAdjustableInRequesterT' interpreter installs 'interpose'-based
-- fresh Requester handlers per replacement widget). This is the wire
-- that closes the 24 failing hydration browser tests. Traverse variants
-- still delegate through the underlying Adjustable instance for now
-- (see runner docstring at eff-reflex/HeadlessEffectful.hs for
-- follow-up on proper traverse-in-Requester-scope implementation).
instance
  ( RAdj.Adjustable t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  , t ~ R.SI.SpiderTimeline x
  , R.SI.HasSpiderTimeline x
  , WidgetE t (DomBuilderSpaceE es) es
  , RH.ReflexHost t
  , MonadFix (Eff es)
  ) => RAdj.Adjustable t (WidgetEff t x es) where
  runWithReplace a0 a' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    let unwrapChild :: WidgetEff t x es b -> Eff es b
        unwrapChild w = runWithJSContextSingleton (unWidgetEff w) jsSing
    -- Delegate to 'runWithReplaceEff' — the port's fully-hydration-aware
    -- runWithReplace implementation. Under upstream reflex-dom-core, the
    -- MTL Adjustable(HydrationDomBuilderT) instance carries hydration
    -- cohort setup (skipToReplaceStart/End, per-child fresh delayed IORef,
    -- commit ordering, walker step). We keep the same design here rather
    -- than moving into a 'withAdjustableHydration' interpose, because:
    --   1. HydrationEnv fields ('parent', 'commitAction') are plain values
    --      in upstream — using IORef wrapping to survive Effectful's
    --      SeqForkUnlift fork boundary would be an unnecessary divergence.
    --   2. Effectful 'Reader.Static.local' modifications do NOT survive
    --      the fork boundary an interpose's 'localUnliftIO' introduces.
    --   3. The MTL-dispatch path has no fork between the localEnv swap
    --      and the child's execution, so upstream's Reader-based design
    --      works correctly here.
    -- Requester + Query interposers are safe because their per-cohort
    -- state is IORef-based (matching upstream Requester's RequesterState),
    -- and IORef survives fork by reference.
    in runWithReplaceEff @t @(DomBuilderSpaceE es)
         (unwrapChild a0)
         (unwrapChild <$> a')
  -- Per-element PostBuild interpose: each traverse element gets its own
  -- fresh postBuild event (fired after the element widget is built).
  -- Mirrors upstream reflex-dom-core's HydrationDomBuilderT which gives
  -- each per-child scope its own PostBuild via 'PostBuildT'. Without this,
  -- 'getPostBuild' inside a newly-added element would return the
  -- long-since-fired app-level pb, breaking widgets like
  --   listWithKey $ \\k v -> do pb <- getPostBuild; performEvent_ (act <$ pb)
  --
  -- Implementation: wrap each 'f k v' with an 'interpose' on
  -- 'HH.PostBuild t' that returns a fresh trigger event; fire the
  -- trigger via the events chan after the element widget completes.
  -- Traverse methods route through the hoistTraverse* wrappers, which
  -- own the DMap/IntMap DOM lifecycle (placeholder management, applyDomUpdate
  -- for insert/delete/move, child-readiness tracking). Without this
  -- routing the Adjustable interpreter runs the FRP layer correctly but
  -- no DOM operations happen — that's why 8 DMap-family tests were
  -- failing pre-wire-up. Each traverse method also wraps the user's
  -- 'f k v' with 'interpose'+'firePb' to give the element its own
  -- per-invocation PostBuild event (mirrors upstream reflex-dom-core's
  -- HydrationDomBuilderT.PostBuildT-per-scope semantics).
  -- Per-element PostBuild interpose: each traverse element gets its own
  -- fresh postBuild event (fired after the element widget is built).
  -- Mirrors upstream reflex-dom-core's HydrationDomBuilderT which gives
  -- each per-child scope its own PostBuild via 'PostBuildT'. Traverse
  -- methods route through the 'hoistTraverse*' wrappers, which own the
  -- DMap/IntMap DOM lifecycle (placeholder management, applyDomUpdate for
  -- insert/delete/move, child-readiness tracking).
  traverseIntMapWithKeyWithAdjust f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    let fWithPb k v = do
          (freshPb, firePb) <- HH.newTriggerEventE @t
          result <- interpose @(HH.PostBuild t)
                      (\_ HH.GetPostBuild -> pure freshPb)
                      (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
          liftIO (firePb ())
          pure result
    in hoistTraverseIntMapWithKeyWithAdjust @t
         (\ff im0 im' -> send (HH.TraverseIntMapWithKeyWithAdjustOp ff im0 im'))
         updateIntMapChildUnreadiness
         applyDomUpdateIntMap
         fWithPb
         dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    -- Inline the per-element PostBuild interpose lambda directly (not
    -- let-bound) so its polymorphism over the existentially-quantified
    -- 'a' is preserved at the call site.
    hoistTraverseWithKeyWithAdjust @t
      (\ff dm0' dm'' -> send (HH.TraverseDMapWithKeyWithAdjustOp ff dm0' dm''))
      mapPatchDMap
      updateDMapChildUnreadiness
      applyDomUpdateDMap
      (\k v -> do
        (freshPb, firePb) <- HH.newTriggerEventE @t
        result <- interpose @(HH.PostBuild t)
                    (\_ HH.GetPostBuild -> pure freshPb)
                    (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
        liftIO (firePb ())
        pure result)
      dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = WidgetEff $ WithJSContextSingleton $ ReaderT $ \jsSing ->
    hoistTraverseWithKeyWithAdjust @t
      (\ff dm0' dm'' -> send (HH.TraverseDMapWithKeyWithAdjustWithMoveOp ff dm0' dm''))
      mapPatchDMapWithMove
      updateDMapWithMoveChildUnreadiness
      applyDomUpdateDMapWithMove
      (\k v -> do
        (freshPb, firePb) <- HH.newTriggerEventE @t
        result <- interpose @(HH.PostBuild t)
                    (\_ HH.GetPostBuild -> pure freshPb)
                    (runWithJSContextSingleton (unWidgetEff (f k v)) jsSing)
        liftIO (firePb ())
        pure result)
      dm0 dm'

-- ---------------------------------------------------------------------
-- DomBuilder — pins 'DomBuilderSpace' to the underlying, delegates
-- every method by unwrapping the widget body and re-wrapping the
-- result.
-- ---------------------------------------------------------------------
instance
  ( RDBC.DomBuilder t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  , DomSpace (DomBuilderSpaceE es)
  , RNR.NotReady t (WidgetEff t x es)
  , RAdj.Adjustable t (WidgetEff t x es)
  , RDBC.DomBuilderSpace (WithJSContextSingleton x (Eff es)) ~ DomBuilderSpaceE es
  ) => RDBC.DomBuilder t (WidgetEff t x es) where
  type DomBuilderSpace (WidgetEff t x es) = DomBuilderSpaceE es
  textNode        cfg          = WidgetEff (RDBC.textNode cfg)
  commentNode     cfg          = WidgetEff (RDBC.commentNode cfg)
  element         tag cfg body = WidgetEff (RDBC.element tag cfg (unWidgetEff body))
  inputElement    cfg          = WidgetEff (RDBC.inputElement cfg)
  textAreaElement cfg          = WidgetEff (RDBC.textAreaElement cfg)
  selectElement   cfg body     = WidgetEff (RDBC.selectElement cfg (unWidgetEff body))
  placeRawElement raw          = WidgetEff (RDBC.placeRawElement raw)
  wrapRawElement  raw cfg      = WidgetEff (RDBC.wrapRawElement raw cfg)

-- ---------------------------------------------------------------------
-- HasDocument — dispatch directly through the port's HasDocumentE
-- effect via 'send'. Bypasses 'WithJSContextSingleton', which
-- reflex-dom-core does not ship a lifting instance for.
-- ---------------------------------------------------------------------
instance
  ( HasDocumentE (DomBuilderSpaceE es) :> es
  , RDBC.DomBuilder t (WidgetEff t x es)
  , DomBuilderSpaceE es ~ RDBC.DomBuilderSpace (WidgetEff t x es)
  ) => RDBC.HasDocument (WidgetEff t x es) where
  askDocument = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(HasDocumentE (DomBuilderSpaceE es)) AskDocument)))

-- ---------------------------------------------------------------------
-- DomRenderHook — bridge directly to the port's @DomRenderHook t (Eff es)@
-- instance (line 1372) rather than routing through
-- @WithJSContextSingleton x (Eff es)@. Reflex-dom-core does not ship
-- a ReaderT-lifting instance for 'DomRenderHook', so an
-- intermediate WithJSContextSingleton hop cannot be satisfied.
-- ---------------------------------------------------------------------
instance
  ( R.Reflex t
  , t ~ DomBuilderTimelineE es
  , DomRenderHookE t :> es
  ) => RDBC.DomRenderHook t (WidgetEff t x es) where
  -- Dispatch directly through the effect layer via 'send' —
  -- matches the 'DomRenderHook t (Eff es)' instance body above and
  -- bypasses the DomRenderHookTimelineE fundep gymnastics.
  withRenderHook hook body = WidgetEff (WithJSContextSingleton (ReaderT (\ctx ->
    send @(DomRenderHookE t) (WithRenderHook hook (runWithJSContextSingleton (unWidgetEff body) ctx)))))
  requestDomAction  ev     = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(DomRenderHookE t) (RequestDomAction  ev))))
  requestDomAction_ ev     = WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
    send @(DomRenderHookE t) (RequestDomAction_ (P.fmap (P.>> pure ()) ev)))))

-- ---------------------------------------------------------------------
-- MountableDomBuilder — pins 'DomFragment' to the underlying.
-- ---------------------------------------------------------------------
instance
  ( RDBC.MountableDomBuilder t (WithJSContextSingleton x (Eff es))
  , RDBC.DomBuilder t (WidgetEff t x es)
  , RDBC.DomFragment (WithJSContextSingleton x (Eff es)) ~ DomFragment (DomBuilderSpaceE es)
  ) => RDBC.MountableDomBuilder t (WidgetEff t x es) where
  type DomFragment (WidgetEff t x es) = DomFragment (DomBuilderSpaceE es)
  buildDomFragment body    = WidgetEff (RDBC.buildDomFragment (unWidgetEff body))
  mountDomFragment df dfEv = WidgetEff (RDBC.mountDomFragment df dfEv)

-- ---------------------------------------------------------------------
-- PerformEvent — pins 'Performable (WidgetEff t x es) = SpiderHostFrame x'
-- directly. This is the upstream-verbatim shape (matches how
-- 'Performable (PerformEventT t m) = HostFrame t' works in upstream Reflex)
-- and works here because 'x' is bound on the LHS of the family instance
-- (part of the 'WidgetEff t x es' newtype), so GHC-53634 is satisfied.
--
-- This is the FRP-honest linkage: widget-registered callbacks type-check as
-- 'SpiderHostFrame x' actions, which Spider registers directly into its
-- per-frame propagation loop. DomBuilder / Reader-cfg / JSM' in a callback
-- are compile errors — hoist them to the widget body.
--
-- Timeline is pinned to 'R.SI.SpiderTimeline x' since 'WidgetEff t x es' is only
-- used for Spider-backed widget code.
-- ---------------------------------------------------------------------
instance
  ( t ~ R.SI.SpiderTimeline x
  , RPE.PerformEvent t (WithJSContextSingleton x (Eff es))
  , R.SI.HasSpiderTimeline x
  , R.Reflex t
  , RH.ReflexHost t
  , Monad (RPE.Performable (WithJSContextSingleton x (Eff es)))
  ) => RPE.PerformEvent t (WidgetEff t x es) where
  -- Delegate Performable to the underlying wrapped monad (mirrors upstream
  -- 'HydrationDomBuilderT': widget transformers propagate their inner
  -- monad's Performable; only the bottom-level 'PerformEventT t m' pins
  -- @Performable = HostFrame t@). The base MTL bridge for 'Eff es' in
  -- 'Reflex.Host.HeadlessEffectful' pins that to 'HostFrame t' via
  -- 'PerformableForRow es', which propagates up naturally through
  -- 'WithJSContextSingleton' and out to us.
  type Performable (WidgetEff t x es) = RPE.Performable (WithJSContextSingleton x (Eff es))
  performEvent  ev = WidgetEff (RPE.performEvent  ev)
  performEvent_ ev = WidgetEff (RPE.performEvent_ ev)

-- ---------------------------------------------------------------------
-- PostBuild — trivial delegation.
-- ---------------------------------------------------------------------
instance
  ( RPB.PostBuild t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => RPB.PostBuild t (WidgetEff t x es) where
  getPostBuild = WidgetEff RPB.getPostBuild

-- ---------------------------------------------------------------------
-- TriggerEvent — has an 'IO () -> IO ()' arg in one method that must be
-- coerced (safe: no 'm' involvement).
-- ---------------------------------------------------------------------
instance
  RTE.TriggerEvent t (WithJSContextSingleton x (Eff es))
  => RTE.TriggerEvent t (WidgetEff t x es) where
  newTriggerEvent               = WidgetEff RTE.newTriggerEvent
  newTriggerEventWithOnComplete = WidgetEff RTE.newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f
                                = WidgetEff (RTE.newEventWithLazyTriggerWithOnComplete f)

-- ---------------------------------------------------------------------
-- MonadSample.
-- ---------------------------------------------------------------------
instance
  ( R.MonadSample t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => R.MonadSample t (WidgetEff t x es) where
  sample b = WidgetEff (R.sample b)

-- ---------------------------------------------------------------------
-- MonadHold.
-- ---------------------------------------------------------------------
instance
  ( R.MonadHold t (WithJSContextSingleton x (Eff es))
  , R.MonadSample t (WidgetEff t x es)
  , R.Reflex t
  ) => R.MonadHold t (WidgetEff t x es) where
  hold a0 ev             = WidgetEff (R.hold a0 ev)
  holdDyn a0 ev          = WidgetEff (R.holdDyn a0 ev)
  holdIncremental p0 ev  = WidgetEff (R.holdIncremental p0 ev)
  buildDynamic pm ev     = WidgetEff (R.buildDynamic pm ev)
  headE ev               = WidgetEff (R.headE ev)
  now                    = WidgetEff R.now

-- ---------------------------------------------------------------------
-- MonadReflexCreateTrigger.
-- ---------------------------------------------------------------------
instance
  ( RH.MonadReflexCreateTrigger t (WithJSContextSingleton x (Eff es))
  , R.Reflex t
  ) => RH.MonadReflexCreateTrigger t (WidgetEff t x es) where
  newEventWithTrigger f    = WidgetEff (RH.newEventWithTrigger f)
  newFanEventWithTrigger f = WidgetEff (RH.newFanEventWithTrigger f)

-- ---------------------------------------------------------------------
-- MonadRef — Ref (WidgetEff t x es) = IORef, delegated to the underlying
-- Eff / WithJSContextSingleton pair. Required by upstream's
-- 'PrerenderBaseConstraints' and by any widget helper carrying
-- @MonadRef m@ / @Ref m ~ IORef@ constraints (e.g. the 'TestWidget'
-- bundle used by upstream reflex-dom-core's hydration test suite).
-- ---------------------------------------------------------------------
instance IOE :> es => MRef.MonadRef (WidgetEff t x es) where
  type Ref (WidgetEff t x es) = IORef
  newRef       = WidgetEff . MRef.newRef
  readRef      = WidgetEff . MRef.readRef
  writeRef ref = WidgetEff . MRef.writeRef ref
  {-# INLINABLE newRef   #-}
  {-# INLINABLE readRef  #-}
  {-# INLINABLE writeRef #-}

-- ---------------------------------------------------------------------
-- EventWriter, DynamicWriter and BehaviorWriter instances are not supplied
-- for WidgetEff. Their functional dependencies require the monad type to
-- determine the output type as well as the timeline.
--
-- Prerender — mirrors the RDPR.Prerender bridge instance for
-- WithJSContextSingleton x (Eff serverEs) further down in this file,
-- but for WidgetEff t x es. Client type is a WidgetEff of the client row.
-- ---------------------------------------------------------------------
instance
  ( t ~ DomBuilderTimelineE es
  , DomBuilderTimelineE (ClientEs t (DomBuilderSpaceE es) es) ~ t
  , HasPrerender t (DomBuilderSpaceE es) es
  , PrerenderClientConstraintE t (ClientEs t (DomBuilderSpaceE es) es)
  , RDPR.PrerenderClientConstraint t
      (WidgetEff t x (ClientEs t (DomBuilderSpaceE es) es))
  ) => RDPR.Prerender t (WidgetEff t x es) where
  type Client (WidgetEff t x es) =
    WidgetEff t x (ClientEs (DomBuilderTimelineE es)
                            (DomBuilderSpaceE es)
                            es)
  prerender server client = WidgetEff (WithJSContextSingleton (ReaderT (\ctx ->
    let serverEff = runWidgetEff server ctx
        clientEff = runWidgetEff client ctx
    in prerender @(DomBuilderTimelineE es) @(DomBuilderSpaceE es) serverEff clientEff)))

-- | Widget type used by 'mainWidget' + 'attachWidgetImmediate'.
-- Timeline is 'DomTimeline' — the single-source-of-truth Reflex
-- timeline alias for widgets (mirrors upstream MTL reflex-dom's
-- 'Reflex.Dom.Main.DomTimeline'). Parameterized on the JSaddle-context
-- tag @x@ and effect row @es@ — callers that want a polymorphic widget
-- wrap the argument in @forall x es. …@ at the function signature.
type ImmediateWidget (x :: Type) (es :: [Effect]) =
  ImmediateWidgetE DomTimeline es
  => WidgetEff DomTimeline x es ()

-- | Run a 'WidgetEff' against a supplied 'JSContextSingleton'.
runWidgetEff :: WidgetEff t x es a -> JSContextSingleton x -> Eff es a
runWidgetEff = runWithJSContextSingleton . unWidgetEff
{-# INLINE runWidgetEff #-}

-- | Lift an @Eff es@ action into 'WidgetEff' by discarding the JSaddle
-- singleton reader context. Analog of upstream MTL reflex-dom's repeated
-- 'lift' through the widget transformer stack (see
-- 'Obelisk.Frontend.runFrontendWithConfigsAndCurrentRoute' for the same
-- pattern applied to 'HydrationDomBuilderT'). Use this to thread raw-Eff
-- helpers (config readers, cookie access, route setup) through composers
-- that expect a widget-shaped body.
liftEff :: Eff es a -> WidgetEff t x es a
liftEff = WidgetEff . WithJSContextSingleton . lift
{-# INLINE liftEff #-}

-- =====================================================================
-- Documentation-only: effect row equivalent to variant B's
-- 'HydrationWidget x a' (from @Reflex.Dom.Main@).
--
-- Variant B's HydrationWidget expands (transformer stack, outermost
-- first):
--
--   HydrationDomBuilderT HydrationDomSpace DomTimeline
--     (PostBuildT DomTimeline
--       (WithJSContextSingleton x
--         (PerformEventT DomTimeline
--           DomHost)))
--   where DomHost = SpiderHost Global
--
-- The Eff-native equivalent: wrap 'Eff (HydrationWidgetRow t)' in
-- 'WithJSContextSingleton x' (same as we do for ImmediateWidget).
-- Each transformer layer maps to one or more effects:
--
--   HydrationDomBuilderT HydrationDomSpace  ↦  DomBuilderE t HydrationDomSpace
--                                            + HasDocumentE HydrationDomSpace
--                                            + DomRenderHookE t
--                                            + Reader (HydrationEnv t)
--                                            + State HydrationState
--   PostBuildT DomTimeline                  ↦  HH.PostBuild t
--   TriggerEventT (used internally by HDBT) ↦  HH.TriggerEvent t
--   Adjustable-via-runWithReplace machinery ↦  HH.Adjustable t
--   PerformEventT DomTimeline               ↦  HH.PerformEventEff '[IOE] t
--   DomHost = SpiderHost Global             ↦  HH.BaseEffectfulHostRow (SpiderTag t)
--                                              (NotReady + Sample + Hold +
--                                               SubscribeEvent + ReadEvent +
--                                               CreateTrigger + HostFrame' +
--                                               Timeline + IOE)
--   Ambient JSaddle context                 ↦  JSM'
--
-- Kept as a type synonym for reference; the concrete widget monad in
-- this module is @WithJSContextSingleton x (Eff (HydrationWidgetRow t))@.
type HydrationWidgetRow t x =
  '[ -- HydrationDomBuilderT HydrationDomSpace layer:
     DomBuilderE t HydrationDomSpace
   , HasDocumentE HydrationDomSpace
   , DomRenderHookE t
   , Reader (HydrationEnv t)
   , State HydrationState

     -- PostBuildT / TriggerEventT / Adjustable layers:
   , HH.PostBuild t
   , HH.TriggerEvent t
   , HH.Adjustable t

     -- PerformEventT layer:
   , HH.PerformEventEff '[IOE] (R.SI.SpiderTimeline x)

     -- Requester layer (Task #122 fix: runRequester at outermost of
     -- hostPerformEventT's composition, exposed here for
     -- AdjustableInRequesterT to send batched requests):
   , HH.Requester t (R.SI.SpiderHostFrame x) Identity

     -- JSaddle-context effect (ours only; variant B threads ctx via
     -- the WithJSContextSingleton Reader that wraps this row):
   , JSM'
   ] `Append` HH.BaseEffectfulHostRow x

-- Type-level list append. Kept private to this documentation type.
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- | Documentation-only alias — direct analog of variant B's
-- @HydrationWidget x a@, expressed in our Eff-native shape. Named
-- @HydrationWidgetSpec@ to avoid clashing with the existing widget
-- constraint synonym @HydrationWidget@ used in the Tier-3 hydration
-- bootstrap composers further down in this file.
type HydrationWidgetSpec x a =
  WithJSContextSingleton x (Eff (HydrationWidgetRow (R.SI.SpiderTimeline x) x)) a

-- | Effect row for "floating" (parent-less) widget scope — everything
-- an 'ImmediateWidget' needs EXCEPT the per-subwidget DOM interpreters
-- ('DomBuilderE', 'HasDocumentE', 'Reader (HydrationEnv t)'). Used by
-- 'runImmediateWidgetWithHeadAndBody' where the app callback runs at
-- floating scope and dispatches per-subwidget content through appenders
-- which install the DOM interpreters around each guest.
--
-- Direct analog of upstream's @FloatingWidget x = TriggerEventT
-- DomTimeline (DomCoreWidget x)@.
type FloatingWidgetE es =
  ( MonadFix (Eff es)
  , HH.PostBuild (DomTimeline) :> es
  , HH.PerformEventM (RH.HostFrame (DomTimeline)) (DomTimeline) :> es
  , HH.PerformEventH (DomTimeline) :> es
  , HH.TriggerEvent (DomTimeline) :> es
  , HH.CreateTrigger (DomTimeline) :> es
  , HH.NotReady (DomTimeline) :> es
  , HH.Adjustable (DomTimeline) :> es
    -- Task #122 fix: expose AdjustableInRequesterT + Requester for
    -- per-cohort fresh Requester scoping in prerender_ client widgets.
  , HH.Requester (DomTimeline)
                 (R.SI.SpiderHostFrame R.SI.Global)
                 Identity :> es
  , HH.Hold (DomTimeline) :> es
  , HH.Sample (DomTimeline) :> es
  , JSM' :> es
  , IOE :> es
  , DomRenderHookE (DomTimeline) :> es
  , HH.PerformableForRow es ~ RH.HostFrame DomTimeline
  )

-- =====================================================================
-- Upstream-mirroring analogs for reflex-dom-core's Main.hs runtime.
--
-- Names + shapes chosen to match upstream (Reflex.Dom.Main) as closely
-- as possible so any reflex-dom reader recognises the structure:
--
--   Upstream                                Ours (this section)
--   ─────────────────────────────           ───────────────────
--   EventChannel                            EventChannel
--   FireCommand DomTimeline DomHost         FireCommand
--   newEventWithTriggerRef                  newEventWithTriggerRef
--   hostPerformEventT                       hostPerformEventT
--   attachImmediateWidget                   attachImmediateWidget
--   attachWidget'                           attachWidget'
--   attachWidget                            attachWidget
--   processAsyncEvents                      processAsyncEvents
--   replaceElementContents                  replaceElementContents
-- =====================================================================

-- | Shared trigger-events chan type — analog of upstream's
-- @EventChannel = Chan [DSum (EventTriggerRef DomTimeline) TriggerInvocation]@.
type EventChannel =
  Chan [DSum (HH.EventTriggerRef (DomTimeline)) HH.TriggerInvocation]

-- | Fire-command handle. Analog of upstream's
-- @FireCommand DomTimeline DomHost = FireCommand ([DSum EventTrigger Identity] -> ReadPhase h a -> h [a])@.
--
-- API divergence from upstream: upstream's @runFireCommand@ takes a
-- rank-N @ReadPhase h a@ callback that runs inside the frame after
-- firing (used by testing/dev-tools that need to sample post-fire
-- values). In this port, the fire operation returns 'IO ()' — no
-- inline post-fire read. Callers that need to sample state after a
-- fire have two upstream-parity alternatives: (a) subscribe to the
-- event and let the subscriber read via 'HH.readSpiderEventHandle' out
-- of band, or (b) use 'HH.subscribeEventE' + 'HH.fireAndDrain' from
-- 'Reflex.Host.HeadlessEffectful' directly (that's what
-- 'hostPerformEventT' builds on top of).
newtype FireCommand = FireCommand
  { runFireCommand :: [DSum (R.SI.RootTrigger R.SI.Global) Identity] -> IO () }

-- | Analog of upstream reflex's
-- @Reflex.Host.Class.newEventWithTriggerRef :: m (Event t a, Ref m (Maybe (EventTrigger t a)))@.
-- Specialised to 'IO' + 'Global' timeline. Allocates a fresh spider
-- event + an IORef that will hold its trigger once the event acquires
-- a subscriber.
newEventWithTriggerRef
  :: forall a
   . IO ( R.Event (DomTimeline) a
        , IORef (Maybe (R.SI.RootTrigger R.SI.Global a)))
newEventWithTriggerRef = do
  rt <- newIORef Nothing
  e <- R.SI.newEventWithTriggerIO @R.SI.Global $ \t -> do
    writeIORef rt (Just t)
    pure (writeIORef rt Nothing)
  pure (R.SI.SpiderEvent e, rt)


-- | Route IO callbacks through the shared Requester and interpret
-- PerformEvent, Adjustable and Requester together. The fused handler keeps
-- replacement cohorts connected to their own response selectors.
hostPerformEventT
  :: forall a
   . Eff ( HH.PerformEventEff '[IOE] (DomTimeline)
         ': HH.Adjustable (DomTimeline)
         ': HH.PerformEventM
             (R.SI.SpiderHostFrame R.SI.Global)
             (DomTimeline)
         ': HH.Requester
             (DomTimeline)
             (R.SI.SpiderHostFrame R.SI.Global)
             Identity
         ': HH.BaseEffectfulHostRow R.SI.Global
         )
        a
  -> HH.EffectfulHost R.SI.Global (a, FireCommand)
hostPerformEventT act = HH.EffectfulHost $ do
  (responseEv, respTrigRef) <- liftIO newEventWithTriggerRef
  (result, aggActions) <-
      HH.runPerformEventAndAdjustableM @R.SI.Global responseEv
    . HH.interpretPerformEventEffAsRequester @'[IOE] @(DomTimeline)
    $ act
  actionsHandle  <- HH.subscribeEventE @(DomTimeline) aggActions
  shutdownHandle <- HH.subscribeEventE @(DomTimeline)
                      (R.never :: R.Event (DomTimeline) ())
  let fc = FireCommand $ \fires ->
        void $ HH.fireAndDrain @R.SI.Global
                 fires actionsHandle shutdownHandle respTrigRef
  pure (result, fc)

-- | Build an immediate widget, fire postBuild, and start its event loop.
attachImmediateWidget
  :: forall a
   . ( IORef HydrationMode
      -> EventChannel
      -> Eff ( HH.PerformEventEff '[IOE] (DomTimeline)
             ': HH.Adjustable (DomTimeline)
             ': HH.PerformEventM
                 (R.SI.SpiderHostFrame R.SI.Global)
                 (DomTimeline)
             ': HH.Requester
                 (DomTimeline)
                 (R.SI.SpiderHostFrame R.SI.Global)
                 Identity
             ': HH.BaseEffectfulHostRow R.SI.Global
             )
            (a, IORef (Maybe (R.SI.RootTrigger R.SI.Global ())))
     )
  -> IO (a, FireCommand)
attachImmediateWidget w = do
  hydrationMode <- newIORef HydrationMode_Immediate
  events        <- newChan
  HH.runEffectfulHost $ do
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <-
      hostPerformEventT (w hydrationMode events)
    mPostBuildTrigger <- liftIO (readIORef postBuildTriggerRef)
    liftIO $ forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()]
    -- Option 3: fork drain INSIDE runEffectfulHost's scope. Callers must
    -- NOT fork processAsyncEvents again outside. Since Spider's timeline
    -- is a static global (runTimeline is a no-op interpreter — see
    -- HeadlessEffectful.hs:1721), the fork thread inherits it via
    -- HasSpiderTimeline. Callbacks that rebuild widgets (runWithReplace
    -- replacements) still need effectful handlers alive; those handlers
    -- live in this runEffectfulHost scope. Whether that scope survives
    -- across attachImmediateWidget's return is the open question — this
    -- is the minimal fix; escalate to continuation-passing if not enough.
    _ <- liftIO $ forkIO $ processAsyncEvents events fc
    pure (result, fc)

-- | Parallel to 'attachImmediateWidget' using the AdjustableE
-- machinery. Uses 'hostPerformEventTAE' + 'runAdjustableEStandalone'.
-- Does NOT modify 'attachImmediateWidget' above.
--
-- Widget row expected: 'AdjustableE (GuestEff t x) t : BaseEffectfulHostRow x'.
-- 'hostPerformEventTAE' installs the AdjustableE handler + internal
-- Requester + per-cohort sub-effects (PerformEvent, PostBuild,
-- TriggerEvent).
--
-- Top-level postBuild is created here (owned by composer, not widget),
-- passed to 'hostPerformEventTAE'. After runEffectfulHost setup, we
-- read the pb triggerRef (populated when someone subscribes to pb
-- inside a cohort) and fire it.

-- | Direct analog of upstream reflex-dom-core's @processAsyncEvents@
-- (Reflex.Dom.Main:334). Consumes 'TriggerInvocation's off the shared
-- 'EventChannel' and dispatches them via the 'FireCommand'. Loops
-- forever; typically forked with 'forkIO'.
processAsyncEvents :: EventChannel -> FireCommand -> IO ()
processAsyncEvents events (FireCommand fire) = forever $ do
  ers <- readChan events
  mes <- forM ers $ \(HH.EventTriggerRef er :=> HH.TriggerInvocation a _) -> do
    me <- readIORef er
    pure $ fmap (\t -> t :=> Identity a) me
  fire (catMaybes mes)
  for_ ers $ \(_ :=> HH.TriggerInvocation _ cb) -> cb

-- | Analog of upstream's @attachWidget'@ (Reflex.Dom.Main:230).
-- Runs @w@ under a fresh DocumentFragment attached to @rootElement@,
-- forks the async-events drain, and returns @(a, FireCommand)@.
attachWidget'
  :: forall e x.
     DOM.IsElement e
  => e
  -> JSContextSingleton x
  -> (forall es. ImmediateWidget x es)
  -> JSM ((), FireCommand)
attachWidget' rootElement jsSing w = do
  ctx <- JSaddle.askJSM
  doc <- Node.getOwnerDocumentUnchecked rootElement
  df  <- Document.createDocumentFragment doc
  ((a, events), fc) <- liftIO . attachImmediateWidget $ \hydrationMode events -> do
    (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
    unreadyChildren <- liftIO $ newIORef 0
    delayed <- liftIO $ newIORef
                 (RunnerAction (pure ()) :: DelayedHydration (DomTimeline))
    let hEnv = HydrationEnv
          { hydrationEnv_document        = toDocument doc
          , hydrationEnv_parent          = Left (toNode df)
          , hydrationEnv_unreadyChildren = unreadyChildren
          , hydrationEnv_commitAction    = pure ()
          , hydrationEnv_hydrationMode   = hydrationMode
          , hydrationEnv_switchover      = R.never
          , hydrationEnv_delayed         = delayed
          , hydrationEnv_events          = events
          }
    a <-   HH.runTriggerEvent @R.SI.Global events
         . HH.runPostBuild postBuild
         . runReader hEnv
         . runJSM' ctx
         . runDomRenderHook @R.SI.Global
         . runHasDocumentGhcjs @(DomTimeline)
         . runDomBuilderImmediate @(DomTimeline)
              . HH.runPerformEventH @(DomTimeline)
         $ runWidgetEff w jsSing
    pure ((a, events), postBuildTriggerRef)
  liftIO $ runJSM (replaceElementContents rootElement df) ctx
  -- Option 3: attachImmediateWidget already forks processAsyncEvents INSIDE
  -- its runEffectfulHost scope. Do NOT fork again here — that was the pre-
  -- Option-3 bug: fork happened OUTSIDE runtime, capturing a stale env.
  pure (a, fc)

-- | Analog of upstream's @attachWidget@ (Reflex.Dom.Main:225).
-- Wraps 'attachWidget'' dropping the 'FireCommand'.
attachWidget
  :: forall e x.
     DOM.IsElement e
  => e
  -> JSContextSingleton x
  -> (forall es. ImmediateWidget x es)
  -> JSM ()
attachWidget rootElement jsSing w = fst <$> attachWidget' rootElement jsSing w

-- | Analog of upstream reflex-dom-core's @replaceElementContents@:
-- clear the root element then append the built fragment.
replaceElementContents :: DOM.IsElement e => e -> DOM.DocumentFragment -> JSM ()
replaceElementContents e df = do
  Element.setInnerHTML e ("" :: String)
  Node.appendChild_ e df

-- | Analog of upstream reflex-dom-core's @mainWidget@ (Reflex.Dom.Main:97).
-- Looks up the document body, calls 'attachWidget'.
mainWidget :: (forall x es. ImmediateWidget x es) -> IO ()
mainWidget w = withDomHost $
  withJSContextSingleton $ \jsSing -> do
    doc  <- GHCJS.DOM.currentDocumentUnchecked
    body <- getBodyUnchecked doc
    attachWidget body jsSing w

-- | Recursive-app bootstrap. Mirrors upstream 'runApp''
-- (Reflex.Dom.Main:360-370).
--
-- Simplification vs upstream: upstream wraps the widget construction in
-- 'rec o <- attachWidget body jsSing $ do ... app (AppInput{...})' so the
-- app's returned 'AppOutput' can feed back into 'wrapWindow' via
-- '_appOutput_windowConfig o'. Today upstream 'WindowConfig' is
-- 'data WindowConfig t = WindowConfig -- No config options yet'
-- (Immediate.hs:2743) — no observable fields — so the recursion resolves
-- trivially with 'WindowConfig'/'def'. When 'WindowConfig' grows fields,
-- this must be rewritten with 'MonadFix (JSM)' (JSaddle 'JSM' has
-- 'MonadFix' via 'ReaderT IO') and a generalized 'attachWidget' that
-- returns the widget's 'AppOutput' rather than discarding it.
--
-- The widget returns 'AppOutput t' but we discard it because the current
-- 'AppOutput' newtype only holds a fields-less 'WindowConfig'. Widget
-- side effects (postBuild firings, event registrations) still fire — only
-- the returned value is dropped.
runApp'
  :: (forall x es. AppInput (DomTimeline)
                -> WidgetEff DomTimeline x es
                     (AppOutput (DomTimeline)))
  -> IO ()
runApp' app = withDomHost $
  withJSContextSingleton $ \jsSing -> do
    doc  <- GHCJS.DOM.currentDocumentUnchecked
    body <- getBodyUnchecked doc
    win  <- getDefaultViewUnchecked doc
    attachWidget body jsSing $ do
      -- Lift 'wrapWindow' (Eff es) into 'WidgetEff' via the newtype +
      -- ReaderT unwrap pattern used by the other MTL bridge instances
      -- (e.g. HasDocument.askDocument at line 4856).
      w <- WidgetEff (WithJSContextSingleton (ReaderT (\_ ->
             wrapWindow win WindowConfig)))
      _ <- app AppInput { _appInput_window = w }
      pure ()

-- =====================================================================
-- Upstream-mirroring variants (Tier 0 rename + re-exports, Tier 1 +
-- Tier 2 additions).
-- =====================================================================

-- 'runDomHost' uses the same hosting shape as 'HH.runEffectfulHost'.

-- ---------------------------------------------------------------------
-- Tier 1: trivial immediate-mode variants
-- ---------------------------------------------------------------------

-- | Analog of upstream's @attachWidget''@ (Reflex.Dom.Main:270).
-- Same shape as 'attachWidget'' but returns the JSM ambient action
-- (upstream returns @JSM (a, IORef HydrationMode, EventChannel,
-- FireCommand)@; this port returns @(a, FireCommand)@ — the
-- HydrationMode ref is bundled inside 'HydrationEnv' rather than
-- exposed positionally, and the 'EventChannel' is captured by the
-- widget scope's ambient interpreters).
attachWidget''
  :: forall e x.
     DOM.IsElement e
  => e
  -> JSContextSingleton x
  -> (forall es. ImmediateWidget x es)
  -> JSM ((), FireCommand)
attachWidget'' = attachWidget'

-- | Analog of upstream's @mainWidget'@ (Reflex.Dom.Main:112).
-- Same as 'mainWidget' but returns the 'FireCommand' so callers can
-- fire external triggers into the widget's FRP network.
mainWidget' :: (forall x es. ImmediateWidget x es) -> IO ((), FireCommand)
mainWidget' w = do
  mvar <- newEmptyMVar
  withDomHost $
    withJSContextSingleton $ \jsSing -> do
      doc  <- GHCJS.DOM.currentDocumentUnchecked
      body <- getBodyUnchecked doc
      r <- attachWidget' body jsSing w
      liftIO $ putMVar mvar r
  takeMVar mvar

-- | Analog of upstream's @mainWidgetInElementById@ (Reflex.Dom.Main:151).
-- Mount the widget in the element with the given id.
mainWidgetInElementById :: Text -> (forall x es. ImmediateWidget x es) -> IO ()
mainWidgetInElementById eid w = withDomHost $
  withJSContextSingleton $ \jsSing -> do
    doc <- GHCJS.DOM.currentDocumentUnchecked
    root <- NonElementParentNode.getElementByIdUnchecked doc eid
    attachWidget root jsSing w

-- ---------------------------------------------------------------------

-- | Analog of upstream's @mainWidgetWithCss@ (Reflex.Dom.Main:141).
-- Prepend a @<style>@ block to @<head>@ with the given CSS bytes,
-- then mount the widget in @<body>@.
mainWidgetWithCss :: ByteString -> (forall x es. ImmediateWidget x es) -> IO ()
mainWidgetWithCss css w = withDomHost $
  withJSContextSingleton $ \jsSing -> do
    doc  <- GHCJS.DOM.currentDocumentUnchecked
    hd   <- Document.getHeadUnchecked doc
    Element.setInnerHTML hd
      ("<style>" P.<> T.unpack (decodeUtf8 css) P.<> "</style>")
    body <- getBodyUnchecked doc
    attachWidget body jsSing w

-- ---------------------------------------------------------------------
-- Tier 2: head + body pair
-- ---------------------------------------------------------------------

-- | Analog of upstream's @mainWidgetWithHead@ (Reflex.Dom.Main:121).
-- Run two widgets — one on @<head>@, one on @<body>@.
mainWidgetWithHead :: (forall x es. ImmediateWidget x es) -> (forall x es. ImmediateWidget x es) -> IO ()
mainWidgetWithHead h b = withDomHost $
  withJSContextSingleton $ \jsSing -> do
    doc  <- GHCJS.DOM.currentDocumentUnchecked
    hd   <- Document.getHeadUnchecked doc
    body <- getBodyUnchecked doc
    _ <- attachWidget hd   jsSing h
    attachWidget    body jsSing b

-- | Analog of upstream's @mainWidgetWithHead'@ (Reflex.Dom.Main:280).
-- Runs two widgets — head + body — parameterised by each other's
-- output values via 'mfix' inside a single floating-widget scope.
--
-- API divergence from upstream: upstream takes a TUPLE
-- @(a -> Widget () b, b -> Widget () a)@ (which requires
-- ImpredicativeTypes to express in our port); this uses curried args.
-- Semantically identical.
mainWidgetWithHead'
  :: forall a b.
     (forall es. ImmediateWidgetE (DomTimeline) es => a -> Eff es b)
     -- ^ head widget parameterised by the body's output
  -> (forall es. ImmediateWidgetE (DomTimeline) es => b -> Eff es a)
     -- ^ body widget parameterised by the head's output
  -> IO ()
mainWidgetWithHead' h b =
  runImmediateWidgetWithHeadAndBody $ \appendHead appendBody -> do
    rec hOut <- appendHead (h bOut)
        bOut <- appendBody (b hOut)
    pure ()

-- | Analog of upstream's @runImmediateWidgetWithHeadAndBody@
-- (Reflex.Dom.Main:245). Build head + body sub-widgets inside a SINGLE
-- floating-widget scope (single postBuild + single FireCommand), each
-- under its own DocumentFragment, then swap the fragments into the real
-- @<head>@ / @<body>@ elements.
--
-- Structure (mirrors upstream lines 251-277):
--
--   1. Look up document, @<head>@, @<body>@; allocate two
--      DocumentFragments.
--   2. Bootstrap the floating scope via 'attachImmediateWidget';
--      inside, install a per-subwidget 'HydrationEnv' pointing at the
--      corresponding fragment and run the guest widget under
--      'runDomBuilderImmediate' + 'runHasDocumentGhcjs'.
--   3. Invoke the caller's 'app' with the two appenders.
--   4. After the attach, 'replaceElementContents' each fragment into
--      the real @<head>@ / @<body>@ element.
--   5. Fork the async-events drain.
runImmediateWidgetWithHeadAndBody
  :: ( forall esF. FloatingWidgetE esF
      => (forall c. (forall esW. ImmediateWidgetE (DomTimeline) esW => Eff esW c) -> Eff esF c)
         -- ^ appendHead — runs a widget under the @<head>@ fragment.
      -> (forall c. (forall esW. ImmediateWidgetE (DomTimeline) esW => Eff esW c) -> Eff esF c)
         -- ^ appendBody — runs a widget under the @<body>@ fragment.
      -> Eff esF ()
     )
  -> IO ()
runImmediateWidgetWithHeadAndBody app = withDomHost $ do
  ctx      <- JSaddle.askJSM
  doc      <- GHCJS.DOM.currentDocumentUnchecked
  headElem <- Document.getHeadUnchecked doc
  bodyElem <- getBodyUnchecked doc
  headFrag <- Document.createDocumentFragment doc
  bodyFrag <- Document.createDocumentFragment doc
  ((), events, fc) <- liftIO $ do
    ((_, events'), fc') <- attachImmediateWidget $ \hydrationMode events -> do
      (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
      let goFragment
            :: forall c esFinner. FloatingWidgetE esFinner
            => DocumentFragment
            -> (forall esW. ImmediateWidgetE (DomTimeline) esW => Eff esW c)
            -> Eff esFinner c
          goFragment df w = do
            unreadyChildren <- liftIO $ newIORef 0
            delayed <- liftIO $ newIORef
                         (RunnerAction (pure ()) :: DelayedHydration (DomTimeline))
            let hEnv = HydrationEnv
                  { hydrationEnv_document        = doc
                  , hydrationEnv_parent          = Left (toNode df)
                  , hydrationEnv_unreadyChildren = unreadyChildren
                  , hydrationEnv_commitAction    = pure ()
                  , hydrationEnv_hydrationMode   = hydrationMode
                  , hydrationEnv_switchover      = R.never
                  , hydrationEnv_delayed         = delayed
                  , hydrationEnv_events          = events
                  }
            runReader hEnv
              . runHasDocumentGhcjs @(DomTimeline)
              . runDomBuilderImmediate @(DomTimeline)
              . HH.runPerformEventH @(DomTimeline)
              $ w
      _ <-   HH.runTriggerEvent @R.SI.Global events
           . HH.runPostBuild postBuild
           . runJSM' ctx
           . runDomRenderHook @R.SI.Global
           . HH.runPerformEventH @(DomTimeline)
           $ app (goFragment headFrag) (goFragment bodyFrag)
      pure (((), events), postBuildTriggerRef)
    pure ((), events', fc')
  liftIO $ runJSM (replaceElementContents headElem headFrag) ctx
  liftIO $ runJSM (replaceElementContents bodyElem bodyFrag) ctx
  -- Option 3: attachImmediateWidget already forks processAsyncEvents INSIDE
  -- its runEffectfulHost scope. Do NOT fork again here.
  pure ()

-- =====================================================================
-- Prerender.
--
-- We expose TWO APIs, connected by a bridge:
--
--   1. 'HasPrerender t space serverEs' — the port-native class, keyed
--      on 'space' as a discriminator. Three concrete instances:
--      GhcjsDomSpace (pure client, identity), StaticDomSpace (server-
--      only, emits markers), HydrationDomSpace (both run, mount at
--      switchover).
--
--   2. Upstream 'Reflex.Dom.Prerender.Prerender' — via a single BRIDGE
--      instance that dispatches to 'HasPrerender' via 'DomBuilderSpaceE'.
--      Any monad @'WithJSContextSingleton' x ('Eff' serverEs)@ where the
--      row's DOM space has a 'HasPrerender' instance automatically
--      satisfies upstream 'Prerender'.
--
-- Why the wrapper: 'WithJSContextSingleton' puts 'JSContextSingleton'
-- in a Reader position, which lets upstream's
-- 'MonadJSM' ('Performable' m)' constraint from 'PrerenderBaseConstraints'
-- resolve via jsaddle's @MonadIO m => MonadJSM (WithJSContextSingleton x m)@
-- (@Foreign/JavaScript/TH.hs:150@), without any 'MonadJSM' orphan on
-- eff-reflex's 'SI.SpiderHostFrame'.
-- =====================================================================

-- | @HasPrerender t space serverEs@ — render one widget on the server
-- and (a different) widget on the client. Analog of upstream
-- reflex-dom's @Prerender t m@ class, keyed on the DOM space as a
-- separate class parameter (instead of derived from the carrier monad
-- as upstream does).
--
-- 'ClientEs' is the associated type for the client widget's row —
-- upstream's @Client m :: Type -> Type@ analog.
-- Superclass chain mirrors upstream 'Reflex.Dom.Prerender.Prerender's
-- @Prerender t (Client m)@ + @PrerenderClientConstraint t (Client m)@ +
-- @Reflex t@ constraints, but expressed over the effect row instead of
-- the monad. Any @HasPrerender t space serverEs@ constraint now
-- transitively gives the caller @PrerenderClientConstraintE t (ClientEs
-- t space serverEs)@ — meaning @PerformEvent@, @TriggerEvent@, @Hold@,
-- @JSM'@, @DomBuilderE@ etc. are in scope inside the client branch
-- without the caller having to re-list them at each 'prerender' call
-- site.
class ( PrerenderClientConstraintE t (ClientEs t space serverEs)
      , R.Reflex t
      ) => HasPrerender t (space :: Type) serverEs where
  type ClientEs t space serverEs :: [Effect]
  prerender :: Eff serverEs a
            -> Eff (ClientEs t space serverEs) a
            -> Eff serverEs (R.Dynamic t a)

-- | Void variant. @prerender_ s c = void (prerender s c)@.
prerender_
  :: forall t space serverEs
   . HasPrerender t space serverEs
  => Eff serverEs ()
  -> Eff (ClientEs t space serverEs) ()
  -> Eff serverEs ()
prerender_ s c = void (prerender @t @space s c)

-- | Immediate/JSaddle client-side prerender. Direct port of upstream
-- @instance Prerender t (HydrationDomBuilderT GhcjsDomSpace t m)@
-- (reflex-dom-core/src/Reflex/Dom/Prerender.hs:97):
-- @Client m = m; prerender _ client = pure <$> client@.
--
-- Effectful shape: @ClientEs = serverEs@ (self-collapse — client
-- widget runs in the same row as the outer). Server branch discarded.
instance
  ( PrerenderClientConstraintE t serverEs
  , R.Reflex t
  ) => HasPrerender t GhcjsDomSpace serverEs where
  type ClientEs t GhcjsDomSpace serverEs = serverEs
  prerender _server client = R.constDyn <$> client

-- ---------------------------------------------------------------------
-- Constraint bundles (from variant A's Prerender.hs, adjusted to
-- eff-reflex effect parameterisation).
-- ---------------------------------------------------------------------

-- | Base capability bundle a prerender's server or client widget needs.
--
-- 'HH.PerformEventH t' is included so that the MTL bridge instance
-- @RPE.PerformEvent t (Eff es)@ resolves inside prerender
-- client branches — meaning upstream reflex-dom bodies that use
-- @performEvent_@ (e.g. 'scrollToTop' from @Obelisk.Route.Frontend@)
-- port over VERBATIM into the effectful layer with only the signature
-- swapping from MTL @(Prerender t m, ...)@ to @HasPrerender t space serverEs@.
type PrerenderBaseConstraintsE t es =
  ( R.Reflex t
  , MonadFix (Eff es)
  , HH.PostBuild t :> es
  , HH.PerformEventM (RH.HostFrame t) t :> es
    -- ↑ FRP-linkage anchor: MTL 'performEvent'/'performEvent_' inside the
    -- client branch route callbacks into Spider's per-frame propagation
    -- loop as 'HostFrame t' actions. This is the ONLY PerformEvent-family
    -- effect the bundle carries — the legacy 'PerformEventEff' /
    -- 'PerformEventH' anchors were vestigial pre-Performable-cleanup and
    -- have been removed. Widgets that need to schedule callbacks in the
    -- client branch use MTL 'performEvent' / 'performEvent_' (or the
    -- port-native 'performEventME' / 'performEventME_' pointing at
    -- 'HostFrame t' explicitly).
  , HH.TriggerEvent t :> es
  , HH.CreateTrigger t :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , JSM' :> es
  , IOE :> es
  )

-- | Full client-widget constraint bundle — real 'DomBuilderE' over
-- 'GhcjsDomSpace' with 'DomRenderHookE' + 'HasDocumentE' + base
-- capabilities.
type PrerenderClientConstraintE t es =
  ( DomBuilderE t GhcjsDomSpace :> es
  , DomRenderHookE t :> es
  , HasDocumentE GhcjsDomSpace :> es
  , PrerenderBaseConstraintsE t es
  )

-- | Canonical "unrunnable" client effect row. Analog of upstream
-- reflex-dom's @UnrunnableT t m@: a row a client widget can be TYPED
-- against but which is never actually EVALUATED, because the static
-- prerender instance discards the client branch. Every dynamic effect
-- in the row has a matching @runUnrunnable*@ trap interpreter below.
type UnrunnableEs t =
  '[ DomBuilderE t GhcjsDomSpace
   , HH.NotReady t
   , HH.Adjustable t
   , HH.PerformEventM (RH.HostFrame t) t
   , HasDocumentE GhcjsDomSpace
   , HH.TriggerEvent t
   , HH.Hold t
   , HH.Sample t
   , HH.PostBuild t
   , DomRenderHookE t
   , HH.CreateTrigger t
   , Effectful.Prim.Prim
   , JSM'
   , IOE
   ]

-- ---------------------------------------------------------------------
-- Prerender comment markers — byte-identical to upstream's so
-- client-side hydration matches by exact string comparison.
-- ---------------------------------------------------------------------

startMarker, endMarker :: Text
startMarker = "prerender/start"
endMarker   = "prerender/end"

-- | Delete the pre-rendered DOM subtree between the current runner
-- cursor's 'prerender/start' and matching 'prerender/end' markers.
-- Returns the end comment (so caller can insert client fragment
-- before it). Handles nested prerender pairs via a depth counter.
deleteToPrerenderEnd
  :: forall t es
   . (HydrationRunnerE t es, R.Reflex t)
  => Document
  -> Eff es DOM.Comment
deleteToPrerenderEnd doc = do
  startNode <- hydrateComment @t doc startMarker Nothing
  let go :: Int -> Node -> Eff es DOM.Comment
      go n lastNode = liftJSM_ (Node.getNextSibling lastNode) >>= \case
        Nothing -> do
          c <- liftJSM_ $ Document.createComment doc endMarker
          insertAfterPreviousNode c
          pure c
        Just node -> liftJSM_ (DOM.castTo DOM.Comment node) >>= \case
          Nothing -> go n node
          Just c -> liftJSM_ (Node.getTextContentUnchecked c) >>= \t ->
            if t == startMarker      then go (P.succ n) node
            else if t == endMarker
              then case n of
                0 -> pure c
                _ -> go (P.pred n) node
              else go n node
  endNode <- go 0 (toNode startNode)
  deleteBetweenExclusive startNode endNode
  setPreviousNode (Just (toNode endNode))
  pure endNode

-- ---------------------------------------------------------------------
-- Hydration-space HasPrerender instance
--
-- Direct port of upstream @instance … => Prerender t
-- (HydrationDomBuilderT HydrationDomSpace t m)@
-- (reflex-dom-core/src/Reflex/Dom/Prerender.hs:101).
--
-- Runs the server widget into a throwaway 'DocumentFragment' (never
-- mounted), runs the client widget into a second fragment, and either
-- mounts the client fragment immediately (Immediate mode) or queues a
-- hydration step that deletes the pre-rendered subtree and inserts
-- the client fragment at switchover (Hydrating mode).
-- ---------------------------------------------------------------------

instance
  ( DomBuilderE t HydrationDomSpace :> serverEs
  , DomBuilderSpaceE serverEs ~ HydrationDomSpace
  , Reader (HydrationEnv t) :> serverEs
  , RH.ReflexHost t
  -- Note: 'DomBuilderE t GhcjsDomSpace' and 'HasDocumentE GhcjsDomSpace'
  -- are NOT required in @serverEs@ — 'runClient' below layers them on
  -- top of @serverEs@ via 'runDomBuilderImmediate' + 'runHasDocumentGhcjs'
  -- for the client body only. Requiring them on @serverEs@ would falsely
  -- propagate to callers of 'HydrationWidget' (whose runtime stack only
  -- installs 'runDomBuilderHydration' at the outer scope).
  , DomRenderHookE t :> serverEs
  , HH.NotReady t :> serverEs
  , HH.Adjustable t :> serverEs
  , PrerenderBaseConstraintsE t serverEs
  ) => HasPrerender t HydrationDomSpace serverEs where
  type ClientEs t HydrationDomSpace serverEs =
       DomBuilderE t GhcjsDomSpace
     : HasDocumentE GhcjsDomSpace
     : HH.PostBuild t
     : Reader (HydrationEnv t)     -- ← Fix A: client body has its OWN
     : serverEs                    --   Reader HydrationEnv, installed via
                                   --   runReader clientEnv. Sidesteps
                                   --   effectful's Reader.local scope-
                                   --   mutation problem that breaks
                                   --   deferred client execution.
  prerender server client = do
    env <- ask @(HydrationEnv t)
    let events = hydrationEnv_events env
        doc   = hydrationEnv_document env
    serverDf <- liftJSM_ $ Document.createDocumentFragment doc
    df       <- liftJSM_ $ Document.createDocumentFragment doc
    let unreadyChildren' = hydrationEnv_unreadyChildren env
    immediateMode <- liftIO $ newIORef HydrationMode_Immediate
    delayedRef    <- liftIO $ newIORef
                       (RunnerAction (pure ()) :: DelayedHydration t)
    let clientEnv = env
          { hydrationEnv_parent          = Left (toNode df)
          , hydrationEnv_hydrationMode   = immediateMode
          }
        serverEnv = HydrationEnv
          { hydrationEnv_document        = doc
          , hydrationEnv_parent          = Left (toNode serverDf)
          , hydrationEnv_unreadyChildren = unreadyChildren'
          , hydrationEnv_commitAction    = pure ()
          , hydrationEnv_delayed         = delayedRef
          , hydrationEnv_hydrationMode   = immediateMode
          , hydrationEnv_switchover      = R.never
          , hydrationEnv_events          = events
          }
    a0 <- localEnv @t (P.const serverEnv) server
    (a', trigger) <- HH.newTriggerEventE @t
    -- Fix A: 'runReader clientEnv' binds a FRESH Reader (HydrationEnv t)
    -- for the client body — no in-place env mutation, no version bump,
    -- no scope-exit trap. The 'Reader (HydrationEnv t)' slot lives at
    -- position 4 of ClientEs (after DomBuilder/HasDoc/PostBuild), so
    -- the peel chain is:
    --   runReader clientEnv . interpretPostBuildAs . runHasDoc . runDomBuilder $ body
    -- (composition reads right-to-left; DomBuilder is peeled first as
    -- it is at the top of the row.)
    let runClient
          :: forall x' es'
           . ( WidgetE t GhcjsDomSpace es'
             , MonadFix (Eff es')
                     , RH.ReflexHost t
             )
          => Eff (DomBuilderE t GhcjsDomSpace
                : HasDocumentE GhcjsDomSpace
                : HH.PostBuild t
                : Reader (HydrationEnv t)
                : es') x'
          -> Eff es' x'
        runClient body =
            runReader clientEnv
          . interpretPostBuildAs (void a')
          . runHasDocumentGhcjs @t
          . runDomBuilderImmediate @t
          $ body
    getHydrationMode @t >>= \case
      HydrationMode_Immediate -> do
        b <- runClient client
        liftIO (trigger b)
        append @t (toNode df)
      HydrationMode_Hydrating -> do
        -- Fix A + SeqForkUnlift: the deferred client runs in a FRESH
        -- COPY of the effectful env at invocation time, so it doesn't
        -- get invalidated by env mutations the parent performs between
        -- 'withEffToIO' and the deferred invocation. 'runReader
        -- clientEnv' installs the client's HydrationEnv freshly in
        -- that copy — no cross-invocation state leakage. Subscribers
        -- (e.g. 'newTriggerEventE') register the trigger ref into the
        -- shared 'events' Chan (IO-carried, not effectful state), so
        -- 'processAsyncEvents' in the parent DOES see them.
        clientRunIO <- withEffToIO SeqForkUnlift $ \unlift ->
          pure $ do
            b <- unlift $
                   runReader clientEnv
                 . interpretPostBuildAs (void a')
                 . runHasDocumentGhcjs @t
                 . runDomBuilderImmediate @t
                 $ client
            trigger b
        addHydrationStep @t $ do
          liftIO clientRunIO
          endComment <- deleteToPrerenderEnd @t doc
          insertBefore df endComment
    HH.holdDynE @t a0 a'
    where
      interpretPostBuildAs
        :: forall es' a'
         . R.Event t ()
        -> Eff (HH.PostBuild t : es') a'
        -> Eff es' a'
      interpretPostBuildAs ev = interpret $ \_ cmd -> case cmd of
        HH.GetPostBuild -> pure ev

-- ---------------------------------------------------------------------
-- Unrunnable trap interpreters — one per effect in 'UnrunnableEs'.
-- Every constructor discharges via 'unrunnable' (⊥), matching upstream
-- @UnrunnableT@ where each MTL method's body was a @Void@ elimination.
-- ---------------------------------------------------------------------

-- | The single well-known bottom used by every unrunnable trap arm.
-- Named to match upstream so a runtime evaluation surfaces the same
-- diagnostic in either codebase.
unrunnable :: a
unrunnable = error "unrunnable"

runUnrunnableDomBuilder
  :: forall t es a
   . Eff (DomBuilderE t GhcjsDomSpace : es) a -> Eff es a
runUnrunnableDomBuilder = interpret $ \_ cmd -> case cmd of
  DB_TextNode _         -> unrunnable
  DB_CommentNode _      -> unrunnable
  DB_Element _ _ _      -> unrunnable
  DB_InputElement _     -> unrunnable
  DB_TextAreaElement _  -> unrunnable
  DB_SelectElement _ _  -> unrunnable
  DB_PlaceRawElement _  -> unrunnable
  DB_WrapRawElement _ _ -> unrunnable

runUnrunnableNotReady
  :: forall t es a
   . Eff (HH.NotReady t : es) a -> Eff es a
runUnrunnableNotReady = interpret $ \_ cmd -> case cmd of
  HH.NotReady      -> unrunnable
  HH.NotReadyUntil _ -> unrunnable

runUnrunnableAdjustable
  :: forall t es a
   . Eff (HH.Adjustable t : es) a -> Eff es a
runUnrunnableAdjustable = interpret $ \_ cmd -> case cmd of
  HH.RunWithReplace _ _                            -> unrunnable
  HH.TraverseIntMapWithKeyWithAdjustOp _ _ _       -> unrunnable
  HH.TraverseDMapWithKeyWithAdjustOp _ _ _         -> unrunnable
  HH.TraverseDMapWithKeyWithAdjustWithMoveOp _ _ _ -> unrunnable

runUnrunnablePerformEvent
  :: forall subEs t es a
   . Eff (HH.PerformEventEff subEs t : es) a -> Eff es a
runUnrunnablePerformEvent = interpret $ \_ cmd -> case cmd of
  HH.PerformEventEffE_ _ -> unrunnable
  HH.PerformEventEffE _  -> unrunnable

-- | Trap for the higher-order 'HH.PerformEventH' effect on the
-- 'UnrunnableEs' row. Symmetric with 'runUnrunnableAdjustable' which
-- also traps a higher-order effect (Adjustable's callbacks). Static
-- Prerender discards the client branch, so these constructors are
-- typed-into-scope but never evaluated at runtime.
runUnrunnablePerformEventH
  :: forall t es a
   . Eff (HH.PerformEventH t : es) a -> Eff es a
runUnrunnablePerformEventH = interpret $ \_ cmd -> case cmd of
  HH.PerformEventH_ _ -> unrunnable
  HH.PerformEventH  _ -> unrunnable

runUnrunnableHasDocument
  :: forall space es a
   . Eff (HasDocumentE space : es) a -> Eff es a
runUnrunnableHasDocument = interpret $ \_ cmd -> case cmd of
  AskDocument -> unrunnable

runUnrunnableTriggerEvent
  :: forall t es a
   . Eff (HH.TriggerEvent t : es) a -> Eff es a
runUnrunnableTriggerEvent = interpret $ \_ cmd -> case cmd of
  HH.NewTriggerEvent                         -> unrunnable
  HH.NewTriggerEventWithOnComplete           -> unrunnable
  HH.NewEventWithLazyTriggerWithOnComplete _ -> unrunnable

runUnrunnableHold
  :: forall t es a
   . Eff (HH.Hold t : es) a -> Eff es a
runUnrunnableHold = interpret $ \_ cmd -> case cmd of
  HH.Hold _ _             -> unrunnable
  HH.HoldDynE _ _         -> unrunnable
  HH.HoldIncrementalE _ _ -> unrunnable
  HH.BuildDynamicE _ _    -> unrunnable
  HH.HeadEE _             -> unrunnable
  HH.NowE                 -> unrunnable

runUnrunnableSample
  :: forall t es a
   . Eff (HH.Sample t : es) a -> Eff es a
runUnrunnableSample = interpret $ \_ cmd -> case cmd of
  HH.Sample _ -> unrunnable

runUnrunnablePostBuild
  :: forall t es a
   . Eff (HH.PostBuild t : es) a -> Eff es a
runUnrunnablePostBuild = interpret $ \_ cmd -> case cmd of
  HH.GetPostBuild -> unrunnable

runUnrunnableDomRenderHook
  :: forall t es a
   . Eff (DomRenderHookE t : es) a -> Eff es a
runUnrunnableDomRenderHook = interpret $ \_ cmd -> case cmd of
  WithRenderHook _ _  -> unrunnable
  RequestDomAction _  -> unrunnable
  RequestDomAction_ _ -> unrunnable

runUnrunnableJSM
  :: forall es a
   . Eff (JSM' : es) a -> Eff es a
runUnrunnableJSM = interpret $ \_ cmd -> case cmd of
  LiftJSM _       -> unrunnable
  WithJSContext _ -> unrunnable
  AskJSContext    -> unrunnable

-- =====================================================================
-- Tier 3 — Hydration bootstrap composers.
--
-- Analogs of upstream Reflex.Dom.Main's:
--   attachHydrationWidget / attachHydrationWidgetWithFailure
--   mainHydrationWidget
--   mainHydrationWidgetWithHead / mainHydrationWidgetWithHead'
--   mainHydrationWidgetWithSwitchoverAction / *'
--   mainHydrationWidgetWithSwitchoverActionWithFailure'
--
-- Semantic divergence from upstream: upstream shares FRP context
-- between head + body via runHydrationWidgetWithHeadAndBody's
-- FloatingWidget/HydrationWidget callback pattern (single
-- hostPerformEventT + single postBuild). This port's head+body
-- variants attach two INDEPENDENT hostPerformEventT scopes. Fine for
-- widgets that don't share FRP state across head <-> body; for
-- shared-state use, callers use attachHydrationWidgetWithFailure'
-- directly.
-- =====================================================================

-- | Effect row a 'HydrationWidget' can run in. Extracted so
-- 'runHydrationWidgetWithHeadAndBody'\'s appenders can quantify it.
-- Pins the @DomBuilderSpaceE@ + @DomBuilderTimelineE@ type families for
-- unambiguous @el@ / @text@ / @elAttr@ inference at call sites.
type HydrationWidgetE es =
  ( MonadFix (Eff es)
  , DomBuilderSpaceE es ~ HydrationDomSpace
  , DomBuilderTimelineE es ~ DomTimeline
  , WidgetE (DomTimeline) HydrationDomSpace es
  , DomBuilderE (DomTimeline) HydrationDomSpace :> es
  , HasDocumentE HydrationDomSpace :> es
    -- Extra effects required by the full 'TestWidget' constraint bundle
    -- (upstream reflex-dom-core hydration tests). Each satisfies a
    -- specific piece of TestWidget:
    --   * DomRenderHookE  → 'RDBC.DomRenderHook t m' via bridge
    --   * PostBuild       → 'PostBuild t m'
    --   * TriggerEvent    → 'TriggerEvent t m'
    --   * PerformEvent    → 'PerformEvent t m'          (Global tag)
    --   * NotReady        → 'NotReady t m'
    --   * Adjustable      → 'Adjustable t m'
    --   * IOE             → 'MonadIO m' + 'MonadRef m'
  , DomRenderHookE (DomTimeline)               :> es
  , HH.PostBuild    (DomTimeline)               :> es
  , HH.TriggerEvent (DomTimeline)               :> es
  , HH.PerformEventM (RH.HostFrame (DomTimeline)) (DomTimeline) :> es
  , HH.PerformEventH (DomTimeline) :> es
  , HH.NotReady     (DomTimeline)               :> es
  , HH.Adjustable   (DomTimeline)               :> es
    -- Task #122 fix: AdjustableInRequesterT + Requester expose the
    -- per-cohort fresh Requester scoping needed by 'runWithReplace' in
    -- 'prerender_' client widgets at hydration switchover.
  , HH.Requester (DomTimeline)
                 (R.SI.SpiderHostFrame R.SI.Global)
                 Identity                                            :> es
  , IOE                                                              :> es
    -- (Removed: 'Effectful.Prim.Prim :> es' constraint. WidgetEff's
    -- PrimMonad instance routes through IOE instead, so no runPrim
    -- interpreter is needed on the attach stack.)
    -- Pin 'PerformableForRow es' so widget bodies carrying
    -- @PerformEvent (SpiderTimeline Global) m@ have their
    -- 'Performable' associated type reduce to the concrete
    -- @SpiderHostFrame Global@. Required by upstream reflex-dom-core's
    -- 'TestWidget' bundle (which includes @MonadIO (Performable m)@).
  , HH.PerformableForRow es ~ RH.HostFrame DomTimeline
  )

-- | Widget type used by 'mainHydrationWidget' + 'attachHydrationWidget'.
-- Timeline pinned to @SpiderTimeline Global@; DOM space =
-- 'HydrationDomSpace' (adopts pre-rendered DOM, unlike 'ImmediateWidget'
-- which builds fresh under a DocumentFragment). Parameterized on the
-- JSaddle-context tag @x@ and effect row @es@ — matches 'ImmediateWidget'
-- shape.
type HydrationWidget (x :: Type) (es :: [Effect]) =
  HydrationWidgetE es => WidgetEff DomTimeline x es ()

-- | Analog of upstream reflex-dom-core's
-- @attachHydrationWidgetWithFailure@ (Reflex.Dom.Main:110-130). Attaches
-- a single 'HydrationWidget' under an existing pre-rendered element in
-- hydration mode:
--
--   1. Allocate 'IORef' seeded with 'HydrationMode_Hydrating'.
--   2. Allocate trigger-events chan.
--   3. Allocate postBuild + syncEvent triggers.
--   4. Run the widget under the hydration interpreter stack.
--   5. Schedule 'delayedAction' on syncEvent via 'runWithReplaceE'.
--   6. Fire postBuild.
--   7. Fire syncEvent (triggers the delayed hydration runner).
--   8. Fork the async-events drain loop.
--
-- The delayedAction installs the runner-scope interpreter stack around
-- 'runHydrationRunnerWithFailure' (which itself provides @State
-- HydrationState@ + @Reader Node@ via internal @runReader parent
-- (runState (HydrationState prev False) ...)@). On completion, flips
-- 'hydrationMode' to 'HydrationMode_Immediate' and fires the caller's
-- 'switchoverAction'.
attachHydrationWidgetWithFailure
  :: forall (x :: Type) e. DOM.IsElement e
  => IO ()          -- ^ 'onFailure' — fires when hydration diverges from pre-rendered DOM.
  -> JSM ()         -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> e              -- ^ Existing pre-rendered element (widget adopts + walks it).
  -> (forall es. HydrationWidget x es)
  -> JSM ((), FireCommand)
attachHydrationWidgetWithFailure onFailure switchoverAction rootElement w =
  withJSContextSingleton $ \(jsSingFresh :: JSContextSingleton xFresh) -> do
  -- Rebrand jsSing at the caller-chosen tag 'x' via Coerce
  -- (JSContextSingleton has a phantom 'x' parameter, so this is safe).
  let jsSing = unsafeCoerce jsSingFresh :: JSContextSingleton x
  ctx <- JSaddle.askJSM
  doc <- Node.getOwnerDocumentUnchecked rootElement
  liftIO $ do
    hydrationMode <- newIORef HydrationMode_Hydrating
    events        <- newChan
    (a, fc) <- HH.runEffectfulHost $ do
      ((_, postBuildTriggerRef, syncTriggerRef), fc@(FireCommand fire)) <-
        hostPerformEventT $ do
          (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
          (syncEv,   syncTriggerRef)       <- liftIO newEventWithTriggerRef
          unreadyChildren <- liftIO $ newIORef 0
          delayed <- liftIO $ newIORef
                       (RunnerAction (pure ()) :: DelayedHydration (DomTimeline))
          let hEnv = HydrationEnv
                { hydrationEnv_document        = toDocument doc
                , hydrationEnv_parent          = Left (toNode rootElement)
                , hydrationEnv_unreadyChildren = unreadyChildren
                , hydrationEnv_commitAction    = pure ()
                , hydrationEnv_hydrationMode   = hydrationMode
                , hydrationEnv_switchover      = syncEv
                , hydrationEnv_delayed         = delayed
                , hydrationEnv_events          = events
                }
          _ <-   HH.runTriggerEvent @R.SI.Global events
               . HH.runPostBuild postBuild
               . runReader hEnv
               . runJSM' ctx
               . runDomRenderHook @R.SI.Global
               . runHasDocumentHydration @(DomTimeline)
               . runDomBuilderHydration @(DomTimeline)
              . HH.runPerformEventH @(DomTimeline)
               $ runWidgetEff w jsSing
          let delayedAction = do
                runner <- liftIO (readIORef delayed)
                HH.runTriggerEvent @R.SI.Global events
                  . HH.runPostBuild
                      (R.never :: R.Event (DomTimeline) ())
                  . runReader hEnv
                  . runJSM' ctx
                  . runDomRenderHook @R.SI.Global
                  $ runHydrationRunnerWithFailure @(DomTimeline)
                      runner onFailure Nothing (toNode rootElement)
                liftIO $ writeIORef hydrationMode HydrationMode_Immediate
                liftIO $ runJSM switchoverAction ctx
          _ <- HH.runWithReplaceE @(DomTimeline)
                 (pure ()) (delayedAction <$ syncEv)
          pure ((), postBuildTriggerRef, syncTriggerRef)
      -- Fire postBuild synchronously (own frame). Any performEventE
      -- responses triggered during this frame are enqueued to 'events'
      -- via runTriggerEvent's chan-writing mechanism.
      mPBT <- liftIO (readIORef postBuildTriggerRef)
      liftIO $ forM_ mPBT $ \t -> fire [t :=> Identity ()]
      -- Fire syncEvent ASYNCHRONOUSLY via the events chan (mirroring
      -- upstream reflex-dom-core/Main.hs:122's 'fireSync ()' — a
      -- 'newTriggerEvent'-based trigger writes to the chan rather than
      -- calling FireCommand directly). This is load-bearing: the drain
      -- loop's readChan → fire sequencing gives each queued trigger its
      -- own reflex frame, so any performEventE responses from pb-frame
      -- drain into their own frames BEFORE the syncEv frame runs the
      -- hydration walker. Without this, syncEv fires in the same
      -- transaction as pb, hold Behaviors (e.g. inner runWithReplace's
      -- 'hold hydrate0 hydrate'') haven't propagated their update, and
      -- the walker samples the initial hydrate0 instead of the
      -- pb-fired replacement — the exact failure signature of
      -- 'runWithReplace can be nested in initial widget' etc.
      liftIO $ writeChan events
        [HH.EventTriggerRef syncTriggerRef :=> HH.TriggerInvocation () (pure ())]
      -- Option 3: fork drain INSIDE runEffectfulHost's scope so effectful
      -- handlers stay alive for the duration of the drain thread's use of
      -- fc. Previously the fork was AFTER runEffectfulHost returned; that
      -- was the storage-mismatch trigger (runAdjustableInRequesterT's
      -- outerEnv capture became stale as soon as the outer scope closed).
      _ <- liftIO $ forkIO $ processAsyncEvents events fc
      pure ((), fc)
    pure (a, fc)

-- | Continuation-passing analog of 'attachHydrationWidgetWithFailure'.
-- The caller-supplied continuation runs INSIDE the 'HH.runEffectfulHost'
-- scope, so effectful handlers stay installed for its entire duration.
-- This is the load-bearing shape that fixes the storage-mismatch crash
-- that hits 'runAdjustableInRequesterT' at drain time — its captured
-- 'outerEnv' remains valid as long as the outer scope is open.
--
-- Callers use it like:
--
-- @
--   withAttachHydrationWidgetWithFailure onFailure sw root widget $ \\_fc -> do
--     doAnyTestSetup
--     liftIO $ forever $ threadDelay maxBound  -- block until client disconnects
-- @
--
-- The old 'attachHydrationWidgetWithFailure' remains but is unsafe for
-- widgets that use 'runWithReplace' / 'dyn' / 'widgetHold' in hydration
-- mode — its returned 'FireCommand' captures handlers whose scope has
-- already closed, and any replacement widget-build the drain triggers
-- crashes with "Env and LocalEnv point to different Storages".
withAttachHydrationWidgetWithFailure
  :: forall (x :: Type) e b. DOM.IsElement e
  => IO ()          -- ^ 'onFailure' — fires when hydration diverges from pre-rendered DOM.
  -> JSM ()         -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> e              -- ^ Existing pre-rendered element (widget adopts + walks it).
  -> (forall es. HydrationWidget x es)
  -> (EventChannel -> FireCommand -> JSM b)
     -- ^ Continuation. Receives the event channel + fc. MUST invoke
     --   'processAsyncEvents events fc' (or an equivalent drain loop)
     --   inline — a raw 'forkIO' of processAsyncEvents WILL NOT WORK
     --   because effectful's runtime state is per-thread. Runs INSIDE
     --   runEffectfulHost so handlers stay installed for the drain's
     --   lifetime.
  -> JSM b
withAttachHydrationWidgetWithFailure onFailure switchoverAction rootElement w callback =
  withJSContextSingleton $ \(jsSingFresh :: JSContextSingleton xFresh) -> do
  let jsSing = unsafeCoerce jsSingFresh :: JSContextSingleton x
  ctx <- JSaddle.askJSM
  doc <- Node.getOwnerDocumentUnchecked rootElement
  liftIO $ do
    hydrationMode <- newIORef HydrationMode_Hydrating
    events        <- newChan
    HH.runEffectfulHost $ do
      ((_, postBuildTriggerRef, syncTriggerRef), fc@(FireCommand fire)) <-
        hostPerformEventT $ do
          (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
          (syncEv,   syncTriggerRef)       <- liftIO newEventWithTriggerRef
          unreadyChildren <- liftIO $ newIORef 0
          delayed <- liftIO $ newIORef
                       (RunnerAction (pure ()) :: DelayedHydration (DomTimeline))
          let hEnv = HydrationEnv
                { hydrationEnv_document        = toDocument doc
                , hydrationEnv_parent          = Left (toNode rootElement)
                , hydrationEnv_unreadyChildren = unreadyChildren
                , hydrationEnv_commitAction    = pure ()
                , hydrationEnv_hydrationMode   = hydrationMode
                , hydrationEnv_switchover      = syncEv
                , hydrationEnv_delayed         = delayed
                , hydrationEnv_events          = events
                }
          _ <-   HH.runTriggerEvent @R.SI.Global events
               . HH.runPostBuild postBuild
               . runReader hEnv
               . runJSM' ctx
               . runDomRenderHook @R.SI.Global
               . runHasDocumentHydration @(DomTimeline)
               . runDomBuilderHydration @(DomTimeline)
              . HH.runPerformEventH @(DomTimeline)
               $ runWidgetEff w jsSing
          let delayedAction = do
                runner <- liftIO (readIORef delayed)
                HH.runTriggerEvent @R.SI.Global events
                  . HH.runPostBuild
                      (R.never :: R.Event (DomTimeline) ())
                  . runReader hEnv
                  . runJSM' ctx
                  . runDomRenderHook @R.SI.Global
                  $ runHydrationRunnerWithFailure @(DomTimeline)
                      runner onFailure Nothing (toNode rootElement)
                liftIO $ writeIORef hydrationMode HydrationMode_Immediate
                liftIO $ runJSM switchoverAction ctx
          _ <- HH.runWithReplaceE @(DomTimeline)
                 (pure ()) (delayedAction <$ syncEv)
          pure ((), postBuildTriggerRef, syncTriggerRef)
      -- Fire postBuild synchronously (own frame).
      mPBT <- liftIO (readIORef postBuildTriggerRef)
      liftIO $ forM_ mPBT $ \t -> fire [t :=> Identity ()]
      -- Fire syncEvent ASYNCHRONOUSLY via events chan (mirrors upstream
      -- Main.hs:122 'fireSync ()'). See attachHydrationWidgetWithFailure
      -- for full rationale on why this must be async, not synchronous.
      liftIO $ writeChan events
        [HH.EventTriggerRef syncTriggerRef :=> HH.TriggerInvocation () (pure ())]
      -- Do NOT fork the drain. Effectful's runtime state is per-thread —
      -- a raw 'forkIO' spawns a thread with NO installed handlers, so
      -- 'fc' invocations from the drain thread trigger storage-mismatch
      -- when they need runAdjustableInRequesterT (which captures a
      -- LocalEnv). The callback below is expected to invoke
      -- 'processAsyncEvents events fc' inline on this thread (or wrap
      -- it however it needs); that way fc's calls happen in the SAME
      -- runtime that installed the handlers.
      --
      -- Caller responsibility: the callback MUST drive the drain either
      -- via 'liftIO $ processAsyncEvents events fc' inline, or by
      -- consuming 'events'/'fc' via its own mechanism. Blocking on an
      -- MVar without draining will hang.
      liftIO $ runJSM (callback events fc) ctx

-- | Analog of upstream reflex-dom-core's @attachHydrationWidget@
-- (Reflex.Dom.Main:96). Convenience: 'attachHydrationWidgetWithFailure'
-- with no-op onFailure.
attachHydrationWidget
  :: forall x e. DOM.IsElement e
  => JSM ()
  -> e
  -> (forall es. HydrationWidget x es)
  -> JSM ((), FireCommand)
attachHydrationWidget = attachHydrationWidgetWithFailure @x (pure ())


-- ---------------------------------------------------------------------
-- User-facing mainHydrationWidget* variants
-- ---------------------------------------------------------------------

-- | Bootstrap a hydration app targeting the document @<body>@. No head
-- widget, no switchover action, no failure handler.
mainHydrationWidget :: (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidget w = withDomHost $ do
  doc  <- GHCJS.DOM.currentDocumentUnchecked
  body <- getBodyUnchecked doc
  _ <- attachHydrationWidget (pure ()) body w
  pure ()

-- | Bootstrap a hydration app with independent head + body widgets.
-- Analog of upstream @mainHydrationWidgetWithHead@ (Reflex.Dom.Main:59).
mainHydrationWidgetWithHead :: (forall x es. HydrationWidget x es) -> (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidgetWithHead h b = mainHydrationWidgetWithHead' h b

-- | Perf-tweak variant of 'mainHydrationWidgetWithHead'. Same shape.
-- Analog of upstream @mainHydrationWidgetWithHead'@ (Reflex.Dom.Main:64).
mainHydrationWidgetWithHead' :: (forall x es. HydrationWidget x es) -> (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidgetWithHead' = mainHydrationWidgetWithSwitchoverAction' (pure ())

-- | Bootstrap head + body hydration with a switchover action fired once
-- after hydration completes (before the widget flips to immediate mode).
-- Analog of upstream @mainHydrationWidgetWithSwitchoverAction@
-- (Reflex.Dom.Main:68).
mainHydrationWidgetWithSwitchoverAction
  :: JSM () -> (forall x es. HydrationWidget x es) -> (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidgetWithSwitchoverAction sw h b =
  mainHydrationWidgetWithSwitchoverAction' sw h b

-- | Perf-tweak variant of 'mainHydrationWidgetWithSwitchoverAction'.
-- Analog of upstream @mainHydrationWidgetWithSwitchoverAction'@
-- (Reflex.Dom.Main:73).
mainHydrationWidgetWithSwitchoverAction'
  :: JSM () -> (forall x es. HydrationWidget x es) -> (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidgetWithSwitchoverAction' =
  mainHydrationWidgetWithSwitchoverActionWithFailure' (pure ())

-- | Full-featured hydration bootstrap: caller supplies both the
-- @onFailure@ IO hook (fired when hydration diverges from the
-- pre-rendered DOM) and the @switchoverAction@ JSM (fired exactly once
-- at switchover). Analog of upstream
-- @mainHydrationWidgetWithSwitchoverActionWithFailure'@
-- (Reflex.Dom.Main:79). Uses 'runHydrationWidgetWithHeadAndBodyWithFailure'
-- so head + body share a single FRP scope and switchoverAction fires
-- exactly once.
mainHydrationWidgetWithSwitchoverActionWithFailure'
  :: IO () -> JSM () -> (forall x es. HydrationWidget x es) -> (forall x es. HydrationWidget x es) -> IO ()
mainHydrationWidgetWithSwitchoverActionWithFailure' onFailure sw h b =
  runHydrationWidgetWithHeadAndBodyWithFailure onFailure sw $
    \appendHead appendBody -> do
      appendHead h
      appendBody b

-- ---------------------------------------------------------------------
-- Fused head + body hydration runner (single FRP scope)
-- ---------------------------------------------------------------------

-- | Analog of upstream reflex-dom-core's
-- @runHydrationWidgetWithHeadAndBody@ (Reflex.Dom.Main:140).
-- Convenience wrapper: 'runHydrationWidgetWithHeadAndBodyWithFailure'
-- with a no-op onFailure hook.
runHydrationWidgetWithHeadAndBody
  :: JSM ()
  -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> ( forall x esF. FloatingWidgetE esF
      => (forall c. (forall esW. HydrationWidgetE esW => WidgetEff DomTimeline x esW c) -> Eff esF c)
      -> (forall c. (forall esW. HydrationWidgetE esW => WidgetEff DomTimeline x esW c) -> Eff esF c)
      -> Eff esF ()
     )
  -- ^ App callback with @appendHead@ / @appendBody@ dispatchers.
  -> IO ()
runHydrationWidgetWithHeadAndBody =
  runHydrationWidgetWithHeadAndBodyWithFailure (pure ())

-- | Full head + body hydration bootstrap. Analog of upstream
-- @runHydrationWidgetWithHeadAndBodyWithFailure@ (Reflex.Dom.Main:150).
--
--   1. Look up @<head>@ / @<body>@ elements.
--   2. Allocate 'HydrationMode_Hydrating' ref + trigger-events chan +
--      shared @rootNodesRef@ + postBuild + syncEvent triggers.
--   3. Bootstrap a SINGLE 'hostPerformEventT' scope; inside, build head
--      + body appenders that install a per-subwidget 'HydrationEnv',
--      run the guest widget under @runDomBuilderHydration +
--      runHasDocumentHydration@, snapshot the queued runner and push
--      @(parentNode, hEnv, runner)@ into @rootNodesRef@.
--   4. Invoke the caller's 'app' with the two appenders.
--   5. Schedule 'delayedAction' on syncEvent via 'runWithReplaceE' —
--      the action iterates @rootNodesRef@, runs each stored
--      'HydrationRunner' under the correct per-subwidget 'HydrationEnv',
--      then flips 'hydrationMode' to 'HydrationMode_Immediate' and
--      fires 'switchoverAction' exactly ONCE.
--   6. Fire postBuild, then syncEvent.
--   7. Fork the async-events drain.
--
-- Head + body share a single postBuild, a single FireCommand, and a
-- single switchoverAction fire — matching upstream semantics.
runHydrationWidgetWithHeadAndBodyWithFailure
  :: IO ()
  -- ^ 'onFailure' — fires when hydration diverges from pre-rendered DOM.
  -> JSM ()
  -- ^ 'switchoverAction' — JSM fired once at switchover.
  -> ( forall x esF. FloatingWidgetE esF
      => (forall c. (forall esW. HydrationWidgetE esW => WidgetEff DomTimeline x esW c) -> Eff esF c)
      -> (forall c. (forall esW. HydrationWidgetE esW => WidgetEff DomTimeline x esW c) -> Eff esF c)
      -> Eff esF ()
     )
  -> IO ()
runHydrationWidgetWithHeadAndBodyWithFailure onFailure switchoverAction app =
  withDomHost $
  withJSContextSingleton $ \(jsSing :: JSContextSingleton x) -> do
    ctx      <- JSaddle.askJSM
    doc      <- GHCJS.DOM.currentDocumentUnchecked
    headElem <- Document.getHeadUnchecked doc
    bodyElem <- getBodyUnchecked doc
    liftIO $ do
      hydrationMode <- newIORef HydrationMode_Hydrating
      events        <- newChan
      rootNodesRef  <- newIORef
        ([] :: [( Node
                , HydrationEnv (DomTimeline)
                , DelayedHydration (DomTimeline)
                )])
      (_, fc) <- HH.runEffectfulHost $ do
        ((_, postBuildTriggerRef, syncTriggerRef), fc@(FireCommand fire)) <-
          hostPerformEventT $ do
            (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
            (syncEv,   syncTriggerRef)       <- liftIO newEventWithTriggerRef
            let hydrateAt
                  :: forall c esFinner. FloatingWidgetE esFinner
                  => Node
                  -> (forall esW. HydrationWidgetE esW => WidgetEff DomTimeline x esW c)
                  -> Eff esFinner c
                hydrateAt parentNode guest = do
                  unreadyChildren <- liftIO $ newIORef 0
                  delayed <- liftIO $ newIORef
                               (RunnerAction (pure ()) :: DelayedHydration (DomTimeline))
                  let hEnv = HydrationEnv
                        { hydrationEnv_document        = doc
                        , hydrationEnv_parent          = Left parentNode
                        , hydrationEnv_unreadyChildren = unreadyChildren
                        , hydrationEnv_commitAction    = pure ()
                        , hydrationEnv_hydrationMode   = hydrationMode
                        , hydrationEnv_switchover      = syncEv
                        , hydrationEnv_delayed         = delayed
                        , hydrationEnv_events          = events
                        }
                  result <- runReader hEnv
                    . runHasDocumentHydration @(DomTimeline)
                    . runDomBuilderHydration  @(DomTimeline)
                    $ runWidgetEff guest jsSing
                  runner <- liftIO (readIORef delayed)
                  liftIO $ modifyIORef' rootNodesRef ((parentNode, hEnv, runner) :)
                  pure result
            _ <-   HH.runTriggerEvent @R.SI.Global events
                 . HH.runPostBuild postBuild
                 . runJSM' ctx
                 . runDomRenderHook @R.SI.Global
                 . HH.runPerformEventH @(DomTimeline)
                 $ app (hydrateAt (toNode headElem))
                       (hydrateAt (toNode bodyElem))
            let delayedAction = do
                  rootNodes <- liftIO (readIORef rootNodesRef)
                  forM_ (P.reverse rootNodes) $ \(parentNode, hEnv, runner) ->
                    HH.runTriggerEvent @R.SI.Global events
                      . HH.runPostBuild
                          (R.never :: R.Event (DomTimeline) ())
                      . runReader hEnv
                      . runJSM' ctx
                      . runDomRenderHook @R.SI.Global
                      $ runHydrationRunnerWithFailure @(DomTimeline)
                          runner onFailure Nothing parentNode
                  liftIO $ writeIORef hydrationMode HydrationMode_Immediate
                  liftIO $ runJSM switchoverAction ctx
            _ <- HH.runWithReplaceE @(DomTimeline)
                   (pure ()) (delayedAction <$ syncEv)
            pure ((), postBuildTriggerRef, syncTriggerRef)
        -- Fire postBuild synchronously (own frame).
        mPBT <- liftIO (readIORef postBuildTriggerRef)
        liftIO $ forM_ mPBT $ \t -> fire [t :=> Identity ()]
        -- Fire syncEvent ASYNCHRONOUSLY via events chan (mirrors upstream
        -- Main.hs:122 'fireSync ()'). See attachHydrationWidgetWithFailure
        -- for the full rationale on why this must be async, not synchronous.
        liftIO $ writeChan events
          [HH.EventTriggerRef syncTriggerRef :=> HH.TriggerInvocation () (pure ())]
        -- Option 3: fork drain INSIDE runEffectfulHost's scope.
        _ <- liftIO $ forkIO $ processAsyncEvents events fc
        pure ((), fc)
      pure ()

-- =====================================================================
-- Static-rendering surface — honest Eff-native port of upstream's
-- @Reflex.Dom.Builder.Static@.
--
-- Replaces the MTL @StaticDomBuilderT@ transformer with:
--
--   * 'StaticDomSpace' — phantom space marker; every associated raw
--     type collapses to @()@ (no live DOM element in static mode).
--   * @Reader (StaticDomBuilderEnv t) :> es@ — ambient escape rule +
--     ambient @<select>@ value + fresh key counter.
--   * @State [Behavior t Builder] :> es@ — byte accumulator in
--     REVERSE assembly order (upstream shape).
--   * 'runDomBuilderStatic' — the @DomBuilderE t StaticDomSpace@
--     interpreter.
--   * 'runHasDocumentStatic' — @askDocument@ returns unit.
--   * 'runTriggerEventStatic' — every trigger op returns
--     'R.never' + no-op fire (upstream semantics).
--   * 'runNotReadyStatic' — no-op (static mode has no stall).
--   * 'runAdjustableStatic' — dispatches to the ambient 'Adjustable'
--     handler with per-child byte-collection.
--   * 'renderStatic' — top-level bootstrap returning @(a, ByteString)@.
--
-- Byte output is byte-for-byte identical to upstream
-- 'Reflex.Dom.Builder.Static.renderStatic' for the same widget input:
-- same escaping rules ('style'/'script' passthrough, everything else
-- HTML-escaped), same reverse-order concat, same HTML void-element
-- handling, same @option@ selected-attr injection.
--
-- This enables the 'HasPrerender' 'StaticDomSpace' instance defined
-- immediately after: server-side rendering pipelines can now write
-- @prerender serverBranch clientBranch@ where the server branch runs
-- for real (producing bytes) and the client branch is discarded via
-- the 'UnrunnableEs' trap row.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Static data types
-- ---------------------------------------------------------------------

-- | Timeline used for static rendering. Pinned to Global Spider — the
-- same timeline every other Global-bound composer in this module uses.
type StaticDomTimeline = DomTimeline

-- | Ambient config for the static interpreter: HTML-escape rule, the
-- ambient @<select>@ value (for @<option selected>@ injection), and a
-- fresh key counter for @runWithReplace@ start/end markers.
data StaticDomBuilderEnv t = StaticDomBuilderEnv
  { _staticDomBuilderEnv_shouldEscape         :: !Bool
  , _staticDomBuilderEnv_selectValue          :: !(Maybe (R.Behavior t Text))
  , _staticDomBuilderEnv_nextRunWithReplaceKey :: !(IORef Int)
  , _staticDomBuilderEnv_postBuild             :: !(R.Event t ())
  }

-- StaticDomSpace / StaticDomEvent / StaticDomHandler / StaticEventSpec
-- now imported from variant B's Reflex.Dom.Builder.Static.

-- | Effect row consumed by the static interpreters. Pinned to
-- @t ~ StaticDomTimeline@ so 'Hold' / 'Sample' / fan-event allocation
-- resolve to their concrete Global Spider interpreters.
type StaticE t es =
  ( t ~ StaticDomTimeline
  , R.Reflex t
  , Reader (StaticDomBuilderEnv t) :> es
  , State [R.Behavior t Builder] :> es
  , HH.Hold t :> es
  , HH.Sample t :> es
  , HH.CreateTrigger t :> es
  , IOE :> es
  )

-- ---------------------------------------------------------------------
-- HasDocumentE interpreter
-- ---------------------------------------------------------------------

-- | @RawDocument StaticDomSpace ~ ()@ — 'askDocument' returns unit.
runHasDocumentStatic
  :: forall es a
   . Eff (HasDocumentE StaticDomSpace : es) a
  -> Eff es a
runHasDocumentStatic = interpret $ \_ -> \case
  AskDocument -> pure ()
{-# INLINABLE runHasDocumentStatic #-}

-- ---------------------------------------------------------------------
-- DomBuilderE interpreter — StaticDomSpace
-- ---------------------------------------------------------------------

-- | Interpret @DomBuilderE t StaticDomSpace@ over the ambient Reader /
-- State / FRP row. Each arm mirrors upstream's
-- @instance DomBuilderE t (StaticDomBuilderT t m)@ verbatim; only the
-- state location (effect row vs transformer stack) differs.
runDomBuilderStatic
  :: forall t es a
   . StaticE t es
  => Eff (DomBuilderE t StaticDomSpace : es) a
  -> Eff es a
runDomBuilderStatic = interpret $ \env -> \case
  DB_TextNode cfg           -> handleTextNodeStatic cfg
  DB_CommentNode cfg        -> handleCommentNodeStatic cfg
  DB_Element tag cfg body   ->
    localSeqUnlift env $ \unlift ->
      handleElementStatic tag cfg (unlift body)
  DB_InputElement cfg       -> handleInputElementStatic cfg
  DB_TextAreaElement cfg    -> handleTextAreaElementStatic cfg
  DB_SelectElement cfg body ->
    localSeqUnlift env $ \unlift ->
      handleSelectElementStatic cfg (unlift body)
  DB_PlaceRawElement ()     -> pure ()
  DB_WrapRawElement () _    ->
    pure (Element (R.EventSelector (P.const R.never)) ())
{-# INLINABLE runDomBuilderStatic #-}

-- ---------------------------------------------------------------------
-- Per-constructor helpers
-- ---------------------------------------------------------------------

-- | Emit a text node's byte behavior onto the accumulator.
handleTextNodeStatic
  :: forall t es
   . StaticE t es
  => TextNodeConfig t
  -> Eff es (TextNode StaticDomSpace t)
handleTextNodeStatic (TextNodeConfig initial mSet) = do
  escape <- escaperStatic @t
  b <- case mSet of
    Nothing -> pure (R.constant (escape initial))
    Just ev -> R.hold (escape initial) (R.fmapCheap escape ev)
  modify @[R.Behavior t Builder] (b :)
  pure (TextNode ())

-- | Emit a comment node's byte behavior onto the accumulator.
handleCommentNodeStatic
  :: forall t es
   . StaticE t es
  => CommentNodeConfig t
  -> Eff es (CommentNode StaticDomSpace t)
handleCommentNodeStatic (CommentNodeConfig initial mSet) = do
  escape <- escaperStatic @t
  inner <- case mSet of
    Nothing -> pure (R.constant (escape initial))
    Just ev -> R.hold (escape initial) (R.fmapCheap escape ev)
  modify @[R.Behavior t Builder]
    (fmap (\c -> "<!--" <> c <> "-->") inner :)
  pure (CommentNode ())

-- | Read @_staticDomBuilderEnv_shouldEscape@ and pick the matching
-- text-to-Builder converter.
escaperStatic :: forall t es. StaticE t es => Eff es (Text -> Builder)
escaperStatic = do
  shouldEscape <- asks @(StaticDomBuilderEnv t) _staticDomBuilderEnv_shouldEscape
  pure $ if shouldEscape
    then fromHtmlEscapedText
    else BSB.byteString . encodeUtf8

-- | Enter a fresh child scope: save outer state, reset to @[]@, run
-- @child@ under a locally-modified reader env, extract child bytes,
-- restore outer state.
withChildBuilderStatic
  :: forall t es a
   . StaticE t es
  => Text
  -> Eff es a
  -> Eff es (a, R.Behavior t Builder)
withChildBuilderStatic tag child = do
  outerAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] []
  let noEscape = Set.fromList ["style", "script"]
      shouldEscape = tag `Set.notMember` noEscape
      resetEnv old = old
        { _staticDomBuilderEnv_shouldEscape = shouldEscape
        , _staticDomBuilderEnv_selectValue  = Nothing
        }
  result <- local @(StaticDomBuilderEnv t) resetEnv child
  childAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] outerAcc
  pure (result, mconcat (P.reverse childAcc))

-- | Emit an element: allocate the fan-event, recurse into child scope,
-- splice open/close tags around child bytes (self-closing for HTML
-- void elements).
handleElementStatic
  :: forall t er es a
   . StaticE t es
  => Text
  -> ElementConfig er t StaticDomSpace
  -> Eff es a
  -> Eff es (Element er StaticDomSpace t, a)
handleElementStatic tag cfg child = do
  es' <- newFanEventStatic @t @er
  (result, innerHtml) <- withChildBuilderStatic @t tag child
  emitElementBytesStatic @t @er tag cfg innerHtml
  pure (Element es' (), result)

-- | Allocate a fresh no-op fan-event trigger. Static elements never
-- dispatch subscribers, but the shape must be a real 'EventSelector'.
newFanEventStatic
  :: forall t er es
   . StaticE t es
  => Eff es (R.EventSelector t (WrapArg er EventName))
newFanEventStatic = HH.newFanEventWithTriggerE (\_ _ -> pure (pure ()))

-- | Splice open/close bytes around child bytes; handle HTML void
-- elements as self-closing; inject @selected@ attr for @<option>@
-- children of @<select>@ whose ambient value matches.
emitElementBytesStatic
  :: forall t er es
   . StaticE t es
  => Text
  -> ElementConfig er t StaticDomSpace
  -> R.Behavior t Builder
  -> Eff es ()
emitElementBytesStatic tag cfg innerHtml = do
  attrs2 <- attrsBehaviorStatic @t @er tag cfg
  let tagBS = encodeUtf8 tag
  if Set.member tag voidElementsStatic
    then modify @[R.Behavior t Builder] $ (:) $
      mconcat
        [ R.constant ("<" <> BSB.byteString tagBS)
        , attrs2
        , R.constant (BSB.byteString " />")
        ]
    else do
      let open  = mconcat
            [ R.constant ("<" <> BSB.byteString tagBS)
            , attrs2
            , R.constant (BSB.byteString ">")
            ]
          close = R.constant $ BSB.byteString $ "</" <> tagBS <> ">"
      modify @[R.Behavior t Builder] $ (:) $ mconcat [open, innerHtml, close]

-- | Fold the modify-attributes event over the initial attributes;
-- inject the @selected@ attribute at matching @option@ children of a
-- @<select>@; serialize to a byte 'Builder'.
attrsBehaviorStatic
  :: forall t er es
   . StaticE t es
  => Text
  -> ElementConfig er t StaticDomSpace
  -> Eff es (R.Behavior t Builder)
attrsBehaviorStatic tag cfg = do
  attrs0 <- RD.foldDyn Map.Misc.applyMap
    (cfg ^. elementConfig_initialAttributes)
    (fromMaybe R.never (_elementConfig_modifyAttributes cfg))
  selectValue <- asks @(StaticDomBuilderEnv t) _staticDomBuilderEnv_selectValue
  let attrs1 = case (tag, selectValue) of
        ("option", Just sv) -> R.pull $ do
          a <- R.sample (R.current attrs0)
          v <- R.sample sv
          pure (addSelectedAttrStatic a v)
        _ -> R.current attrs0
  pure $ fmap (mconcat . fmap (\(k, v) -> " " <> toAttrStatic k v) . Map.toList) attrs1

-- | Serialize a single @key="value"@ attribute pair to a 'Builder'.
toAttrStatic :: AttributeName -> Text -> Builder
toAttrStatic (AttributeName _mns k) v =
  BSB.byteString (encodeUtf8 k)
    <> BSB.byteString "=\""
    <> fromHtmlEscapedText v
    <> BSB.byteString "\""

-- | If the parent @<select>@\'s current value matches this option's
-- value, inject a @selected@ attribute; otherwise strip any stale
-- @selected@.
addSelectedAttrStatic
  :: Map AttributeName Text -> Text -> Map AttributeName Text
addSelectedAttrStatic attrs sel = case Map.lookup "value" attrs of
  Just v | v == sel -> attrs <> Map.singleton "selected" ""
  _                 -> Map.delete "selected" attrs

-- | HTML void elements — no closing tag.
voidElementsStatic :: Set.Set Text
voidElementsStatic = Set.fromList
  [ "area", "base", "br", "col", "command", "embed", "hr", "img"
  , "input", "keygen", "link", "meta", "param", "source", "track", "wbr"
  ]

-- ---------------------------------------------------------------------
-- Input / textarea / select
-- ---------------------------------------------------------------------

-- | Emit an @<input>@ element with @value@/@checked@ threaded into the
-- attribute stream.
handleInputElementStatic
  :: forall t er es
   . StaticE t es
  => InputElementConfig er t StaticDomSpace
  -> Eff es (InputElement er StaticDomSpace t)
handleInputElementStatic cfg = do
  let adjustedConfig = inputConfigWithValueAttrsStatic cfg
  (e, ()) <- handleElementStatic @t @er "input" adjustedConfig (pure ())
  v <- valueDynStatic @t cfg
  c <- checkedDynStatic @t cfg
  pure InputElement
    { _inputElement_value         = v
    , _inputElement_checked       = c
    , _inputElement_checkedChange = R.never
    , _inputElement_input         = R.never
    , _inputElement_hasFocus      = pure False
    , _inputElement_element       = e
    , _inputElement_raw           = ()
    , _inputElement_files         = pure mempty
    }

-- | Merge @value@ / @checked@ config knobs into the underlying element
-- config so serialization emits them as attributes.
inputConfigWithValueAttrsStatic
  :: forall t er
   . R.Reflex t
  => InputElementConfig er t StaticDomSpace
  -> ElementConfig er t StaticDomSpace
inputConfigWithValueAttrsStatic cfg =
  _inputElementConfig_elementConfig cfg
    & elementConfig_initialAttributes %~ setInitialValue . setInitialChecked
    & elementConfig_modifyAttributes  %~ setUpdatedValue . setUpdatedChecked
  where
    setInitialValue = Map.insert "value" (_inputElementConfig_initialValue cfg)
    setInitialChecked = if _inputElementConfig_initialChecked cfg
      then Map.insert "checked" "checked"
      else P.id
    setUpdatedValue
      :: R.Event t (Map AttributeName (Maybe Text))
      -> R.Event t (Map AttributeName (Maybe Text))
    setUpdatedValue updatedAttrs = case _inputElementConfig_setValue cfg of
      Nothing -> updatedAttrs
      Just e  -> (Map.singleton "value" . Just <$> e) <> updatedAttrs
    setUpdatedChecked
      :: R.Event t (Map AttributeName (Maybe Text))
      -> R.Event t (Map AttributeName (Maybe Text))
    setUpdatedChecked updatedAttrs = case _inputElementConfig_setChecked cfg of
      Nothing -> updatedAttrs
      Just e  -> (Map.singleton "checked" (Just "checked") <$ e) <> updatedAttrs

-- | Track the input's current value as a 'Dynamic'.
valueDynStatic
  :: forall t er es
   . StaticE t es
  => InputElementConfig er t StaticDomSpace
  -> Eff es (R.Dynamic t Text)
valueDynStatic cfg = case _inputElementConfig_setValue cfg of
  Nothing -> pure (pure (cfg ^. inputElementConfig_initialValue))
  Just ev -> R.holdDyn (cfg ^. inputElementConfig_initialValue) ev

-- | Track the input's current @checked@ state as a 'Dynamic'.
checkedDynStatic
  :: forall t er es
   . StaticE t es
  => InputElementConfig er t StaticDomSpace
  -> Eff es (R.Dynamic t Bool)
checkedDynStatic cfg = case _inputElementConfig_setChecked cfg of
  Nothing -> pure (pure (_inputElementConfig_initialChecked cfg))
  Just ev -> R.holdDyn (_inputElementConfig_initialChecked cfg) ev

-- | Emit a @<textarea>@ element with the initial value as a child text
-- node (matches upstream).
handleTextAreaElementStatic
  :: forall t er es
   . StaticE t es
  => TextAreaElementConfig er t StaticDomSpace
  -> Eff es (TextAreaElement er StaticDomSpace t)
handleTextAreaElementStatic cfg = do
  (e, ()) <- handleElementStatic @t @er "textarea"
    (_textAreaElementConfig_elementConfig cfg) $
      void $ handleTextNodeStatic @t $
        def & textNodeConfig_initialContents .~ _textAreaElementConfig_initialValue cfg
            & textNodeConfig_setContents     .~ fromMaybe R.never (_textAreaElementConfig_setValue cfg)
  v <- case _textAreaElementConfig_setValue cfg of
    Nothing -> pure (pure (cfg ^. textAreaElementConfig_initialValue))
    Just ev -> R.holdDyn (cfg ^. textAreaElementConfig_initialValue) ev
  pure TextAreaElement
    { _textAreaElement_value    = v
    , _textAreaElement_input    = R.never
    , _textAreaElement_hasFocus = pure False
    , _textAreaElement_element  = e
    , _textAreaElement_raw      = ()
    }

-- | Emit a @<select>@ element. Manual save/reset/local/restore to
-- match upstream's inner-@runStaticDomBuilderT@ pattern with
-- @shouldEscape = False@ and the ambient select value installed.
handleSelectElementStatic
  :: forall t er es a
   . StaticE t es
  => SelectElementConfig er t StaticDomSpace
  -> Eff es a
  -> Eff es (SelectElement er StaticDomSpace t, a)
handleSelectElementStatic cfg child = do
  v <- R.holdDyn (cfg ^. selectElementConfig_initialValue)
    (fromMaybe R.never (_selectElementConfig_setValue cfg))
  es' <- newFanEventStatic @t @er
  outerAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] []
  let selectEnv old = old
        { _staticDomBuilderEnv_shouldEscape = False
        , _staticDomBuilderEnv_selectValue  = Just (R.current v)
        }
  result <- local @(StaticDomBuilderEnv t) selectEnv child
  childAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] outerAcc
  let innerHtml = mconcat (P.reverse childAcc)
  emitElementBytesStatic @t @er "select"
    (_selectElementConfig_elementConfig cfg) innerHtml
  let e = Element es' ()
      wrapped = SelectElement
        { _selectElement_value    = v
        , _selectElement_change   = R.never
        , _selectElement_hasFocus = pure False
        , _selectElement_element  = e
        , _selectElement_raw      = ()
        }
  pure (wrapped, result)

-- ---------------------------------------------------------------------
-- Run-with-replace markers
-- ---------------------------------------------------------------------

-- | Emit a @replace-start@ marker and return the fresh key.
replaceStartStatic
  :: forall t es
   . StaticE t es
  => StaticDomBuilderEnv t
  -> Eff es Text
replaceStartStatic env = do
  str <- liftIO $ P.show <$>
    atomicModifyIORef' (_staticDomBuilderEnv_nextRunWithReplaceKey env)
      (\k -> (P.succ k, k))
  let key = "-" <> T.pack str
  _ <- handleCommentNodeStatic @t $
    def & commentNodeConfig_initialContents .~ ("replace-start" <> key)
  pure key

-- | Emit the matching @replace-end@ marker.
replaceEndStatic
  :: forall t es
   . StaticE t es
  => Text
  -> Eff es ()
replaceEndStatic key = void $ handleCommentNodeStatic @t $
  def & commentNodeConfig_initialContents .~ ("replace-end" <> key)

-- ---------------------------------------------------------------------
-- renderStatic bootstrap
-- ---------------------------------------------------------------------

-- | Render a widget to a UTF-8 HTML 'ByteString'. Byte-for-byte
-- equivalent to upstream 'Reflex.Dom.Builder.Static.renderStatic'.
--
-- Interpreter stack (outside-in):
--
-- @
--   runEff
--     . HH.runHold                       -- Hold t
--     . HH.runSample                     -- Sample t
--     . HH.runTimeline @t                -- Timeline t
--     . runReader env0                   -- Reader (StaticDomBuilderEnv t)
--     . evalState []                     -- State [Behavior t Builder]
--     . runHasDocumentStatic             -- HasDocumentE StaticDomSpace
--     . runDomBuilderStatic              -- DomBuilderE t StaticDomSpace
--     $ (widget >>= extractBytes)
-- @
--
-- Ports upstream @Reflex.Dom.Builder.Static.renderStatic@
-- (reflex-dom-core Static.hs:386-397) into Eff-shape. The composer
-- MUST stack 'HH.PostBuild' / 'HH.PerformEvent' / 'HH.TriggerEvent'
-- / 'HH.NotReady' / 'HH.Adjustable' + fire the postBuild trigger
-- BEFORE sampling the byte builder — otherwise any widget that uses
-- @getPostBuild@ to append bytes has its update silently dropped
-- from the static HTML output.
renderStatic
  :: (forall es
       . ( DomBuilderE StaticDomTimeline StaticDomSpace :> es
         , HasDocumentE StaticDomSpace :> es
         , HasDocumentSpace es ~ StaticDomSpace
         , HH.Hold StaticDomTimeline :> es
         , HH.Sample StaticDomTimeline :> es
         , HH.CreateTrigger StaticDomTimeline :> es
         , HH.PostBuild StaticDomTimeline :> es
         , HH.TriggerEvent StaticDomTimeline :> es
         , HH.NotReady StaticDomTimeline :> es
         , HH.Adjustable StaticDomTimeline :> es
         , HH.PerformEventM (RH.HostFrame StaticDomTimeline) StaticDomTimeline :> es
         , HH.Requester
             StaticDomTimeline
             (R.SI.SpiderHostFrame R.SI.Global)
             Identity :> es
         , Reader (StaticDomBuilderEnv StaticDomTimeline) :> es
         , State [R.Behavior StaticDomTimeline Builder] :> es
         , IOE :> es
         , Effectful.Prim.Prim :> es
         , DomBuilderTimelineE es ~ StaticDomTimeline
         , DomBuilderSpaceE    es ~ StaticDomSpace
         , HH.PerformableForRow es ~ RH.HostFrame StaticDomTimeline
         ) => Eff es a)
  -> IO (a, ByteString)
renderStatic widget = do
  nextKey <- newIORef 0
  HH.runEffectfulHost . HH.EffectfulHost $ do
    (postBuild, postBuildTriggerRef) <- liftIO newEventWithTriggerRef
    let env0 = StaticDomBuilderEnv True Nothing nextKey postBuild
    ((r, bsBhv), FireCommand fire) <- HH.unEffectfulHost . hostPerformEventT $
        runTriggerEventStatic @StaticDomTimeline
      . runNotReadyStatic @StaticDomTimeline
      . runReader env0
      . evalState @[R.Behavior StaticDomTimeline Builder] []
      . runPostBuildStatic @StaticDomTimeline
      . runAdjustableStatic @StaticDomTimeline
      . runHasDocumentStatic
      . runDomBuilderStatic @StaticDomTimeline
      . Effectful.Prim.runPrim
      $ do
          a   <- widget
          acc <- get @[R.Behavior StaticDomTimeline Builder]
          pure (a, mconcat (P.reverse acc))
    mPostBuildTrigger <- liftIO (readIORef postBuildTriggerRef)
    liftIO $ forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()]
    finalBs <- R.sample bsBhv
    pure (r, LBS.toStrict (BSB.toLazyByteString finalBs))
{-# INLINABLE renderStatic #-}

-- ---------------------------------------------------------------------
-- TriggerEvent / NotReady interpreters (static-scope)
-- ---------------------------------------------------------------------

-- | Static-mode 'TriggerEvent': every constructor returns 'R.never' +
-- a no-op fire callback. Matches upstream verbatim.
runTriggerEventStatic
  :: forall t es a
   . (R.Reflex t, IOE :> es)
  => Eff (HH.TriggerEvent t : es) a
  -> Eff es a
runTriggerEventStatic = interpret $ \_ -> \case
  HH.NewTriggerEvent                         -> pure (R.never, P.const (pure ()))
  HH.NewTriggerEventWithOnComplete           -> pure (R.never, \_ _ -> pure ())
  HH.NewEventWithLazyTriggerWithOnComplete _ -> pure R.never
{-# INLINABLE runTriggerEventStatic #-}

-- | Static-mode 'NotReady': both operations are no-ops (static
-- rendering has no "not-ready" state to wait on). Matches upstream.
runNotReadyStatic
  :: forall t es a
   . Eff (HH.NotReady t : es) a
  -> Eff es a
runNotReadyStatic = interpret $ \_ -> \case
  HH.NotReadyUntil _ -> pure ()
  HH.NotReady        -> pure ()
{-# INLINABLE runNotReadyStatic #-}

-- ---------------------------------------------------------------------
-- Adjustable interpreter (static-scope)
-- ---------------------------------------------------------------------

-- | Static-mode 'Adjustable': dispatches each constructor to the
-- ambient (outer) 'Adjustable' handler with per-child byte-collection,
-- and emits @replace-start@ / @replace-end@ comment markers around
-- each cohort. Requires an OUTER @HH.Adjustable t :> es@ handler
-- (typically eff-reflex's 'HH.runAdjustable') to actually queue the
-- sub-widget executions.
-- | Resolve postBuild from the current static widget's environment.
runPostBuildStatic
  :: forall t es a. Reader (StaticDomBuilderEnv t) :> es
  => Eff (HH.PostBuild t : es) a -> Eff es a
runPostBuildStatic = interpret $ \_ HH.GetPostBuild ->
  asks @(StaticDomBuilderEnv t) _staticDomBuilderEnv_postBuild

runAdjustableStatic
  :: forall t es a
   . ( t ~ StaticDomTimeline
     , R.Reflex t
     , Reader (StaticDomBuilderEnv t) :> es
     , State [R.Behavior t Builder] :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , HH.CreateTrigger t :> es
     , HH.Adjustable t :> es
     , IOE :> es
     )
  => Eff (HH.Adjustable t : es) a
  -> Eff es a
-- Replacement children run after the initial interpreter scope returns.
-- Lend the byte accumulator before cloning, so child writes and collection
-- use the same storage; collecting outside the clone would lose those bytes.
runAdjustableStatic = interpret $ \env -> \case
  HH.RunWithReplace a0 a' ->
    localUnlift env SeqForkUnlift $ \unlift ->
    localLend @'[State [R.Behavior t Builder], Reader (StaticDomBuilderEnv t)] env SeqForkUnlift $ \lend ->
      runWithReplaceStatic @t
        (\postBuild child -> unlift $ lend $
          local @(StaticDomBuilderEnv t)
            (\e -> e { _staticDomBuilderEnv_postBuild = postBuild }) $
          runChildBytesStatic @t $ raise $ raise child) a0 a'
  HH.TraverseIntMapWithKeyWithAdjustOp f im0 im' ->
    localUnlift env SeqForkUnlift $ \unlift ->
    localLend @'[State [R.Behavior t Builder], Reader (StaticDomBuilderEnv t)] env SeqForkUnlift $ \lend ->
      hoistIntMapWithKeyWithAdjustStatic @t
        (\child -> unlift $ lend $ runChildBytesStatic @t $ raise $ raise child)
        RA.traverseIntMapWithKeyWithAdjust
        f
        im0 im'
  HH.TraverseDMapWithKeyWithAdjustOp f dm0 dm' ->
    localUnlift env SeqForkUnlift $ \unlift ->
    localLend @'[State [R.Behavior t Builder], Reader (StaticDomBuilderEnv t)] env SeqForkUnlift $ \lend ->
      hoistDMapWithKeyWithAdjustStatic @t
        (\child -> unlift $ lend $ runChildBytesStatic @t $ raise $ raise child)
        RA.traverseDMapWithKeyWithAdjust
        mapPatchDMap
        f
        dm0 dm'
  HH.TraverseDMapWithKeyWithAdjustWithMoveOp f dm0 dm' ->
    localUnlift env SeqForkUnlift $ \unlift ->
    localLend @'[State [R.Behavior t Builder], Reader (StaticDomBuilderEnv t)] env SeqForkUnlift $ \lend ->
      hoistDMapWithKeyWithAdjustStatic @t
        (\child -> unlift $ lend $ runChildBytesStatic @t $ raise $ raise child)
        RA.traverseDMapWithKeyWithAdjustWithMove
        mapPatchDMapWithMove
        f
        dm0 dm'
{-# INLINABLE runAdjustableStatic #-}

-- | Body of static @runWithReplace@: emit @replace-start@ marker,
-- delegate to the outer 'HH.runWithReplaceE' with each child under a
-- fresh byte-accumulator scope, hold the per-cohort byte behavior,
-- push @join o@ onto the outer accumulator, emit @replace-end@.
runWithReplaceStatic
  :: forall t es localEs a b
   . ( t ~ StaticDomTimeline
     , Reader (StaticDomBuilderEnv t) :> es
     , State [R.Behavior t Builder] :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , HH.CreateTrigger t :> es
     , HH.Adjustable t :> es
     , IOE :> es
     )
  => (forall c. R.Event t () -> Eff localEs c -> Eff es (c, R.Behavior t Builder))
  -> Eff localEs a
  -> R.Event t (Eff localEs b)
  -> Eff es (a, R.Event t b)
runWithReplaceStatic collect a0 a' = do
  e <- ask @(StaticDomBuilderEnv t)
  key <- replaceStartStatic @t e
  rec (result0, result') <- HH.runWithReplaceE @t
        (collect (_staticDomBuilderEnv_postBuild e) a0)
        (fmap (\child -> do
          postBuild <- R.headE (() <$ result')
          collect postBuild child) a')
  o <- R.hold (P.snd result0) (R.fmapCheap P.snd result')
  modify @[R.Behavior t Builder] (Monad.join o :)
  replaceEndStatic @t key
  pure (P.fst result0, R.fmapCheap P.fst result')

-- | Port of upstream @hoistIntMapWithKeyWithAdjust@. Delegates to the
-- outer widget-scope base traversal, wrapping each child in
-- 'runChildBytesStatic'; holds the incremental of byte-behaviors and
-- pushes a @pull@-computed byte concatenation onto the outer accumulator.
hoistIntMapWithKeyWithAdjustStatic
  :: forall t p a b es localEs
   . ( t ~ StaticDomTimeline
     , Reader (StaticDomBuilderEnv t) :> es
     , State [R.Behavior t Builder] :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , IOE :> es
     , Functor p
     , Patch (p (R.Behavior t Builder))
     , PatchTarget (p (R.Behavior t Builder)) ~ IntMap (R.Behavior t Builder)
     )
  => (forall c. Eff localEs c -> Eff es (c, R.Behavior t Builder))
  -> (   (IntMap.Key -> a -> Eff es (b, R.Behavior t Builder))
      -> IntMap a
      -> R.Event t (p a)
      -> Eff es (IntMap (b, R.Behavior t Builder), R.Event t (p (b, R.Behavior t Builder)))
     )
  -> (IntMap.Key -> a -> Eff localEs b)
  -> IntMap a
  -> R.Event t (p a)
  -> Eff es (IntMap b, R.Event t (p b))
hoistIntMapWithKeyWithAdjustStatic collect base f im0 im' = do
  (children0, children') <- base
    (\k v -> collect (f k v))
    im0 im'
  let result0 = IntMap.map P.fst children0
      result' = (fmap . fmap) P.fst children'
      outputs0 :: IntMap (R.Behavior t Builder)
      outputs0 = IntMap.map P.snd children0
      outputs' :: R.Event t (p (R.Behavior t Builder))
      outputs' = (fmap . fmap) P.snd children'
  outputs <- R.holdIncremental outputs0 outputs'
  modify @[R.Behavior t Builder] $ (:) $ R.pull $ do
    os <- R.sample (R.currentIncremental outputs)
    fmap mconcat $ forM (IntMap.toList os) $ \(_, o) -> R.sample o
  pure (result0, result')

-- | Port of upstream @hoistDMapWithKeyWithAdjust@. Delegates to the
-- outer base traversal, using @Compose . swap@ to re-shape each
-- child's @(result, Behavior)@ pair; holds the incremental of
-- byte-behaviors and pushes a @pull@-computed byte concatenation onto
-- the outer accumulator.
hoistDMapWithKeyWithAdjustStatic
  :: forall t k1 (k :: k1 -> Type) (v :: k1 -> Type) (v' :: k1 -> Type) (p :: (k1 -> Type) -> (k1 -> Type) -> Type) es localEs
   . ( t ~ StaticDomTimeline
     , GCompare k
     , Reader (StaticDomBuilderEnv t) :> es
     , State [R.Behavior t Builder] :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , IOE :> es
     , PatchTarget (p k (Constant (R.Behavior t Builder)))
         ~ DMap k (Constant (R.Behavior t Builder))
     , Patch (p k (Constant (R.Behavior t Builder)))
     )
  => (forall c. Eff localEs c -> Eff es (c, R.Behavior t Builder))
  -> ( forall (vv :: k1 -> Type) (vv' :: k1 -> Type)
       . GCompare k
       => (forall (x :: k1). k x -> vv x -> Eff es (vv' x))
       -> DMap k vv
       -> R.Event t (p k vv)
       -> Eff es (DMap k vv', R.Event t (p k vv'))
     )
  -> (forall (vv :: k1 -> Type) (vv' :: k1 -> Type). (forall (x :: k1). vv x -> vv' x) -> p k vv -> p k vv')
  -> (forall (x :: k1). k x -> v x -> Eff localEs (v' x))
  -> DMap k v
  -> R.Event t (p k v)
  -> Eff es (DMap k v', R.Event t (p k v'))
hoistDMapWithKeyWithAdjustStatic collect base mapPatch f dm0 dm' = do
  (children0, children') <- base
    (\k v -> fmap (Compose . swapTuple) (collect (f k v)))
    dm0 dm'
  let result0 = DMap.map (P.snd . getCompose) children0
      result' = R.ffor children' $ mapPatch (P.snd . getCompose)
      outputs0 :: DMap k (Constant (R.Behavior t Builder))
      outputs0 = DMap.map (Constant . P.fst . getCompose) children0
      outputs' :: R.Event t (p k (Constant (R.Behavior t Builder)))
      outputs' = R.ffor children' $ mapPatch (Constant . P.fst . getCompose)
  outputs <- R.holdIncremental outputs0 outputs'
  modify @[R.Behavior t Builder] $ (:) $ R.pull $ do
    os <- R.sample (R.currentIncremental outputs)
    fmap mconcat $ forM (DMap.toList os) $ \(_ :=> Constant o) ->
      R.sample o
  pure (result0, result')

-- | Local swap helper (avoids adding a Data.Tuple import for one use).
swapTuple :: (a, b) -> (b, a)
swapTuple (a, b) = (b, a)

-- | Run a child widget in a fresh byte-accumulator scope, returning
-- @(childResult, childBytes)@ where @childBytes = mconcat (reverse
-- childAcc)@ — same shape upstream's @runStaticDomBuilderT@ returns.
--
-- Unlike 'withChildBuilderStatic', does NOT modify the reader env:
-- the child inherits the parent's escape rule and select context
-- unchanged, matching upstream's @flip runStaticDomBuilderT e@.
runChildBytesStatic
  :: forall t es a
   . ( State [R.Behavior t Builder] :> es
     , R.Reflex t
     )
  => Eff es a
  -> Eff es (a, R.Behavior t Builder)
runChildBytesStatic child = do
  outerAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] []
  result <- child
  childAcc <- get @[R.Behavior t Builder]
  put @[R.Behavior t Builder] outerAcc
  pure (result, mconcat (P.reverse childAcc))

-- =====================================================================
-- Static HasPrerender instance
--
-- Server body emits @<!--prerender/start-->@, runs the server widget,
-- emits @<!--prerender/end-->@. Client body is DISCARDED into the
-- 'UnrunnableEs' trap row (matching upstream reflex-dom's
-- @UnrunnableT@ shape). Result 'Dynamic' is a constant of the
-- server-side value.
-- =====================================================================

instance
  ( DomBuilderE t StaticDomSpace :> serverEs
  , DomBuilderSpaceE serverEs ~ StaticDomSpace
  , R.Reflex t
  ) => HasPrerender t StaticDomSpace serverEs where
  type ClientEs t StaticDomSpace serverEs = UnrunnableEs t
  prerender server _client = do
    _ <- send @(DomBuilderE t StaticDomSpace)
              (DB_CommentNode (CommentNodeConfig startMarker Nothing))
    a <- server
    _ <- send @(DomBuilderE t StaticDomSpace)
              (DB_CommentNode (CommentNodeConfig endMarker Nothing))
    pure (R.constDyn a)

-- =====================================================================
-- Bridge: upstream 'Reflex.Dom.Prerender.Prerender' dispatches to
-- 'HasPrerender'.
--
-- One instance. Selects on the widget-monad shape
-- @WithJSContextSingleton x (Eff serverEs)@ (which every port widget
-- uses). Resolves 'HasPrerender' via 'DomBuilderSpaceE serverEs' — the
-- row's DOM space — so the three concrete 'HasPrerender' instances
-- (Ghcjs / Static / Hydration) fire on the right rows automatically.
--
-- @Client m@ = @WithJSContextSingleton x (Eff (ClientEs t space serverEs))@
-- — the same wrapper around 'ClientEs' from 'HasPrerender'. This means
-- variant-B widget code written against upstream 'Prerender' composes
-- freely on our port with no source-level change.
-- =====================================================================

instance
  ( t ~ DomBuilderTimelineE serverEs
  , DomBuilderTimelineE (ClientEs t (DomBuilderSpaceE serverEs) serverEs) ~ t
  , HasPrerender t (DomBuilderSpaceE serverEs) serverEs
  , PrerenderClientConstraintE t (ClientEs t (DomBuilderSpaceE serverEs) serverEs)
  , RDPR.PrerenderClientConstraint t
      (WithJSContextSingleton x
        (Eff (ClientEs t (DomBuilderSpaceE serverEs) serverEs)))
  ) => RDPR.Prerender t (WithJSContextSingleton x (Eff serverEs)) where
  type Client (WithJSContextSingleton x (Eff serverEs)) =
    WithJSContextSingleton x
      (Eff (ClientEs (DomBuilderTimelineE serverEs)
                     (DomBuilderSpaceE serverEs)
                     serverEs))
  prerender server client = WithJSContextSingleton $ ReaderT $ \ctx ->
    let serverEff = runWithJSContextSingleton server ctx
        clientEff = runWithJSContextSingleton client ctx
    in prerender @(DomBuilderTimelineE serverEs) @(DomBuilderSpaceE serverEs) serverEff clientEff

-- ---------------------------------------------------------------------
-- UnrunnableEff — Eff-side analog of upstream 'UnrunnableT'
-- (reflex-dom-core Prerender.hs:134-206).
--
-- Type-routing newtype used ONLY as the 'Client' of the MTL
-- 'Reflex.Dom.Prerender.Prerender' instance on 'Eff serverEs' below.
-- Wraps @ReaderT Void IO@ so 'unrunnableEff' is a *genuine*
-- impossibility (empty case match, not 'error'). Every MTL superclass
-- instance below has an 'unrunnableEff' body — safe because the client
-- branch of MTL Prerender in a static context is discarded by the
-- 'RDPR.Prerender t (Eff serverEs)' instance below (@_client@ pattern),
-- so no code path invokes these methods at runtime.
--
-- Not exported; only used to satisfy the @Client m@ associated-type
-- and @Client (Client m) ~ Client m@ constraints in the MTL 'Prerender'
-- class chain.
-- ---------------------------------------------------------------------

newtype UnrunnableEff t a = UnrunnableEff (ReaderT Void IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix)

unrunnableEff :: UnrunnableEff t a
unrunnableEff = UnrunnableEff $ ReaderT $ \case {}

instance RH.MonadReflexCreateTrigger t (UnrunnableEff t) where
  newEventWithTrigger _    = unrunnableEff
  newFanEventWithTrigger _ = unrunnableEff

instance R.Reflex t => R.MonadSample t (UnrunnableEff t) where
  sample _ = unrunnableEff

instance R.Reflex t => R.MonadHold t (UnrunnableEff t) where
  hold _ _            = unrunnableEff
  holdDyn _ _         = unrunnableEff
  holdIncremental _ _ = unrunnableEff
  buildDynamic _ _    = unrunnableEff
  headE _             = unrunnableEff
  now                 = unrunnableEff

instance MRef.MonadRef (UnrunnableEff t) where
  type Ref (UnrunnableEff t) = IORef
  newRef _     = unrunnableEff
  readRef _    = unrunnableEff
  writeRef _ _ = unrunnableEff

instance MRef.MonadAtomicRef (UnrunnableEff t) where
  atomicModifyRef _ _ = unrunnableEff

instance RDBC.HasDocument (UnrunnableEff t) where
  askDocument = unrunnableEff

instance RTE.TriggerEvent t (UnrunnableEff t) where
  newTriggerEvent                         = unrunnableEff
  newTriggerEventWithOnComplete           = unrunnableEff
  newEventWithLazyTriggerWithOnComplete _ = unrunnableEff

instance R.Reflex t => RPB.PostBuild t (UnrunnableEff t) where
  getPostBuild = unrunnableEff

instance PrimMonad (UnrunnableEff t) where
  type PrimState (UnrunnableEff t) = PrimState IO
  primitive _ = unrunnableEff

instance JSaddleTypes.MonadJSM (UnrunnableEff t) where
  liftJSM' _ = unrunnableEff

instance R.Reflex t => RDBC.DomBuilder t (UnrunnableEff t) where
  type DomBuilderSpace (UnrunnableEff t) = GhcjsDomSpace
  textNode _         = unrunnableEff
  commentNode _      = unrunnableEff
  element _ _ _      = unrunnableEff
  inputElement _     = unrunnableEff
  textAreaElement _  = unrunnableEff
  selectElement _ _  = unrunnableEff
  placeRawElement _  = unrunnableEff
  wrapRawElement _ _ = unrunnableEff

instance R.Reflex t => RNR.NotReady t (UnrunnableEff t) where
  notReadyUntil _ = unrunnableEff
  notReady        = unrunnableEff

instance R.Reflex t => RA.Adjustable t (UnrunnableEff t) where
  runWithReplace _ _                          = unrunnableEff
  traverseIntMapWithKeyWithAdjust _ _ _       = unrunnableEff
  traverseDMapWithKeyWithAdjust _ _ _         = unrunnableEff
  traverseDMapWithKeyWithAdjustWithMove _ _ _ = unrunnableEff

instance R.Reflex t => RPE.PerformEvent t (UnrunnableEff t) where
  type Performable (UnrunnableEff t) = UnrunnableEff t
  performEvent _  = unrunnableEff
  performEvent_ _ = unrunnableEff

instance R.Reflex t => RDBC.DomRenderHook t (UnrunnableEff t) where
  withRenderHook _ _  = unrunnableEff
  requestDomAction _  = unrunnableEff
  requestDomAction_ _ = unrunnableEff

instance R.Reflex t => RDPR.Prerender t (UnrunnableEff t) where
  type Client (UnrunnableEff t) = UnrunnableEff t
  prerender _ _ = unrunnableEff

-- | 'Prerender' on plain 'Eff' — used by widgets driven directly by
-- 'renderStatic' (no JSaddle context). Pinned to 'StaticDomSpace' since
-- @Prerender t (Eff serverEs)@ on the Eff-only path is inherently a
-- static concept in this port (hydration\/ghcjs paths use 'WidgetEff'
-- which has its own Prerender instance). The client branch is
-- 'UnrunnableEff t' — genuinely impossible, matching upstream MTL
-- Static's use of @UnrunnableT t m@.
instance
  ( t ~ DomBuilderTimelineE serverEs
  , DomBuilderSpaceE serverEs ~ StaticDomSpace
  , DomBuilderE t StaticDomSpace :> serverEs
  , R.Reflex t
  , RDPR.PrerenderClientConstraint t (UnrunnableEff t)
  ) => RDPR.Prerender t (Eff serverEs) where
  type Client (Eff serverEs) = UnrunnableEff (DomBuilderTimelineE serverEs)
  prerender server _client = do
    _ <- send @(DomBuilderE t StaticDomSpace)
              (DB_CommentNode (RDBC.CommentNodeConfig startMarker Nothing))
    a <- server
    _ <- send @(DomBuilderE t StaticDomSpace)
              (DB_CommentNode (RDBC.CommentNodeConfig endMarker Nothing))
    pure (R.constDyn a)

-- =====================================================================
-- Widget vocabulary: Widget/Basic + Widget/Input surface.
--
-- Ports:
--   * upstream 'Reflex.Dom.Widget.Basic' (dynText / dynamicAttributes* /
--     elDynAttr(NS)(') / elDynClass(') / Link / link / linkClass /
--     divClass / dtdd / blank / (=:)).
--   * upstream 'Reflex.Dom.Widget.Input' (TextInput / TextArea /
--     RangeInput / Checkbox / FileInput plus their config records + the
--     six 'HasAttributes' / 'HasSetValue' / 'HasValue' class instances
--     used to feed values back through @elDynAttr@-style plumbing).
--
-- NOT ported here (scope for a follow-up wave — each depends on the
-- 'Adjustable'-widget-scope + Network module which aren't yet ported):
--   dyn / dyn_ / widgetHold / widgetHold_ / networkView / networkHold
--   / listWithKey / listViewWithKey / listHoldWithKey / tableDynAttr /
--   tabDisplay / dropdown / checkboxView.
--
-- Users writing forms today can use textInput / textArea / rangeInput /
-- checkbox / fileInput plus the static widget vocabulary (text / el /
-- elAttr / elClass / elDynAttr).

-- ---------------------------------------------------------------------
-- HasAttributes class (from Widget/Basic L376) + HasValue class (from
-- Widget/Input L682)
-- ---------------------------------------------------------------------

class HasAttributes a where
  type Attrs a :: Type
  attributes :: Lens' a (Attrs a)

class HasValue a where
  type Value a :: Type
  value :: a -> Value a

instance HasValue (InputElement er d t) where
  type Value (InputElement er d t) = R.Dynamic t Text
  value = _inputElement_value

instance HasValue (TextAreaElement er d t) where
  type Value (TextAreaElement er d t) = R.Dynamic t Text
  value = _textAreaElement_value

-- ---------------------------------------------------------------------
-- Convenience operator + tiny helpers
-- ---------------------------------------------------------------------

-- | @k =: a@ = @Map.singleton k a@. Convenience for building small
-- attribute maps. Fixity binds tighter than @<>@.
(=:) :: k -> v -> Map k v
k =: a = Map.singleton k a
infixr 7 =:

-- | @blank = 'pure' ()@. Handy at end of do-blocks.
blank :: Applicative m => m ()
blank = pure ()

-- ---------------------------------------------------------------------
-- Widget/Basic ports
-- ---------------------------------------------------------------------

-- | Render a text node whose contents track a 'R.Dynamic'. The
-- timeline is derived from the effect row via 'DomBuilderTimelineE'.
dynText
  :: forall es
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , HH.PostBuild (DomBuilderTimelineE es) :> es
     , HH.NotReady (DomBuilderTimelineE es) :> es
     )
  => R.Dynamic (DomBuilderTimelineE es) Text -> Eff es ()
dynText dt = do
  postBuild <- HH.getPostBuildE @(DomBuilderTimelineE es)
  void $ textNode @(DomBuilderTimelineE es) $
    (def :: TextNodeConfig (DomBuilderTimelineE es))
      & textNodeConfig_setContents .~ R.leftmost
          [ R.updated dt
          , R.tag (R.current dt) postBuild
          ]
  HH.notReadyUntilE @(DomBuilderTimelineE es) postBuild
{-# INLINABLE dynText #-}

-- | Render a value that changes over time as text (via 'Show').
display
  :: forall es a
   . ( DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , R.Reflex (DomBuilderTimelineE es)
     , HH.PostBuild (DomBuilderTimelineE es) :> es
     , HH.NotReady (DomBuilderTimelineE es) :> es
     , P.Show a
     )
  => R.Dynamic (DomBuilderTimelineE es) a -> Eff es ()
display = dynText . fmap (T.pack . P.show)
{-# INLINABLE display #-}

-- | Rebuild the sub-network whenever the 'Dynamic' updates. The
-- returned 'Event' fires at post-build and each 'Dynamic' update; its
-- value is the widget's result.
--
-- Delegates to 'RNet.networkView' from the reflex vendored library
-- (which discovers the 'Adjustable' + 'NotReady' + 'PostBuild' MTL
-- instances on @Eff es@ automatically).
dyn
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => R.Dynamic t (Eff es a) -> Eff es (R.Event t a)
dyn = RNet.networkView
{-# INLINABLE dyn #-}

-- | 'dyn' with the returned 'Event' discarded.
dyn_
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => R.Dynamic t (Eff es a) -> Eff es ()
dyn_ = void . dyn @t
{-# INLINABLE dyn_ #-}

-- | Given an initial widget and an 'Event' of widget-creating actions,
-- create a widget that is recreated whenever the 'Event' fires. Result
-- 'Dynamic' fires when the 'Event' does.
widgetHold
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     )
  => Eff es a -> R.Event t (Eff es a) -> Eff es (R.Dynamic t a)
widgetHold = RNet.networkHold
{-# INLINABLE widgetHold #-}

-- | 'widgetHold' with the returned 'Dynamic' discarded.
widgetHold_
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     )
  => Eff es a -> R.Event t (Eff es a) -> Eff es ()
widgetHold_ z = void . widgetHold @t z
{-# INLINABLE widgetHold_ #-}

-- | Render a placeholder network to be shown while another network is
-- not yet done building.
untilReady
  :: forall t es a b
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     )
  => Eff es a -> Eff es b -> Eff es (a, R.Event t b)
untilReady = RNet.untilReady
{-# INLINABLE untilReady #-}

-- | Alias for 'RNet.networkView', re-exported for callers who prefer
-- upstream reflex-dom's naming.
networkView
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => R.Dynamic t (Eff es a) -> Eff es (R.Event t a)
networkView = RNet.networkView
{-# INLINABLE networkView #-}

-- | Alias for 'RNet.networkHold'.
networkHold
  :: forall t es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     )
  => Eff es a -> R.Event t (Eff es a) -> Eff es (R.Dynamic t a)
networkHold = RNet.networkHold
{-# INLINABLE networkHold #-}

-- | Convert a 'Dynamic' attribute map into an @Event t (Map k (Maybe
-- Text))@ that emits a modification whenever the map changes.
-- Starts by diffing from the given seed (defaults to empty via
-- 'dynamicAttributesToModifyAttributes').
dynamicAttributesToModifyAttributesWithInitial
  :: forall t k es
   . (Ord k, R.Reflex t, HH.PostBuild t :> es)
  => Map k Text -> R.Dynamic t (Map k Text)
  -> Eff es (R.Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributesWithInitial attrs0 d = do
  postBuild <- HH.getPostBuildE @t
  let modificationsNeeded = R.push
        (\x -> do
            p <- case x of
              This ()      -> do
                new <- R.sample (R.current d)
                pure $ Map.Misc.diffMap attrs0 new
              These () new -> pure $ Map.Misc.diffMap attrs0 new
              That new     -> do
                old <- R.sample (R.current d)
                pure $ Map.Misc.diffMap old new
            pure $ if Map.null p then Nothing else Just p)
        (Align.align postBuild (R.updated d))
  pure modificationsNeeded

-- | 'dynamicAttributesToModifyAttributesWithInitial' seeded with an
-- empty initial attribute map.
dynamicAttributesToModifyAttributes
  :: forall t k es
   . (Ord k, R.Reflex t, HH.PostBuild t :> es)
  => R.Dynamic t (Map k Text) -> Eff es (R.Event t (Map k (Maybe Text)))
dynamicAttributesToModifyAttributes =
  dynamicAttributesToModifyAttributesWithInitial mempty

-- | Create a DOM element with 'Dynamic' attributes and return the
-- built 'Element' alongside the child result.
elDynAttrNS'
  :: forall t es a
   . ( R.Reflex t
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => Maybe Namespace
  -> Text
  -> R.Dynamic t (Map Text Text)
  -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpaceE es) t, a)
elDynAttrNS' mns elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes @t attrs
  let cfg = def
        & elementConfig_namespace  .~ mns
        & modifyAttributes @t      .~ R.fmapCheap mapKeysToAttributeName modifyAttrs
  result <- element @t elementTag cfg child
  postBuild <- HH.getPostBuildE @t
  HH.notReadyUntilE @t postBuild
  pure result
{-# INLINABLE elDynAttrNS' #-}

-- | Namespace-less variant.
elDynAttr'
  :: forall t es a
   . ( R.Reflex t
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => Text -> R.Dynamic t (Map Text Text)
  -> Eff es a
  -> Eff es (Element EventResult (DomBuilderSpaceE es) t, a)
elDynAttr' = elDynAttrNS' @t Nothing
{-# INLINABLE elDynAttr' #-}

-- | Create a DOM element with 'Dynamic' attributes; discard the
-- returned 'Element'.
elDynAttr
  :: forall t es a
   . ( R.Reflex t
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => Text -> R.Dynamic t (Map Text Text) -> Eff es a -> Eff es a
elDynAttr elementTag attrs child =
  P.snd <$> elDynAttr' @t elementTag attrs child
{-# INLINABLE elDynAttr #-}

-- | Create a DOM element with a 'Dynamic' class attribute.
elDynClass
  :: forall t es a
   . ( R.Reflex t
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => Text -> R.Dynamic t Text -> Eff es a -> Eff es a
elDynClass elementTag c =
  elDynAttr @t elementTag (fmap ("class" =:) c)
{-# INLINABLE elDynClass #-}

-- | Create a DOM element with a 'Dynamic' namespace + attributes;
-- discard the returned 'Element'.
elDynAttrNS
  :: forall t es a
   . ( R.Reflex t
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     )
  => Maybe Namespace -> Text
  -> R.Dynamic t (Map Text Text) -> Eff es a -> Eff es a
elDynAttrNS mns elementTag attrs child =
  P.snd <$> elDynAttrNS' @t mns elementTag attrs child
{-# INLINABLE elDynAttrNS #-}

-- | Anchor tag wrapper — click event as 'Event t ()'.
newtype Link t = Link { _link_clicked :: R.Event t () }

linkClass
  :: forall es
   . ( R.Reflex (DomBuilderTimelineE es)
     , DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Text -> Eff es (Link (DomBuilderTimelineE es))
linkClass s c = do
  (l, _) <- elAttr' "a" ("class" =: c) $ text s
  pure $ Link (domEvent Click l)
{-# INLINABLE linkClass #-}

link
  :: forall es
   . ( R.Reflex (DomBuilderTimelineE es)
     , DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Eff es (Link (DomBuilderTimelineE es))
link s = linkClass s ""
{-# INLINABLE link #-}

-- | @div class="…"@ shortcut.
divClass
  :: forall es a
   . ( R.Reflex (DomBuilderTimelineE es)
     , DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Eff es a -> Eff es a
divClass = elClass "div"
{-# INLINABLE divClass #-}

-- | @<dt>header</dt><dd>widget</dd>@ pair.
dtdd
  :: forall es a
   . ( R.Reflex (DomBuilderTimelineE es)
     , DomBuilderE (DomBuilderTimelineE es) (DomBuilderSpaceE es) :> es
     , DomSpace (DomBuilderSpaceE es)
     )
  => Text -> Eff es a -> Eff es a
dtdd h w = do
  el "dt" $ text h
  el "dd" w
{-# INLINABLE dtdd #-}

-- =====================================================================
-- Widget/Input port
-- =====================================================================

-- ---------------------------------------------------------------------
-- keycode compat + deprecated helpers
-- ---------------------------------------------------------------------

-- | Deprecated: use @'WebKC.keyCodeLookup' x == WebKC.Enter@.
keycodeEnter :: Int
keycodeEnter = 13

-- | Deprecated: use @'WebKC.keyCodeLookup' x == WebKC.Escape@.
keycodeEscape :: Int
keycodeEscape = 27

-- ---------------------------------------------------------------------
-- TextInput
-- ---------------------------------------------------------------------

-- | Text-input result bundle. Kept for source-parity with upstream
-- 'Reflex.Dom.Widget.Input.TextInput'; new code is encouraged to use
-- 'inputElement' directly.
data TextInput t = TextInput
  { _textInput_value          :: R.Dynamic t Text
  , _textInput_input          :: R.Event t Text
  , _textInput_keypress       :: R.Event t Word
  , _textInput_keydown        :: R.Event t Word
  , _textInput_keyup          :: R.Event t Word
  , _textInput_hasFocus       :: R.Dynamic t Bool
  , _textInput_builderElement :: InputElement EventResult GhcjsDomSpace t
  }

_textInput_element :: TextInput t -> HTMLInputElement
_textInput_element = _inputElement_raw . _textInput_builderElement

instance R.Reflex t => HasDomEvent t (TextInput t) en where
  type DomEventType (TextInput t) en =
    DomEventType (InputElement EventResult GhcjsDomSpace t) en
  domEvent en = domEvent en . _textInput_builderElement

data TextInputConfig t = TextInputConfig
  { _textInputConfig_inputType    :: Text
  , _textInputConfig_initialValue :: Text
  , _textInputConfig_setValue     :: R.Event t Text
  , _textInputConfig_attributes   :: R.Dynamic t (Map Text Text)
  }

instance R.Reflex t => Default (TextInputConfig t) where
  def = TextInputConfig
    { _textInputConfig_inputType    = "text"
    , _textInputConfig_initialValue = ""
    , _textInputConfig_setValue     = R.never
    , _textInputConfig_attributes   = R.constDyn mempty
    }

-- | Create an input whose value is a 'Text'. By default @type="text"@;
-- set 'textInputConfig_inputType' to change.
textInput
  :: forall t es
   . ( R.Reflex t
     , DomBuilderTimelineE es ~ t
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderE t GhcjsDomSpace :> es
     , HH.PostBuild t :> es
     )
  => TextInputConfig t -> Eff es (TextInput t)
textInput (TextInputConfig inputType initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes @t
    (fmap (Map.insert "type" inputType) dAttrs)
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ initial
    & inputElementConfig_setValue     .~ eSetValue
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ fmap mapKeysToAttributeName modifyAttrs
  pure TextInput
    { _textInput_value          = _inputElement_value i
    , _textInput_input          = _inputElement_input i
    , _textInput_keypress       = domEvent Keypress i
    , _textInput_keydown        = domEvent Keydown i
    , _textInput_keyup          = domEvent Keyup i
    , _textInput_hasFocus       = _inputElement_hasFocus i
    , _textInput_builderElement = i
    }
{-# INLINABLE textInput #-}

-- | Deprecated helper (use @'keypress' 'WebKC.Enter'@ directly).
textInputGetEnter :: R.Reflex t => TextInput t -> R.Event t ()
textInputGetEnter = keypress WebKC.Enter
{-# INLINE textInputGetEnter #-}

-- | Filter a keypress-style event stream to only fire when the pressed
-- key matches. Works on any element type that carries a 'Word'-shaped
-- keycode event via 'HasDomEvent'.
keypress
  :: (R.Reflex t, HasDomEvent t e 'KeypressTag, DomEventType e 'KeypressTag ~ Word)
  => WebKC.Key -> e -> R.Event t ()
keypress key = R.fmapMaybe
  (\n -> Monad.guard $ WebKC.keyCodeLookup (fromIntegral n) == key)
  . domEvent Keypress
{-# INLINABLE keypress #-}

-- | 'keydown' variant of 'keypress'.
keydown
  :: (R.Reflex t, HasDomEvent t e 'KeydownTag, DomEventType e 'KeydownTag ~ Word)
  => WebKC.Key -> e -> R.Event t ()
keydown key = R.fmapMaybe
  (\n -> Monad.guard $ WebKC.keyCodeLookup (fromIntegral n) == key)
  . domEvent Keydown
{-# INLINABLE keydown #-}

-- | 'keyup' variant of 'keypress'.
keyup
  :: (R.Reflex t, HasDomEvent t e 'KeyupTag, DomEventType e 'KeyupTag ~ Word)
  => WebKC.Key -> e -> R.Event t ()
keyup key = R.fmapMaybe
  (\n -> Monad.guard $ WebKC.keyCodeLookup (fromIntegral n) == key)
  . domEvent Keyup
{-# INLINABLE keyup #-}

-- ---------------------------------------------------------------------
-- RangeInput
-- ---------------------------------------------------------------------

data RangeInputConfig t = RangeInputConfig
  { _rangeInputConfig_initialValue :: Float
  , _rangeInputConfig_setValue     :: R.Event t Float
  , _rangeInputConfig_attributes   :: R.Dynamic t (Map Text Text)
  }

instance R.Reflex t => Default (RangeInputConfig t) where
  def = RangeInputConfig
    { _rangeInputConfig_initialValue = 0
    , _rangeInputConfig_setValue     = R.never
    , _rangeInputConfig_attributes   = R.constDyn mempty
    }

data RangeInput t = RangeInput
  { _rangeInput_value    :: R.Dynamic t Float
  , _rangeInput_input    :: R.Event t Float
  , _rangeInput_mouseup  :: R.Event t (Int, Int)
  , _rangeInput_hasFocus :: R.Dynamic t Bool
  , _rangeInput_element  :: HTMLInputElement
  }

-- | Create an @<input type="range">@ whose value is a 'Float'.
rangeInput
  :: forall t es
   . ( R.Reflex t
     , DomBuilderTimelineE es ~ t
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderE t GhcjsDomSpace :> es
     , HH.PostBuild t :> es
     )
  => RangeInputConfig t -> Eff es (RangeInput t)
rangeInput (RangeInputConfig initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes @t
    (fmap (Map.insert "type" "range") dAttrs)
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ T.pack (P.show initial)
    & inputElementConfig_setValue     .~ (T.pack . P.show <$> eSetValue)
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ fmap mapKeysToAttributeName modifyAttrs
  pure RangeInput
    { _rangeInput_value    = fmap (P.read . T.unpack) (_inputElement_value i)
    , _rangeInput_input    = fmap (P.read . T.unpack) (_inputElement_input i)
    , _rangeInput_mouseup  = domEvent Mouseup i
    , _rangeInput_hasFocus = _inputElement_hasFocus i
    , _rangeInput_element  = _inputElement_raw i
    }
{-# INLINABLE rangeInput #-}

-- ---------------------------------------------------------------------
-- TextArea
-- ---------------------------------------------------------------------

data TextAreaConfig t = TextAreaConfig
  { _textAreaConfig_initialValue :: Text
  , _textAreaConfig_setValue     :: R.Event t Text
  , _textAreaConfig_attributes   :: R.Dynamic t (Map Text Text)
  }

instance R.Reflex t => Default (TextAreaConfig t) where
  def = TextAreaConfig
    { _textAreaConfig_initialValue = ""
    , _textAreaConfig_setValue     = R.never
    , _textAreaConfig_attributes   = R.constDyn mempty
    }

data TextArea t = TextArea
  { _textArea_value    :: R.Dynamic t Text
  , _textArea_input    :: R.Event t Text
  , _textArea_hasFocus :: R.Dynamic t Bool
  , _textArea_keypress :: R.Event t Word
  , _textArea_element  :: HTMLTextAreaElement
  }

-- | Create a @<textarea>@ element.
textArea
  :: forall t es
   . ( R.Reflex t
     , DomBuilderTimelineE es ~ t
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderE t GhcjsDomSpace :> es
     , HH.PostBuild t :> es
     )
  => TextAreaConfig t -> Eff es (TextArea t)
textArea (TextAreaConfig initial eSet attrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes @t attrs
  i <- textAreaElement $ def
    & textAreaElementConfig_initialValue .~ initial
    & textAreaElementConfig_setValue     .~ eSet
    & textAreaElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ fmap mapKeysToAttributeName modifyAttrs
  pure TextArea
    { _textArea_value    = _textAreaElement_value i
    , _textArea_input    = _textAreaElement_input i
    , _textArea_keypress = domEvent Keypress i
    , _textArea_hasFocus = _textAreaElement_hasFocus i
    , _textArea_element  = _textAreaElement_raw i
    }
{-# INLINABLE textArea #-}

-- ---------------------------------------------------------------------
-- Checkbox
-- ---------------------------------------------------------------------

data CheckboxConfig t = CheckboxConfig
  { _checkboxConfig_setValue   :: R.Event t Bool
  , _checkboxConfig_attributes :: R.Dynamic t (Map Text Text)
  }

instance R.Reflex t => Default (CheckboxConfig t) where
  def = CheckboxConfig
    { _checkboxConfig_setValue   = R.never
    , _checkboxConfig_attributes = R.constDyn mempty
    }

data Checkbox t = Checkbox
  { _checkbox_value  :: R.Dynamic t Bool
  , _checkbox_change :: R.Event t Bool
  }

-- | Create an editable checkbox. Any @"type"@ or @"checked"@ attribute
-- passed in via 'CheckboxConfig' is stripped — they're always overridden
-- to @type="checkbox"@ and driven by 'initialChecked' / 'setValue'.
checkbox
  :: forall t es
   . ( R.Reflex t
     , DomBuilderTimelineE es ~ t
     , DomSpace (DomBuilderSpaceE es)
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , HH.PostBuild t :> es
     )
  => Bool -> CheckboxConfig t -> Eff es (Checkbox t)
checkbox checked config = do
  let permanentAttrs = "type" =: "checkbox"
      dAttrs        = Map.delete "checked" . Map.union permanentAttrs
                    <$> _checkboxConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes @t dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked     .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_initialAttributes
        .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes
        .~ fmap mapKeysToAttributeName modifyAttrs
  pure Checkbox
    { _checkbox_value  = _inputElement_checked i
    , _checkbox_change = _inputElement_checkedChange i
    }
{-# INLINABLE checkbox #-}

-- ---------------------------------------------------------------------
-- FileInput
-- ---------------------------------------------------------------------

data FileInput d t = FileInput
  { _fileInput_value   :: R.Dynamic t [DOM.File]
  , _fileInput_element :: RawInputElement d
  }

newtype FileInputConfig t = FileInputConfig
  { _fileInputConfig_attributes :: R.Dynamic t (Map Text Text) }

instance R.Reflex t => Default (FileInputConfig t) where
  def = FileInputConfig { _fileInputConfig_attributes = R.constDyn mempty }

-- | Create a file-picker input.
fileInput
  :: forall t es
   . ( R.Reflex t
     , DomBuilderTimelineE es ~ t
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderE t GhcjsDomSpace :> es
     , HH.PostBuild t :> es
     )
  => FileInputConfig t -> Eff es (FileInput GhcjsDomSpace t)
fileInput config = do
  let dAttrs = Map.insert "type" "file" <$> _fileInputConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes @t dAttrs
  let cfg = (def :: InputElementConfig EventResult t GhcjsDomSpace)
        & inputElementConfig_elementConfig . elementConfig_modifyAttributes
            .~ fmap mapKeysToAttributeName modifyAttrs
  i <- inputElement cfg
  pure FileInput
    { _fileInput_value   = _inputElement_files i
    , _fileInput_element = _inputElement_raw i
    }
{-# INLINABLE fileInput #-}

-- ---------------------------------------------------------------------
-- HasValue instances for the widget-result records
-- ---------------------------------------------------------------------

instance HasValue (TextArea t) where
  type Value (TextArea t) = R.Dynamic t Text
  value = _textArea_value

instance HasValue (TextInput t) where
  type Value (TextInput t) = R.Dynamic t Text
  value = _textInput_value

instance HasValue (RangeInput t) where
  type Value (RangeInput t) = R.Dynamic t Float
  value = _rangeInput_value

instance HasValue (FileInput d t) where
  type Value (FileInput d t) = R.Dynamic t [DOM.File]
  value = _fileInput_value

instance HasValue (Checkbox t) where
  type Value (Checkbox t) = R.Dynamic t Bool
  value = _checkbox_value

-- =====================================================================
-- Collection wrappers (delegate to Reflex.Collection).
--
-- All these run via eff-reflex's MTL-shape @Adjustable t (Eff es) +
-- MonadHold t (Eff es) + PostBuild t (Eff es)@ instances, so the port
-- is a thin wrapper set — no per-arm reimplementation.
-- =====================================================================

-- | Create a set of widgets based on the provided 'Map'. When the
-- input 'Event' fires, remove widgets for keys with the value 'Nothing'
-- and add/replace widgets for keys with 'Just' values.
listHoldWithKey
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => Map k v
  -> R.Event t (Map k (Maybe v))
  -> (k -> v -> Eff es a)
  -> Eff es (R.Dynamic t (Map k a))
listHoldWithKey = RColl.listHoldWithKey
{-# INLINABLE listHoldWithKey #-}

-- | Create a set of widgets from a 'Dynamic' Map. Efficiently
-- add/remove/update children as the map changes rather than rebuilding
-- the whole set.
listWithKey
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , MonadFix (Eff es)
     , HH.Hold t :> es
     , HH.Sample t :> es
     )
  => R.Dynamic t (Map k v)
  -> (k -> R.Dynamic t v -> Eff es a)
  -> Eff es (R.Dynamic t (Map k a))
listWithKey = RColl.listWithKey
{-# INLINABLE listWithKey #-}

-- | 'listWithKey' variant that gets an 'Event' of child-value updates
-- instead of a 'Dynamic'.
listWithKeyShallowDiff
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , MonadFix (Eff es)
     , HH.Hold t :> es
     , HH.Sample t :> es
     )
  => Map k v
  -> R.Event t (Map k (Maybe v))
  -> (k -> v -> R.Event t v -> Eff es a)
  -> Eff es (R.Dynamic t (Map k a))
listWithKeyShallowDiff = RColl.listWithKeyShallowDiff
{-# INLINABLE listWithKeyShallowDiff #-}

-- | Like 'listWithKey' but flattens each child's 'Event' result into a
-- single 'Event' of the map.
listViewWithKey
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => R.Dynamic t (Map k v)
  -> (k -> R.Dynamic t v -> Eff es (R.Event t a))
  -> Eff es (R.Event t (Map k a))
listViewWithKey = RColl.listViewWithKey
{-# INLINABLE listViewWithKey #-}

-- | Selectable widget list: at any point, exactly one child is
-- "selected" (per the input 'Dynamic'). Each child receives a
-- 'Dynamic Bool' indicating whether it's the selected one.
selectViewListWithKey
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => R.Dynamic t k
  -> R.Dynamic t (Map k v)
  -> (k -> R.Dynamic t v -> R.Dynamic t Bool -> Eff es (R.Event t a))
  -> Eff es (R.Event t (k, a))
selectViewListWithKey = RColl.selectViewListWithKey
{-# INLINABLE selectViewListWithKey #-}

-- | 'selectViewListWithKey' variant that discards the per-child result.
selectViewListWithKey_
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => R.Dynamic t k
  -> R.Dynamic t (Map k v)
  -> (k -> R.Dynamic t v -> R.Dynamic t Bool -> Eff es (R.Event t a))
  -> Eff es (R.Event t k)
selectViewListWithKey_ = RColl.selectViewListWithKey_
{-# INLINABLE selectViewListWithKey_ #-}

-- | Like 'listWithKey' but the widget-maker doesn't see the key.
list
  :: forall t k v es a
   . ( Ord k
     , R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => R.Dynamic t (Map k v)
  -> (R.Dynamic t v -> Eff es a)
  -> Eff es (R.Dynamic t (Map k a))
list = RColl.list
{-# INLINABLE list #-}

-- | Render a dynamic list. Widgets are given the child value only —
-- keys are internal (positional 'Int').
simpleList
  :: forall t v es a
   . ( R.Reflex t
     , HH.Adjustable t :> es
     , HH.PostBuild t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , MonadFix (Eff es)
     )
  => R.Dynamic t [v]
  -> (R.Dynamic t v -> Eff es a)
  -> Eff es (R.Dynamic t [a])
simpleList = RColl.simpleList
{-# INLINABLE simpleList #-}

-- =====================================================================
-- Dropdown (Widget/Input L388-487) — now unblocked with listWithKey
-- available.
-- =====================================================================

data DropdownConfig t k = DropdownConfig
  { _dropdownConfig_setValue   :: R.Event t k
  , _dropdownConfig_attributes :: R.Dynamic t (Map Text Text)
  }

instance R.Reflex t => Default (DropdownConfig t k) where
  def = DropdownConfig
    { _dropdownConfig_setValue   = R.never
    , _dropdownConfig_attributes = R.constDyn mempty
    }

data Dropdown t k = Dropdown
  { _dropdown_value  :: R.Dynamic t k
  , _dropdown_change :: R.Event t k
  }

-- | Create a @<select>@ dropdown box.
--
-- The first argument is the initial value; if it isn't already in the
-- provided options 'Map', it's added with an empty text label. This
-- matches upstream 'Reflex.Dom.Widget.Input.dropdown' verbatim.
dropdown
  :: forall k t es
   . ( R.Reflex t
     , Ord k
     , P.Show k
     , Eq k
     , DomBuilderTimelineE es ~ t
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomSpace GhcjsDomSpace
     , DomBuilderE t GhcjsDomSpace :> es
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     , HH.Sample t :> es
     , HH.CreateTrigger t :> es
     , MonadFix (Eff es)
     )
  => k -> R.Dynamic t (Map k Text)
  -> DropdownConfig t k -> Eff es (Dropdown t k)
dropdown k0 options (DropdownConfig setK attrs) = do
  optionsWithAddedKeys <- fmap (R.zipDynWith Map.union options) $
    RD.foldDyn Map.union (k0 =: "") (fmap (=: "") setK)
  defaultKey <- R.holdDyn k0 setK
  let (indexedOptions, ixKeys) = RD.splitDynPure $ R.ffor optionsWithAddedKeys $ \os ->
        let xs = fmap (\(ix, (k, v)) -> ((ix, k), ((ix, k), v)))
                   (P.zip [0 :: Int ..] (Map.toList os))
        in (Map.fromList (P.map P.snd xs), Bimap.fromList (P.map P.fst xs))
  modifyAttrs <- dynamicAttributesToModifyAttributes @t attrs
  postBuild <- HH.getPostBuildE @t
  let setSelection = RD.attachPromptlyDynWithMaybe (P.flip Bimap.lookupR) ixKeys $
        R.leftmost [setK, k0 <$ postBuild]
  let cfg = def
        & selectElementConfig_elementConfig . elementConfig_modifyAttributes
            .~ fmap mapKeysToAttributeName modifyAttrs
        & selectElementConfig_setValue .~ fmap (T.pack . P.show) setSelection
  (eRaw, _) <- selectElement @t cfg $
    listWithKey indexedOptions $ \(ix, k) v -> do
      let optionAttrs = fmap
            (\dk -> "value" =: T.pack (P.show ix)
                 <> if dk P.== k then "selected" =: "selected" else mempty)
            defaultKey
      elDynAttr "option" optionAttrs $ dynText v
  let lookupSelected ks v = do
        key <- readTextMaybe v
        Bimap.lookup key ks
      readTextMaybe :: Text -> Maybe Int
      readTextMaybe = TR.readMaybe . T.unpack
  let eChange = RD.attachPromptlyDynWith lookupSelected ixKeys $ _selectElement_change eRaw
  let readKey keys mk = fromMaybe k0 $ do
        k <- mk
        Monad.guard (Bimap.memberR k keys)
        pure k
  dValue <- fmap (R.zipDynWith readKey ixKeys) $
    R.holdDyn (Just k0) (R.leftmost [eChange, fmap Just setK])
  pure $ Dropdown dValue (RD.attachPromptlyDynWith readKey ixKeys eChange)
{-# INLINABLE dropdown #-}

instance HasValue (Dropdown t k) where
  type Value (Dropdown t k) = R.Dynamic t k
  value = _dropdown_value

-- =====================================================================
-- tableDynAttr + tabDisplay (Widget/Basic L314-374). Now unblocked
-- with listWithKey landed + Demux available from Reflex.Dynamic.
-- =====================================================================

-- | Display a table with static columns and dynamic rows.
tableDynAttr
  :: forall t es r k v
   . ( R.Reflex t
     , Ord k
     , Eq r
     , DomBuilderTimelineE es ~ t
     , DomSpace (DomBuilderSpaceE es)
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , HH.Adjustable t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     , MonadFix (Eff es)
     )
  => Text                                          -- ^ Class on the outer @<table>@
  -> [(Text, k -> R.Dynamic t r -> Eff es v)]      -- ^ Columns (header, row → value → widget)
  -> R.Dynamic t (Map k r)                         -- ^ Row-key → row-value map
  -> (k -> Eff es (R.Dynamic t (Map Text Text)))   -- ^ Row-attr computation per key
  -> Eff es (R.Dynamic t (Map k (Element EventResult (DomBuilderSpaceE es) t, [v])))
tableDynAttr klass cols dRows rowAttrs =
  elAttr "div" (Map.singleton "style" "zoom: 1; overflow: auto; background: white;") $
    elAttr "table" (Map.singleton "class" klass) $ do
      el "thead" $ el "tr" $
        P.mapM_ (\(h, _) -> el "th" $ text h) cols
      el "tbody" $
        listWithKey dRows $ \k r -> do
          dAttrs <- rowAttrs k
          elDynAttr' "tr" dAttrs $ P.mapM (\x -> el "td" $ P.snd x k r) cols
{-# INLINABLE tableDynAttr #-}

-- | Build a tabbed view: only one child widget visible at a time.
tabDisplay
  :: forall t es k
   . ( R.Reflex t
     , Ord k
     , DomBuilderTimelineE es ~ t
     , DomSpace (DomBuilderSpaceE es)
     , DomBuilderE t (DomBuilderSpaceE es) :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , HH.NotReady t :> es
     , HH.PostBuild t :> es
     , MonadFix (Eff es)
     )
  => Text                          -- ^ Class on outer @<ul>@
  -> Text                          -- ^ Class on the active @<li>@
  -> Map k (Text, Eff es ())       -- ^ Key → (tab label, child widget)
  -> Eff es ()
tabDisplay ulClass activeClass tabItems = do
  let t0 = listToMaybe (Map.keys tabItems)
  rec currentTab :: RD.Demux t (Maybe k) <- elAttr "ul" ("class" =: ulClass) $ do
        tabClicksList :: [R.Event t k] <- Map.elems P.<$>
          Map.traverseWithKey
            (\k (s, _) -> headerBarLink s k (RD.demuxed currentTab (Just k)))
            tabItems
        let eTabClicks :: R.Event t k = R.leftmost tabClicksList
        fmap RD.demux $ R.holdDyn t0 (fmap Just eTabClicks)
  el "div" $ do
    _ <- Map.traverseWithKey
      (\k (_, w) -> do
        let isSelected = RD.demuxed currentTab (Just k)
            attrs      = R.ffor isSelected $ \s -> if s
              then Map.empty
              else Map.singleton "style" "display:none;"
        elDynAttr "div" attrs w)
      tabItems
    pure ()
  where
    headerBarLink :: Text -> k -> R.Dynamic t Bool -> Eff es (R.Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap
            (\b -> if b then Map.singleton "class" activeClass else Map.empty)
            isSelected
      elDynAttr "li" attrs $ do
        a <- link x
        pure $ fmap (P.const k) (_link_clicked a)

-- =====================================================================
-- WebSocket surface — port of Reflex.Dom.WebSocket +
-- Reflex.Dom.WebSocket.Foreign + Foreign.JavaScript.Utils bytestring
-- helpers.
--
-- Widget-monad constraints (PerformEvent / TriggerEvent / PostBuild)
-- are expressed as effect-row memberships (HH.PerformEventEff '[IOE] (R.SI.SpiderTimeline x) :> es
-- etc.); MonadJSM maps to JSM' :> es.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Foreign JavaScript helpers (bytestring ↔ ArrayBuffer, JSON parse)
-- ---------------------------------------------------------------------

-- | ByteString → ArrayBuffer via GHCJS.Buffer + jsaddle. On an empty
-- bytestring, constructs a fresh @new ArrayBuffer(0)@ (avoiding
-- ghcjs-only 'JSTAB.create' which isn't exported on the GHC build).
bsToArrayBuffer :: ByteString -> JSM ArrayBuffer
bsToArrayBuffer bs = do
  (b, off, len) <- ghcjsPure (JSBuf.fromByteString bs)
  ArrayBuffer <$> if BS.length bs P.== 0
    then JSaddleWS.new (jsg ("ArrayBuffer" :: Text)) [(0 :: Int)]
    else do
      ref <- ghcjsPure (JSBuf.getArrayBuffer b) >>= ghcjsPure . jsval
      dataViewCall off len ref
  where
    -- Inlined new-Uint8Array view — mirrors variant A's 'js_dataView'
    -- in Foreign.JavaScript.Internal.Utils (JSaddle route).
    -- Constructs `new Uint8Array(buf, off, len)`.
    dataViewCall :: Int -> Int -> JSVal -> JSM JSVal
    dataViewCall off len buf =
      JSaddleWS.new (jsg ("Uint8Array" :: Text)) (buf, off, len)

-- | ArrayBuffer (mutable) → ByteString via GHCJS.Buffer.
bsFromMutableArrayBuffer :: JSTAB.MutableArrayBuffer -> JSM ByteString
bsFromMutableArrayBuffer ab =
  JSTAB.unsafeFreeze ab >>= ghcjsPure . JSBuf.createFromArrayBuffer
                        >>= ghcjsPure . JSBuf.toByteString 0 Nothing

-- | Native @JSON.parse(...)@ wrapper.
js_jsonParse :: JSString -> JSM JSVal
js_jsonParse a = jsg ("JSON" :: Text) ^. JSaddleWS.js1 ("parse" :: Text) a

-- | Decode a JSString as JSON, returning 'Nothing' on any error. GHC
-- path uses Data.Aeson.decode directly; a GHCJS-only path would invoke
-- @JSON.parse@ then @fromJSVal@ — that variant is guarded by @ghcjs@
-- CPP upstream but not needed here since the isolated build is GHC.
jsonDecode :: FromJSON a => JSString -> Maybe a
jsonDecode = Aeson.decode . WSLBS.fromStrict . encodeUtf8 . JSaddleWS.textFromJSString

-- ---------------------------------------------------------------------
-- IsWebSocketMessage + JSWebSocket newtype + raw send/close/new
-- ---------------------------------------------------------------------

newtype JSWebSocket = JSWebSocket { unJSWebSocket :: WebSocket }

-- | Values that can be sent over a 'JSWebSocket'.
class IsWebSocketMessage a where
  webSocketSend :: JSWebSocket -> a -> JSM ()

instance (IsWebSocketMessage a, IsWebSocketMessage b) => IsWebSocketMessage (Either a b) where
  webSocketSend jws = bitraverse_ (webSocketSend jws) (webSocketSend jws)

-- Binary path — sends as an ArrayBuffer. Not supported on IE 11/below
-- but every modern browser handles it.
instance IsWebSocketMessage ByteString where
  webSocketSend (JSWebSocket ws) bs = do
    ab <- bsToArrayBuffer bs
    DOMWS.send ws ab

instance IsWebSocketMessage WSLBS.ByteString where
  webSocketSend ws = webSocketSend ws . WSLBS.toStrict

-- Plaintext path.
instance IsWebSocketMessage Text where
  webSocketSend (JSWebSocket ws) = DOMWS.sendString ws

-- | Close a websocket with the given status code + reason.
closeWebSocket :: JSWebSocket -> Word -> Text -> JSM ()
closeWebSocket (JSWebSocket ws) code reason =
  DOMWS.close ws (Just code) (Just reason)

-- | Construct a browser 'WebSocket' with the supplied callbacks.
newWebSocket
  :: Text                                   -- ^ URL
  -> [Text]                                 -- ^ Sub-protocols
  -> (Either ByteString JSVal -> JSM ())    -- ^ onmessage
  -> JSM ()                                 -- ^ onopen
  -> JSM ()                                 -- ^ onerror
  -> ((Bool, Word, Text) -> JSM ())         -- ^ onclose
  -> JSM JSWebSocket
newWebSocket url protocols onMessage onOpen onError onClose = do
  let onOpenWrapped  = fun $ \_ _ _ -> onOpen
      onErrorWrapped = fun $ \_ _ _ -> onError
      onCloseWrapped = fun $ \_ _ evArgs -> case evArgs of
        [] -> pure ()
        (e:_) -> do
          let e' = DOMCloseEvent.CloseEvent e
          wasClean <- DOMCloseEvent.getWasClean e'
          code     <- DOMCloseEvent.getCode     e'
          reason   <- DOMCloseEvent.getReason   e'
          JSaddleWS.liftJSM $ onClose (wasClean, code, reason)
      onMessageWrapped = fun $ \_ _ evArgs -> case evArgs of
        [] -> pure ()
        (e:_) -> do
          let e' = DOMMessageEvent.MessageEvent e
          d <- DOMMessageEvent.getData e'
          JSaddleWS.liftJSM $ ghcjsPure (GHCJSFor.jsTypeOf d) >>= \case
            GHCJSFor.String -> onMessage $ Right d
            _               -> do
              ab <- mutableArrayBufferFromJSVal d
              bsFromMutableArrayBuffer ab >>= onMessage . Left
  ws <- JSaddleWS.new (jsg ("WebSocket" :: Text)) (url, protocols)
  _  <- ws ^. jss ("binaryType" :: Text) ("arraybuffer" :: Text)
  _  <- ws ^. js2 ("addEventListener" :: Text) ("open"    :: Text) onOpenWrapped
  _  <- ws ^. js2 ("addEventListener" :: Text) ("error"   :: Text) onErrorWrapped
  _  <- ws ^. js2 ("addEventListener" :: Text) ("close"   :: Text) onCloseWrapped
  _  <- ws ^. js2 ("addEventListener" :: Text) ("message" :: Text) onMessageWrapped
  pure $ JSWebSocket (WebSocket ws)

-- | Decode a raw ws payload as bytes (Left = binary, Right = string
-- coming from a browser that surfaces text as JSVal).
onBSMessage :: Either ByteString JSVal -> JSM ByteString
onBSMessage = either pure (fmap encodeUtf8 . DOMTypes.fromJSValUnchecked)

-- | Fork an IO thread that runs the supplied 'JSM' action inside the
-- current jsaddle context.
forkJSM :: JSM () -> JSM ()
forkJSM a = do
  jsm <- JSaddleWS.askJSM
  void $ liftIO $ forkIO $ runJSM a jsm

-- ---------------------------------------------------------------------
-- Widget: WebSocket config + result records
-- ---------------------------------------------------------------------

data WebSocketConfig t a = WebSocketConfig
  { _webSocketConfig_send      :: R.Event t [a]
  , _webSocketConfig_close     :: R.Event t (Word, Text)
  , _webSocketConfig_reconnect :: Bool
  , _webSocketConfig_protocols :: [Text]
  }

instance R.Reflex t => Default (WebSocketConfig t a) where
  def = WebSocketConfig R.never R.never True []

type WebSocket_ t = RawWebSocket t ByteString

data RawWebSocket t a = RawWebSocket
  { _webSocket_recv  :: R.Event t a
  , _webSocket_open  :: R.Event t ()
  , _webSocket_error :: R.Event t ()
  , _webSocket_close :: R.Event t (Bool, Word, Text)
  }

-- ---------------------------------------------------------------------
-- WebSocket widget entry points
-- ---------------------------------------------------------------------

-- | Bind a browser websocket. Delivers received bytes as an 'Event',
-- fires @open@ / @error@ / @close@ events accordingly. If
-- '_webSocketConfig_reconnect' is 'True' (default), automatically
-- reconnects after a 1-second delay when the connection closes.
webSocket
  :: forall t es a
   . ( R.Reflex t, IsWebSocketMessage a
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , DomBuilderTimelineE es ~ t
     , JSM' :> es, IOE :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.TriggerEvent t :> es
     , HH.PostBuild t :> es
     )
  => Text -> WebSocketConfig t a -> Eff es (WebSocket_ t)
webSocket url config = webSocket' url config onBSMessage

-- | Lower-level variant of 'webSocket' — customise the on-message
-- decoder from @Either ByteString JSVal@ to any @b@.
webSocket'
  :: forall t es a b
   . ( R.Reflex t, IsWebSocketMessage a
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , DomBuilderTimelineE es ~ t
     , JSM' :> es, IOE :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.TriggerEvent t :> es
     , HH.PostBuild t :> es
     )
  => Text -> WebSocketConfig t a
  -> (Either ByteString JSVal -> JSM b)
  -> Eff es (RawWebSocket t b)
webSocket' url config onRawMessage = do
  (eRecv,  onMessage)     <- HH.newTriggerEventE @t
  currentSocketRef        <- liftIO $ newIORef Nothing
  (eOpen,  triggerEOpen)  <- HH.newTriggerEventWithOnCompleteE @t
  (eError, triggerEError) <- HH.newTriggerEventE @t
  (eClose, triggerEClose) <- HH.newTriggerEventE @t
  payloadQueue <- liftIO STM.newTQueueIO
  isOpen       <- liftIO STM.newEmptyTMVarIO
  ctx <- askJSM
  let onOpen = triggerEOpen () (liftIO $ void $ STM.atomically $ STM.tryPutTMVar isOpen ())
      onError = triggerEError ()
      onClose args = do
        liftIO $ triggerEClose args
        _ <- liftIO $ STM.atomically $ STM.tryTakeTMVar isOpen
        liftIO $ writeIORef currentSocketRef Nothing
        when (_webSocketConfig_reconnect config) $ forkJSM $ do
          liftIO $ threadDelay 1000000
          start
      start = do
        ws <- newWebSocket url (_webSocketConfig_protocols config)
                (onRawMessage >=> liftIO . onMessage)
                (liftIO onOpen) (liftIO onError) onClose
        liftIO $ writeIORef currentSocketRef (Just ws)
        pure ()
  postBuild <- HH.getPostBuildE @t
  HH.performEventME_ @(RH.HostFrame t) @t $
    R.ffor postBuild $ \_ -> liftIO (runJSM start ctx)
  HH.performEventME_ @(RH.HostFrame t) @t $
    R.ffor (_webSocketConfig_send config) $ \payloads ->
      liftIO $ forM_ payloads $ \payload ->
        STM.atomically $ STM.writeTQueue payloadQueue payload
  HH.performEventME_ @(RH.HostFrame t) @t $
    R.ffor (_webSocketConfig_close config) $ \(code, reason) ->
      liftIO $ (runJSM `P.flip` ctx) $ do
        mws <- liftIO $ readIORef currentSocketRef
        case mws of
          Nothing -> pure ()
          Just ws -> closeWebSocket ws (fromIntegral code) reason
  _ <- liftIO $ forkIO $ forever $ do
    payload <- STM.atomically $ do
      pl   <- STM.readTQueue payloadQueue
      open <- STM.tryReadTMVar isOpen
      if isJust open then pure pl else STM.retry
    mws <- readIORef currentSocketRef
    success <- case mws of
      Nothing -> pure False
      Just ws -> (runJSM `P.flip` ctx) $ do
        rs <- getReadyState (unJSWebSocket ws)
        if rs P.== 1
          then (webSocketSend ws payload >> pure True)
                 `JSMonad.catch` (\(_ :: CE.SomeException) -> pure False)
          else pure False
    unless success $ STM.atomically $ STM.unGetTQueue payloadQueue payload
  pure (RawWebSocket eRecv eOpen eError eClose)

-- | 'webSocket'' variant that decodes each incoming message as 'Text'
-- (via 'decodeUtf8' for binary frames, or 'fromJSValUnchecked' for
-- string frames).
textWebSocket
  :: forall t es a
   . ( R.Reflex t, IsWebSocketMessage a
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , DomBuilderTimelineE es ~ t
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.TriggerEvent t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     )
  => Text -> WebSocketConfig t a -> Eff es (RawWebSocket t Text)
textWebSocket url cfg =
  webSocket' url cfg (either (pure . decodeUtf8) DOMTypes.fromJSValUnchecked)

-- | Send/receive JSON. Outgoing @a@ values are 'encode'd; incoming
-- messages are JSON-decoded to @Maybe b@.
jsonWebSocket
  :: forall t es a b
   . ( R.Reflex t, ToJSON a, FromJSON b
     , RH.ReflexHost t
     , MonadIO (RH.HostFrame t)
     , DomBuilderTimelineE es ~ t
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.TriggerEvent t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     )
  => Text -> WebSocketConfig t a -> Eff es (RawWebSocket t (Maybe b))
jsonWebSocket url cfg = do
  ws <- textWebSocket url $ cfg
    { _webSocketConfig_send = fmap (decodeUtf8 . WSLBS.toStrict . encode)
                                <$> _webSocketConfig_send cfg
    }
  pure ws { _webSocket_recv = jsonDecode . textToJSString <$> _webSocket_recv ws }

-- ---------------------------------------------------------------------
-- WebSocket record lenses
-- ---------------------------------------------------------------------

webSocketConfig_send :: Lens' (WebSocketConfig t a) (R.Event t [a])
webSocketConfig_send f (WebSocketConfig x1 x2 x3 x4) =
  (\y -> WebSocketConfig y x2 x3 x4) <$> f x1
{-# INLINE webSocketConfig_send #-}

webSocketConfig_close :: Lens' (WebSocketConfig t a) (R.Event t (Word, Text))
webSocketConfig_close f (WebSocketConfig x1 x2 x3 x4) =
  (\y -> WebSocketConfig x1 y x3 x4) <$> f x2
{-# INLINE webSocketConfig_close #-}

webSocketConfig_reconnect :: Lens' (WebSocketConfig t a) Bool
webSocketConfig_reconnect f (WebSocketConfig x1 x2 x3 x4) =
  (\y -> WebSocketConfig x1 x2 y x4) <$> f x3
{-# INLINE webSocketConfig_reconnect #-}

webSocketConfig_protocols :: Lens' (WebSocketConfig t a) [Text]
webSocketConfig_protocols f (WebSocketConfig x1 x2 x3 x4) =
  (\y -> WebSocketConfig x1 x2 x3 y) <$> f x4
{-# INLINE webSocketConfig_protocols #-}

webSocket_recv :: Lens' (RawWebSocket t a) (R.Event t a)
webSocket_recv f (RawWebSocket x1 x2 x3 x4) =
  (\y -> RawWebSocket y x2 x3 x4) <$> f x1
{-# INLINE webSocket_recv #-}

webSocket_open :: Lens' (RawWebSocket t a) (R.Event t ())
webSocket_open f (RawWebSocket x1 x2 x3 x4) =
  (\y -> RawWebSocket x1 y x3 x4) <$> f x2
{-# INLINE webSocket_open #-}

webSocket_error :: Lens' (RawWebSocket t a) (R.Event t ())
webSocket_error f (RawWebSocket x1 x2 x3 x4) =
  (\y -> RawWebSocket x1 x2 y x4) <$> f x3
{-# INLINE webSocket_error #-}

webSocket_close :: Lens' (RawWebSocket t a) (R.Event t (Bool, Word, Text))
webSocket_close f (RawWebSocket x1 x2 x3 x4) =
  (\y -> RawWebSocket x1 x2 x3 y) <$> f x4
{-# INLINE webSocket_close #-}

-- =====================================================================
-- Xhr surface — port of Reflex.Dom.Xhr + Xhr.Foreign + Xhr.Exception
-- + Xhr.ResponseType + Xhr.FormData.
--
-- Widget-monad constraints (PerformEvent / TriggerEvent / JSM' / IOE)
-- expressed as effect-row memberships. Delegates to
-- 'RPE.performEventAsync' (from Reflex.PerformEvent.Class, MTL-shape)
-- which discovers the Adjustable / PerformEvent / TriggerEvent
-- instances on 'Eff es' automatically.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Xhr.Exception + Xhr.ResponseType (trivial data types)
-- ---------------------------------------------------------------------

data XhrException = XhrException_Error | XhrException_Aborted
  deriving (Show, Read, Eq, Ord)

instance CE.Exception XhrException

data XhrResponseType
  = XhrResponseType_Default
  | XhrResponseType_ArrayBuffer
  | XhrResponseType_Blob
  | XhrResponseType_Text
  deriving (Show, Read, Eq, Ord)

data XhrResponseBody
  = XhrResponseBody_Default Text
  | XhrResponseBody_Text Text
  | XhrResponseBody_Blob Blob
  | XhrResponseBody_ArrayBuffer ByteString

-- ---------------------------------------------------------------------
-- Xhr.Foreign — IsXhrPayload class + XMLHttpRequest wrappers
-- ---------------------------------------------------------------------

xmlHttpRequestNew :: JSM XMLHttpRequest
xmlHttpRequestNew = DOMXhr.newXMLHttpRequest

xmlHttpRequestOpen
  :: XMLHttpRequest -> Text -> Text -> Bool -> Text -> Text -> JSM ()
xmlHttpRequestOpen req method url isAsync user password =
  DOMXhr.open req method url isAsync (Just user) (Just password)

convertXhrException :: XHRError -> XhrException
convertXhrException e = case e of
  XHRError   -> XhrException_Error
  XHRAborted -> XhrException_Aborted

class IsXhrPayload a where
  sendXhrPayload :: XMLHttpRequest -> a -> JSM ()

instance IsXhrPayload () where
  sendXhrPayload xhr _ = DOMXhr.send xhr

instance IsXhrPayload P.String where
  sendXhrPayload = DOMXhr.sendString

instance IsXhrPayload Text where
  sendXhrPayload = DOMXhr.sendString

instance IsXhrPayload FormData where
  sendXhrPayload = DOMXhr.sendFormData

instance IsXhrPayload Blob where
  sendXhrPayload = DOMXhr.sendBlob

instance IsXhrPayload ArrayBuffer where
  sendXhrPayload xhr ab = DOMXhr.sendArrayBuffer xhr (ArrayBufferView (unArrayBuffer ab))

instance IsXhrPayload ByteString where
  sendXhrPayload xhr bs = sendXhrPayload xhr =<< JSaddleWS.liftJSM (bsToArrayBuffer bs)

xmlHttpRequestSend :: IsXhrPayload payload => XMLHttpRequest -> payload -> JSM ()
xmlHttpRequestSend self p =
  sendXhrPayload self p `JSMonad.catch` (liftIO . CE.throwIO . convertXhrException)

xmlHttpRequestSetRequestHeader :: XMLHttpRequest -> Text -> Text -> JSM ()
xmlHttpRequestSetRequestHeader = DOMXhr.setRequestHeader

xmlHttpRequestAbort :: XMLHttpRequest -> JSM ()
xmlHttpRequestAbort = DOMXhr.abort

xmlHttpRequestGetAllResponseHeaders :: MonadJSM m => XMLHttpRequest -> m Text
xmlHttpRequestGetAllResponseHeaders = DOMXhr.getAllResponseHeaders

xmlHttpRequestGetResponseHeader :: MonadJSM m => XMLHttpRequest -> Text -> m Text
xmlHttpRequestGetResponseHeader self header =
  fromMaybe "" <$> DOMXhr.getResponseHeader self header

xmlHttpRequestOverrideMimeType :: MonadJSM m => XMLHttpRequest -> Text -> m ()
xmlHttpRequestOverrideMimeType = DOMXhr.overrideMimeType

xmlHttpRequestOnreadystatechange
  :: XMLHttpRequest
  -> XhrEventM.EventM XMLHttpRequest DOMTypesXhr.Event ()
  -> JSM (JSM ())
xmlHttpRequestOnreadystatechange = (`XhrEventM.onSync` DOMXhr.readyStateChange)

xmlHttpRequestSetTimeout :: MonadJSM m => XMLHttpRequest -> Word -> m ()
xmlHttpRequestSetTimeout = DOMXhr.setTimeout

xmlHttpRequestGetTimeout :: MonadJSM m => XMLHttpRequest -> m Word
xmlHttpRequestGetTimeout = DOMXhr.getTimeout

xmlHttpRequestGetReadyState :: MonadJSM m => XMLHttpRequest -> m Word
xmlHttpRequestGetReadyState = DOMXhr.getReadyState

xmlHttpRequestSetWithCredentials :: MonadJSM m => XMLHttpRequest -> Bool -> m ()
xmlHttpRequestSetWithCredentials = DOMXhr.setWithCredentials

xmlHttpRequestGetWithCredentials :: MonadJSM m => XMLHttpRequest -> m Bool
xmlHttpRequestGetWithCredentials = DOMXhr.getWithCredentials

xmlHttpRequestGetResponseText :: MonadJSM m => XMLHttpRequest -> m (Maybe Text)
xmlHttpRequestGetResponseText = DOMXhr.getResponseText

xmlHttpRequestSetResponseType :: MonadJSM m => XMLHttpRequest -> XMLHttpRequestResponseType -> m ()
xmlHttpRequestSetResponseType = DOMXhr.setResponseType

fromResponseType :: XhrResponseType -> XMLHttpRequestResponseType
fromResponseType XhrResponseType_Default     = XMLHttpRequestResponseType
fromResponseType XhrResponseType_ArrayBuffer = XMLHttpRequestResponseTypeArraybuffer
fromResponseType XhrResponseType_Blob        = XMLHttpRequestResponseTypeBlob
fromResponseType XhrResponseType_Text        = XMLHttpRequestResponseTypeText

toResponseType :: XMLHttpRequestResponseType -> Maybe XhrResponseType
toResponseType XMLHttpRequestResponseType             = Just XhrResponseType_Default
toResponseType XMLHttpRequestResponseTypeArraybuffer  = Just XhrResponseType_ArrayBuffer
toResponseType XMLHttpRequestResponseTypeBlob         = Just XhrResponseType_Blob
toResponseType XMLHttpRequestResponseTypeText         = Just XhrResponseType_Text
toResponseType _                                      = Nothing

xmlHttpRequestGetResponseType :: MonadJSM m => XMLHttpRequest -> m (Maybe XhrResponseType)
xmlHttpRequestGetResponseType = fmap toResponseType . DOMXhr.getResponseType

xmlHttpRequestGetStatus :: MonadJSM m => XMLHttpRequest -> m Word
xmlHttpRequestGetStatus = DOMXhr.getStatus

xmlHttpRequestGetStatusText :: MonadJSM m => XMLHttpRequest -> m Text
xmlHttpRequestGetStatusText = DOMXhr.getStatusText

xmlHttpRequestGetResponse :: MonadJSM m => XMLHttpRequest -> m (Maybe XhrResponseBody)
xmlHttpRequestGetResponse xhr = do
  mr <- DOMXhr.getResponse xhr
  rt <- xmlHttpRequestGetResponseType xhr
  case rt of
    Just XhrResponseType_Blob        -> JSaddleWS.liftJSM $ fmap XhrResponseBody_Blob <$> castTo Blob mr
    Just XhrResponseType_Text        -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
    Just XhrResponseType_Default     -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
    Just XhrResponseType_ArrayBuffer -> JSaddleWS.liftJSM $ do
      ab <- mutableArrayBufferFromJSVal mr
      Just . XhrResponseBody_ArrayBuffer <$> bsFromMutableArrayBuffer ab
    _ -> pure Nothing

-- ---------------------------------------------------------------------
-- Xhr — request / response records + XhrRequest constructor
-- ---------------------------------------------------------------------

data XhrRequest a = XhrRequest
  { _xhrRequest_method :: Text
  , _xhrRequest_url    :: Text
  , _xhrRequest_config :: XhrRequestConfig a
  }
  deriving (Show, Read, Eq, Ord, Functor)

data XhrRequestConfig a = XhrRequestConfig
  { _xhrRequestConfig_headers          :: Map Text Text
  , _xhrRequestConfig_user             :: Maybe Text
  , _xhrRequestConfig_password         :: Maybe Text
  , _xhrRequestConfig_responseType     :: Maybe XhrResponseType
  , _xhrRequestConfig_sendData         :: a
  , _xhrRequestConfig_withCredentials  :: Bool
  , _xhrRequestConfig_responseHeaders  :: XhrResponseHeaders
  }
  deriving (Show, Read, Eq, Ord, Functor)

data XhrResponse = XhrResponse
  { _xhrResponse_status       :: Word
  , _xhrResponse_statusText   :: Text
  , _xhrResponse_response     :: Maybe XhrResponseBody
  , _xhrResponse_responseText :: Maybe Text
  , _xhrResponse_headers      :: Map (CI Text) Text
  }

data XhrResponseHeaders
  = OnlyHeaders (XhrSet.Set (CI Text))
  | AllHeaders
  deriving (Show, Read, Eq, Ord)

instance Default XhrResponseHeaders where
  def = OnlyHeaders mempty

instance a ~ () => Default (XhrRequestConfig a) where
  def = XhrRequestConfig
    { _xhrRequestConfig_headers         = Map.empty
    , _xhrRequestConfig_user            = Nothing
    , _xhrRequestConfig_password        = Nothing
    , _xhrRequestConfig_responseType    = Nothing
    , _xhrRequestConfig_sendData        = ()
    , _xhrRequestConfig_withCredentials = False
    , _xhrRequestConfig_responseHeaders = def
    }

-- | Construct a request object from method, URL, config record.
xhrRequest :: Text -> Text -> XhrRequestConfig a -> XhrRequest a
xhrRequest = XhrRequest

-- ---------------------------------------------------------------------
-- Xhr — runner + performRequestAsync* family
-- ---------------------------------------------------------------------

-- | The core "open + configure + send + on-ready-state" loop, factored
-- so both the effect-typed and raw-IO variants can share it.
runXhrJSM
  :: IsXhrPayload a
  => XMLHttpRequest
  -> XhrRequest a
  -> (Either XhrException XhrResponse -> JSM ())
  -> JSM ()
runXhrJSM xhr req cb = do
  let c     = _xhrRequest_config req
      rt    = _xhrRequestConfig_responseType c
      creds = _xhrRequestConfig_withCredentials c
  xmlHttpRequestOpen xhr
    (_xhrRequest_method req)
    (_xhrRequest_url req)
    True
    (fromMaybe "" (_xhrRequestConfig_user c))
    (fromMaybe "" (_xhrRequestConfig_password c))
  _ <- Map.traverseWithKey (xmlHttpRequestSetRequestHeader xhr) (_xhrRequestConfig_headers c)
  maybe (pure ()) (xmlHttpRequestSetResponseType xhr . fromResponseType) rt
  xmlHttpRequestSetWithCredentials xhr creds
  _ <- xmlHttpRequestOnreadystatechange xhr $ do
    readyState <- xmlHttpRequestGetReadyState xhr
    status     <- xmlHttpRequestGetStatus xhr
    statusText <- xmlHttpRequestGetStatusText xhr
    when (readyState P.== 4) $ do
      t <- if rt P.== Just XhrResponseType_Text P.|| isNothing rt
           then xmlHttpRequestGetResponseText xhr
           else pure Nothing
      r <- xmlHttpRequestGetResponse xhr
      h <- case _xhrRequestConfig_responseHeaders c of
        AllHeaders -> parseAllHeadersString <$> xmlHttpRequestGetAllResponseHeaders xhr
        OnlyHeaders xs -> traverse (xmlHttpRequestGetResponseHeader xhr) (Map.fromSet CI.original xs)
      JSaddleWS.liftJSM $ cb $ Right XhrResponse
        { _xhrResponse_status       = status
        , _xhrResponse_statusText   = statusText
        , _xhrResponse_response     = r
        , _xhrResponse_responseText = t
        , _xhrResponse_headers      = h
        }
  _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
  pure ()

parseAllHeadersString :: Text -> Map (CI Text) Text
parseAllHeadersString s = Map.fromList $ fmap (stripBoth . T.span (P./= ':')) $
  XhrList.dropWhileEnd T.null $ T.splitOn (T.pack "\r\n") s
  where stripBoth (txt1, txt2) = (CI.mk (T.strip txt1), T.strip (T.drop 1 txt2))

-- | Async Xhr: forks a thread that runs the request via the ambient
-- JSaddle context and calls back with either a response or an error.
newXMLHttpRequestWithError
  :: forall es a. (JSM' :> es, IOE :> es, IsXhrPayload a)
  => XhrRequest a
  -> (Either XhrException XhrResponse -> JSM ())
  -> Eff es XMLHttpRequest
newXMLHttpRequestWithError req cb = do
  xhr <- liftJSM xmlHttpRequestNew
  ctx <- askJSM
  void $ liftIO $ forkIO $ handle ((`runJSM` ctx) . cb . Left) $
    runJSM (runXhrJSM xhr req cb) ctx
  pure xhr

newXMLHttpRequest
  :: (JSM' :> es, IOE :> es, IsXhrPayload a)
  => XhrRequest a -> (XhrResponse -> JSM ()) -> Eff es XMLHttpRequest
newXMLHttpRequest req cb = newXMLHttpRequestWithError req $ P.mapM_ cb

-- | Off-Eff IO variant, used inside 'performEventAsync' callbacks.
newXMLHttpRequestWithErrorIO
  :: IsXhrPayload a
  => JSContextRef
  -> XhrRequest a
  -> (Either XhrException XhrResponse -> JSM ())
  -> IO XMLHttpRequest
newXMLHttpRequestWithErrorIO ctx req cb = do
  xhr <- runJSM xmlHttpRequestNew ctx
  void $ forkIO $ handle ((`runJSM` ctx) . cb . Left) $
    runJSM (runXhrJSM xhr req cb) ctx
  pure xhr

newXMLHttpRequestIO
  :: IsXhrPayload a
  => JSContextRef -> XhrRequest a -> (XhrResponse -> JSM ()) -> IO XMLHttpRequest
newXMLHttpRequestIO ctx req cb = newXMLHttpRequestWithErrorIO ctx req $ P.mapM_ cb

-- | Fire an Xhr request for each Event occurrence; result Event carries
-- the response or an exception.
performRequestAsyncWithError
  :: forall t es a
   . ( R.Reflex t, IsXhrPayload a
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (XhrRequest a)
  -> Eff es (R.Event t (Either XhrException XhrResponse))
performRequestAsyncWithError =
  performRequestAsync' newXMLHttpRequestWithErrorIO . fmap pure

performRequestAsync
  :: forall t es a
   . ( R.Reflex t, IsXhrPayload a
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (XhrRequest a) -> Eff es (R.Event t XhrResponse)
performRequestAsync =
  performRequestAsync' newXMLHttpRequestIO . fmap pure

performMkRequestAsync
  :: forall t es a
   . ( R.Reflex t, IsXhrPayload a
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (JSM (XhrRequest a)) -> Eff es (R.Event t XhrResponse)
performMkRequestAsync = performRequestAsync' newXMLHttpRequestIO

performRequestAsync'
  :: forall t es a p
   . ( R.Reflex t
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => (JSContextRef -> XhrRequest p -> (a -> JSM ()) -> IO XMLHttpRequest)
  -> R.Event t (JSM (XhrRequest p))
  -> Eff es (R.Event t a)
performRequestAsync' newXhr req = do
  ctx <- askJSM
  RPE.performEventAsync $ R.ffor req $ \hr cb -> P.fmap (P.const ()) $ liftIO $ do
    r <- runJSM hr ctx
    _ <- newXhr ctx r (liftIO . cb)
    pure ()

performRequestsAsyncWithError
  :: forall t es a f
   . ( R.Reflex t, IsXhrPayload a, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (f (XhrRequest a))
  -> Eff es (R.Event t (f (Either XhrException XhrResponse)))
performRequestsAsyncWithError =
  performRequestsAsync' newXMLHttpRequestWithErrorIO . fmap pure

performRequestsAsync
  :: forall t es a f
   . ( R.Reflex t, IsXhrPayload a, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (f (XhrRequest a)) -> Eff es (R.Event t (f XhrResponse))
performRequestsAsync = performRequestsAsync' newXMLHttpRequestIO . fmap pure

performMkRequestsAsync
  :: forall t es a f
   . ( R.Reflex t, IsXhrPayload a, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t (JSM (f (XhrRequest a))) -> Eff es (R.Event t (f XhrResponse))
performMkRequestsAsync = performRequestsAsync' newXMLHttpRequestIO

performRequestsAsync'
  :: forall t es f a b
   . ( R.Reflex t, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => (JSContextRef -> XhrRequest b -> (a -> JSM ()) -> IO XMLHttpRequest)
  -> R.Event t (JSM (f (XhrRequest b)))
  -> Eff es (R.Event t (f a))
performRequestsAsync' newXhr req = do
  ctx <- askJSM
  RPE.performEventAsync $ R.ffor req $ \hrs cb -> P.fmap (P.const ()) $ liftIO $ do
    rs <- runJSM hrs ctx
    resps <- forM rs $ \r -> do
      resp <- newEmptyMVar
      _ <- newXhr ctx r (liftIO . putMVar resp)
      pure resp
    _ <- forkIO $ cb =<< forM resps takeMVar
    pure ()

-- ---------------------------------------------------------------------
-- Common Patterns
-- ---------------------------------------------------------------------

-- | GET a URL and decode the response as JSON.
getAndDecode
  :: forall t es a
   . ( R.Reflex t, FromJSON a
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t Text -> Eff es (R.Event t (Maybe a))
getAndDecode url = do
  r <- performRequestAsync $ fmap (\x -> XhrRequest "GET" x def) url
  pure $ fmap decodeXhrResponse r

-- | POST some JSON-encodable body to a URL.
postJson :: ToJSON a => Text -> a -> XhrRequest Text
postJson url a = XhrRequest "POST" url $ def
  { _xhrRequestConfig_headers  = "Content-type" =: "application/json"
  , _xhrRequestConfig_sendData = XhrLT.toStrict (XhrLB.toLazyText (encodeToTextBuilder (Aeson.toJSON a)))
  }

-- | Lift an @Event a -> Eff es (Event b)@ handler over @Maybe a@.
getMay
  :: R.Reflex t
  => (R.Event t a -> Eff es (R.Event t b))
  -> R.Event t (Maybe a)
  -> Eff es (R.Event t (Maybe b))
getMay f e = do
  e' <- f (R.fmapMaybe P.id e)
  pure $ R.leftmost [fmap Just e', R.fmapMaybe (maybe (Just Nothing) (P.const Nothing)) e]

decodeText :: FromJSON a => Text -> Maybe a
decodeText = Aeson.decode . WSLBS.fromStrict . encodeUtf8

decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a
decodeXhrResponse = decodeText <=< _xhrResponse_responseText

-- ---------------------------------------------------------------------
-- Xhr.FormData surface
-- ---------------------------------------------------------------------

data FormValue blob
  = FormValue_Text Text
  | FormValue_File blob (Maybe Text)

-- | POST FormData payloads.
postForms
  :: forall t es blob f
   . ( R.Reflex t, IsBlob blob, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => Text
  -> R.Event t (f (Map Text (FormValue blob)))
  -> Eff es (R.Event t (f XhrResponse))
postForms t = postForms' t def

-- | Like 'postForms' with a custom 'XhrRequestConfig'.
postForms'
  :: forall t es blob f a
   . ( R.Reflex t, IsBlob blob, Traversable f
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , RTE.TriggerEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => Text
  -> XhrRequestConfig a
  -> R.Event t (f (Map Text (FormValue blob)))
  -> Eff es (R.Event t (f XhrResponse))
postForms' url cfg payload = do
  performMkRequestsAsync $ R.ffor payload $ \fs ->
    for fs $ \u -> JSaddleWS.liftJSM $ do
      fd <- DOMFD.newFormData Nothing
      _  <- Map.traverseWithKey
        (\k v -> case v of
          FormValue_Text tt      -> DOMFD.append     fd k tt
          FormValue_File b mFn   -> DOMFD.appendBlob fd k b mFn)
        u
      pure $ xhrRequest "POST" url (cfg & xhrRequestConfig_sendData .~ fd)

-- | Convert a File (e.g. from 'fileInput') to a FormValue with filename.
fileToFormValue :: (JSM' :> es) => File -> Eff es (FormValue File)
fileToFormValue f = do
  fn <- liftJSM (getName f)
  pure $ FormValue_File f (Just fn)

-- ---------------------------------------------------------------------
-- Xhr record lenses
-- ---------------------------------------------------------------------

xhrRequest_method :: Lens' (XhrRequest a) Text
xhrRequest_method f (XhrRequest x1 x2 x3) = (\y -> XhrRequest y x2 x3) <$> f x1
{-# INLINE xhrRequest_method #-}

xhrRequest_url :: Lens' (XhrRequest a) Text
xhrRequest_url f (XhrRequest x1 x2 x3) = (\y -> XhrRequest x1 y x3) <$> f x2
{-# INLINE xhrRequest_url #-}

xhrRequest_config :: Lens' (XhrRequest a) (XhrRequestConfig a)
xhrRequest_config f (XhrRequest x1 x2 x3) = (\y -> XhrRequest x1 x2 y) <$> f x3
{-# INLINE xhrRequest_config #-}

xhrRequestConfig_headers :: Lens' (XhrRequestConfig a) (Map Text Text)
xhrRequestConfig_headers f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig y x2 x3 x4 x5 x6 x7) <$> f x1
{-# INLINE xhrRequestConfig_headers #-}

xhrRequestConfig_user :: Lens' (XhrRequestConfig a) (Maybe Text)
xhrRequestConfig_user f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 y x3 x4 x5 x6 x7) <$> f x2
{-# INLINE xhrRequestConfig_user #-}

xhrRequestConfig_password :: Lens' (XhrRequestConfig a) (Maybe Text)
xhrRequestConfig_password f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 x2 y x4 x5 x6 x7) <$> f x3
{-# INLINE xhrRequestConfig_password #-}

xhrRequestConfig_responseType :: Lens' (XhrRequestConfig a) (Maybe XhrResponseType)
xhrRequestConfig_responseType f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 x2 x3 y x5 x6 x7) <$> f x4
{-# INLINE xhrRequestConfig_responseType #-}

xhrRequestConfig_sendData :: Lens (XhrRequestConfig a) (XhrRequestConfig b) a b
xhrRequestConfig_sendData f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 x2 x3 x4 y x6 x7) <$> f x5
{-# INLINE xhrRequestConfig_sendData #-}

xhrRequestConfig_withCredentials :: Lens' (XhrRequestConfig a) Bool
xhrRequestConfig_withCredentials f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 x2 x3 x4 x5 y x7) <$> f x6
{-# INLINE xhrRequestConfig_withCredentials #-}

xhrRequestConfig_responseHeaders :: Lens' (XhrRequestConfig a) XhrResponseHeaders
xhrRequestConfig_responseHeaders f (XhrRequestConfig x1 x2 x3 x4 x5 x6 x7) =
  (\y -> XhrRequestConfig x1 x2 x3 x4 x5 x6 y) <$> f x7
{-# INLINE xhrRequestConfig_responseHeaders #-}

xhrResponse_status :: Lens' XhrResponse Word
xhrResponse_status f (XhrResponse x1 x2 x3 x4 x5) =
  (\y -> XhrResponse y x2 x3 x4 x5) <$> f x1
{-# INLINE xhrResponse_status #-}

xhrResponse_statusText :: Lens' XhrResponse Text
xhrResponse_statusText f (XhrResponse x1 x2 x3 x4 x5) =
  (\y -> XhrResponse x1 y x3 x4 x5) <$> f x2
{-# INLINE xhrResponse_statusText #-}

xhrResponse_response :: Lens' XhrResponse (Maybe XhrResponseBody)
xhrResponse_response f (XhrResponse x1 x2 x3 x4 x5) =
  (\y -> XhrResponse x1 x2 y x4 x5) <$> f x3
{-# INLINE xhrResponse_response #-}

xhrResponse_responseText :: Lens' XhrResponse (Maybe Text)
xhrResponse_responseText f (XhrResponse x1 x2 x3 x4 x5) =
  (\y -> XhrResponse x1 x2 x3 y x5) <$> f x4
{-# INLINE xhrResponse_responseText #-}

xhrResponse_headers :: Lens' XhrResponse (Map (CI Text) Text)
xhrResponse_headers f (XhrResponse x1 x2 x3 x4 x5) =
  (\y -> XhrResponse x1 x2 x3 x4 y) <$> f x5
{-# INLINE xhrResponse_headers #-}
{-# INLINABLE tabDisplay #-}

-- =====================================================================
-- Location — window/history navigation.
--
-- Direct port of upstream 'Reflex.Dom.Location'. Every helper stays
-- polymorphic in 'MonadJSM m' so it can be called from both @Eff es@
-- (via effectful-jsaddle's MonadJSM instance) and 'JSM' proper.
-- The higher-order combinators ('browserHistoryWith', 'manageHistory',
-- 'manageHistory'', 'manageHistoryExposingExternalUpdates',
-- 'popHistoryState') take the standard Eff-shape constraint pack.
-- =====================================================================

withLocation :: MonadJSM m => (Location -> m a) -> m a
withLocation f = GHCJS.DOM.currentWindowUnchecked >>= Window.getLocation >>= f

-- | Full URI-decoded URL of the current window location.
getLocationUrl :: MonadJSM m => m Text
getLocationUrl = withLocation (DOMLocation.getHref >=> decodeURIText)

-- | Host of the current window location.
getLocationHost :: MonadJSM m => m Text
getLocationHost = withLocation DOMLocation.getHost

-- | Protocol/scheme (e.g. @http:@ or @https:@) of the current window location.
getLocationProtocol :: MonadJSM m => m Text
getLocationProtocol = withLocation DOMLocation.getProtocol

-- | URI-decoded location after the host and port; i.e. path + query + fragment.
getLocationAfterHost :: MonadJSM m => m Text
getLocationAfterHost = withLocation $ \loc -> do
  pathname <- DOMLocation.getPathname loc
  search   <- DOMLocation.getSearch   loc
  hash     <- DOMLocation.getHash     loc
  decodeURI (P.mconcat [pathname, search, hash] :: Text)

-- | URI-decoded path of the current window location.
getLocationPath :: MonadJSM m => m Text
getLocationPath = withLocation (DOMLocation.getPathname >=> decodeURIText)

-- | URI-decoded fragment/hash of the current window location.
getLocationFragment :: MonadJSM m => m Text
getLocationFragment = withLocation (DOMLocation.getHash >=> decodeURIText)

-- | Decode a URI via JavaScript's @decodeURI@. Loud-crash on malformed
-- input, matching upstream reflex-dom's behaviour. Callers wanting a
-- 'Maybe'-shaped result should wrap in 'try' at the call site.
decodeURI :: (MonadJSM m, ToJSString a, FromJSString b) => a -> m b
decodeURI input = do
  window  <- JSaddleWS.liftJSM GHCJS.DOM.currentWindowUnchecked
  window' <- JSaddleWS.liftJSM (toJSVal window)
  JSaddleWS.liftJSM (window' ^. js1 (T.pack "decodeURI") input >>= fromJSValUnchecked)

decodeURIText :: MonadJSM m => Text -> m Text
decodeURIText = decodeURI

-- | Build a Dynamic carrying the current window location, updated on @popstate@.
browserHistoryWith
  :: forall t es a
   . ( R.Reflex t
     , JSM' :> es, IOE :> es
     , HH.TriggerEvent t :> es
     , HH.Hold t :> es
     )
  => (forall jsm. MonadJSM jsm => Location -> jsm a)
  -- ^ Encode the window location in a more useful form (e.g. 'getLocationAfterHost').
  -> Eff es (R.Dynamic t a)
browserHistoryWith f = do
  (window, loc0) <- liftJSM $ do
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    v0 <- f location
    pure (win, v0)
  locEv <- wrapDomEvent @t window (`XhrEventM.on` WinEvtHandlers.popState) $ do
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    f location
  HH.holdDynE @t loc0 locEv

data HistoryItem = HistoryItem
  { _historyItem_state :: SerializedScriptValue
  , _historyItem_uri   :: URI
  -- ^ NOTE: All URIs in this module are assumed to be already percent-escaped.
  }

data HistoryStateUpdate = HistoryStateUpdate
  { _historyStateUpdate_state :: SerializedScriptValue
  , _historyStateUpdate_title :: Text
  , _historyStateUpdate_uri   :: Maybe URI
  -- ^ If Just, update the URI; otherwise leave it unchanged.
  -- NOTE: All URIs in this module are assumed to be already percent-escaped.
  }

data HistoryCommand
  = HistoryCommand_PushState    HistoryStateUpdate
  | HistoryCommand_ReplaceState HistoryStateUpdate

runHistoryCommand :: MonadJSM m => History -> HistoryCommand -> m ()
runHistoryCommand history = \case
  HistoryCommand_PushState su -> DOMHistory.pushState history
    (_historyStateUpdate_state su)
    (_historyStateUpdate_title su)
    (P.fmap P.show (_historyStateUpdate_uri su))
  HistoryCommand_ReplaceState su -> DOMHistory.replaceState history
    (_historyStateUpdate_state su)
    (_historyStateUpdate_title su)
    (P.fmap P.show (_historyStateUpdate_uri su))

getLocationUriAuth :: MonadJSM m => Location -> m URIAuth
getLocationUriAuth location = URIAuth ""  -- username/password not portable across browsers
  <$> P.fmap T.unpack (DOMLocation.getHostname location)
  <*> P.fmap (appendColonIfNotEmpty . T.unpack) (DOMLocation.getPort location)
  where
    appendColonIfNotEmpty = \case
      "" -> ""
      x  -> ":" P.++ x

getLocationUri :: MonadJSM m => Location -> m URI
getLocationUri location = URI
  <$> P.fmap T.unpack (DOMLocation.getProtocol location)
  <*> P.fmap Just     (getLocationUriAuth location)
  <*> P.fmap T.unpack (DOMLocation.getPathname location)
  <*> P.fmap T.unpack (DOMLocation.getSearch   location)
  <*> P.fmap T.unpack (DOMLocation.getHash     location)

manageHistory
  :: forall t es
   . ( R.Reflex t
     , JSM' :> es, IOE :> es
     , HH.TriggerEvent t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , HH.Hold t :> es
     )
  => R.Event t HistoryCommand
  -> Eff es (R.Dynamic t HistoryItem)
manageHistory runCmd = do
  ctx <- askJSM
  (window, history, item0) <- liftJSM $ do
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    hist <- Window.getHistory win
    i0 <- HistoryItem <$> DOMHistory.getState hist <*> getLocationUri location
    pure (win, hist, i0)
  itemSetInternal <- RPE.performEvent $ R.ffor runCmd $ \cmd -> liftIO $ runJSM
    (do
        runHistoryCommand history cmd
        location <- Window.getLocation window
        HistoryItem <$> DOMHistory.getState history <*> getLocationUri location)
    ctx
  itemSetExternal <- wrapDomEvent @t window (`XhrEventM.on` WinEvtHandlers.popState) $ do
    e <- XhrEventM.event
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    HistoryItem
      <$> P.fmap SerializedScriptValue (PopStateEvent.getState e)
      <*> getLocationUri location
  HH.holdDynE @t item0 (R.leftmost [itemSetInternal, itemSetExternal])

manageHistory'
  :: forall t es
   . ( R.Reflex t
     , MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.TriggerEvent t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , HH.Hold t :> es
     )
  => R.Event t ()          -- ^ Gate — do nothing until this event has fired.
  -> R.Event t HistoryCommand
  -> Eff es (R.Dynamic t HistoryItem)
manageHistory' switchover runCmd =
  P.fmap P.fst (manageHistoryExposingExternalUpdates switchover runCmd)

manageHistoryExposingExternalUpdates
  :: forall t es
   . ( R.Reflex t
     , MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.TriggerEvent t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , HH.Hold t :> es
     )
  => R.Event t ()          -- ^ Gate — do nothing until this event has fired.
  -> R.Event t HistoryCommand
  -> Eff es (R.Dynamic t HistoryItem, R.Event t HistoryItem)
manageHistoryExposingExternalUpdates switchover runCmd = do
  ctx <- askJSM
  (window, history, item0) <- liftJSM $ do
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    hist <- Window.getHistory win
    i0 <- HistoryItem <$> DOMHistory.getState hist <*> getLocationUri location
    pure (win, hist, i0)
  itemSetExternal' <- wrapDomEvent @t window (`XhrEventM.on` WinEvtHandlers.popState) $ do
    e <- XhrEventM.event
    win <- GHCJS.DOM.currentWindowUnchecked
    location <- Window.getLocation win
    HistoryItem
      <$> P.fmap SerializedScriptValue (PopStateEvent.getState e)
      <*> getLocationUri location
  let f :: (Bool, Maybe a) -> These a () -> (Maybe (Bool, Maybe a), Maybe a)
      f (switched, acc) = \case
        This change
          | switched  -> (Nothing, Just change)
          | P.otherwise -> (Just (switched, Just change), Nothing)
        That ()          -> (Just (True, Nothing), acc)
        These change ()  -> (Just (True, Nothing), Just change)
  -- Accumulate the events before switchover.
  (_, cmd') <- RClass.mapAccumMaybeB f (False, Nothing)
                 (Align.align (R.leftmost [Left <$> runCmd, Right <$> itemSetExternal']) switchover)
  let (itemSetInternal', itemSetExternal) = RClass.fanEither cmd'
  itemSetInternal <- RPE.performEvent $ R.ffor itemSetInternal' $ \cmd -> liftIO $ runJSM
    (do
        runHistoryCommand history cmd
        location <- Window.getLocation window
        HistoryItem <$> DOMHistory.getState history <*> getLocationUri location)
    ctx
  currentHistoryItem <- HH.holdDynE @t item0 (R.leftmost [itemSetInternal, itemSetExternal])
  pure (currentHistoryItem, itemSetExternal)

popHistoryState
  :: forall t es
   . ( R.Reflex t
     , JSM' :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     )
  => R.Event t ()
  -> Eff es ()
popHistoryState evt = do
  ctx <- askJSM
  RPE.performEvent_ $ R.ffor evt $ \_ -> liftIO $ runJSM
    (do
        window  <- GHCJS.DOM.currentWindowUnchecked
        history <- Window.getHistory window
        DOMHistory.back history)
    ctx

-- =====================================================================
-- Widget.Resize — DOM-driven resize event detector.
--
-- Direct port of upstream 'Reflex.Dom.Widget.Resize'. The technique
-- (nested scroll-triggered element with expanding/shrinking sentinels)
-- is adapted from @github.com/marcj/css-element-queries@ and detects
-- element resizes without ResizeObserver.
-- =====================================================================

-- | Wrap the given widget in a div and fire an event when it resizes.
--
-- Can cause strange scrollbars to appear in some browsers; hide with
-- CSS pseudo-selectors, e.g. in webkit:
-- @.wrapper *::-webkit-scrollbar { width: 0px; background: transparent; }@
resizeDetector
  :: forall t es a
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.TriggerEvent t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     )
  => Eff es a
  -> Eff es (R.Event t (Maybe Double, Maybe Double), a)
resizeDetector = resizeDetectorWithStyle @t ""

resizeDetectorWithStyle
  :: forall t es a
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.TriggerEvent t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     )
  => Text  -- ^ CSS style string. Must NOT contain the @position@ attribute.
  -> Eff es a  -- ^ Embedded widget
  -> Eff es (R.Event t (Maybe Double, Maybe Double), a)
resizeDetectorWithStyle styleString =
  resizeDetectorWithAttrs @t (Map.singleton (T.pack "style") styleString)

resizeDetectorWithAttrs
  :: forall t es a
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.TriggerEvent t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     )
  => Map Text Text
  -> Eff es a
  -> Eff es (R.Event t (Maybe Double, Maybe Double), a)
resizeDetectorWithAttrs attrs w = do
  let childStyle     = T.pack "position: absolute; left: 0; top: 0;"
      containerAttrs = Map.singleton (T.pack "style")
        (T.pack "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;")
  (parent, (expand, expandChild, shrink, w')) <-
    elAttr' "div"
      (Map.unionWith (<>) attrs
         (Map.singleton (T.pack "style") (T.pack "position: relative;"))) $ do
        w'' <- w
        elAttr "div" containerAttrs $ do
          (expand', (expandChild', _)) <-
            elAttr' "div" containerAttrs $
              elAttr' "div" (Map.singleton (T.pack "style") childStyle) $ pure ()
          (shrink', _) <-
            elAttr' "div" containerAttrs $
              elAttr "div"
                (Map.singleton (T.pack "style") (childStyle <> T.pack "width: 200%; height: 200%;")) $
                pure ()
          pure (expand', expandChild', shrink', w'')
  let p = uncheckedCastTo DOMTypes'.HTMLElement (_element_raw parent)
      reset = do
        let e = uncheckedCastTo DOMTypes'.HTMLElement (_element_raw expand)
            s = _element_raw shrink
        eow <- DOMHTMLElement.getOffsetWidth  e
        eoh <- DOMHTMLElement.getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        Element.setAttribute (_element_raw expandChild) (T.pack "style")
          (childStyle
             <> T.pack "width: "  <> T.pack (P.show ecw) <> T.pack "px;"
             <> T.pack "height: " <> T.pack (P.show ech) <> T.pack "px;")
        esw <- Element.getScrollWidth  e
        Element.setScrollLeft e esw
        esh <- Element.getScrollHeight e
        Element.setScrollTop  e esh
        ssw <- Element.getScrollWidth  s
        Element.setScrollLeft s ssw
        ssh <- Element.getScrollHeight s
        Element.setScrollTop  s ssh
        lastWidth  <- DOMHTMLElement.getOffsetWidth  p
        lastHeight <- DOMHTMLElement.getOffsetHeight p
        pure (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- DOMHTMLElement.getOffsetWidth  p
        poh <- DOMHTMLElement.getOffsetHeight p
        if ds P.== (Just pow, Just poh)
          then pure Nothing
          else fmap Just reset
  pb <- RTime.delay 0 P.=<< HH.getPostBuildE @t
  expandScroll <- wrapDomEvent @t (uncheckedCastTo DOMTypes'.HTMLElement (_element_raw expand))
    (`XhrEventM.on` Events.scroll) $ pure ()
  shrinkScroll <- wrapDomEvent @t (uncheckedCastTo DOMTypes'.HTMLElement (_element_raw shrink))
    (`XhrEventM.on` Events.scroll) $ pure ()
  ctx <- askJSM
  size0 <- RPE.performEvent $ fmap (P.const (liftIO (runJSM reset ctx))) pb
  rec resize <- RPE.performEventAsync $
        fmap (\d cb -> liftIO . cb P.=<< liftIO (runJSM (resetIfChanged d) ctx)) $
          R.tag (R.current dimensions) $ R.leftmost [expandScroll, shrinkScroll]
      dimensions <- HH.holdDynE @t (Nothing, Nothing) $
        R.leftmost [size0, R.fmapMaybe P.id resize]
  pure (R.updated dimensions, w')

-- =====================================================================
-- Widget.Lazy — virtualising list widgets for long collections.
--
-- Direct port of upstream 'Reflex.Dom.Widget.Lazy'. Only child rows
-- near the current scroll position get rendered, so the total DOM cost
-- is proportional to the visible window, not the total list length.
-- =====================================================================

virtualListWithSelection
  :: forall t es k v
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     , Ord k, Eq v
     )
  => R.Dynamic t Int  -- ^ Visible region height (px)
  -> Int              -- ^ Row height (px)
  -> R.Dynamic t Int  -- ^ Total number of items
  -> Int              -- ^ Row index to scroll to on initialisation
  -> R.Event t Int    -- ^ Row indices to scroll to on demand
  -> Text             -- ^ Element tag for the list
  -> R.Dynamic t (Map Text Text)  -- ^ Attributes of the list
  -> Text             -- ^ Element tag for a row
  -> R.Dynamic t (Map Text Text)  -- ^ Attributes of each row
  -> (k -> R.Dynamic t (Maybe v) -> R.Dynamic t Bool -> Eff es ())
      -- ^ Row-child builder
  -> R.Dynamic t (Map k v)  -- ^ The 'Map' of items
  -> (Int -> k)             -- ^ Index → Key
  -> Eff es (R.Dynamic t (Int, Int), R.Event t k)
     -- ^ (Current (index, count) window, selected-row events)
virtualListWithSelection heightPx rowPx maxIndex i0 setI listTag listAttrs rowTag rowAttrs itemBuilder items indexToKey = do
  let totalHeightStyle = fmap (toHeightStyle . (P.* rowPx)) maxIndex
      containerStyle   = fmap toContainer heightPx
      viewportStyle    = fmap toViewport  heightPx
  rec (container, sel) <-
        elDynAttr' @t "div" containerStyle $ elDynAttr @t "div" viewportStyle $ do
          let currentTop = fmap (listWrapperStyle . P.fst) window
              listAttrs' = R.zipDynWith Map.union currentTop listAttrs
          (_, lis) <-
            elDynAttr @t "div" totalHeightStyle $
              elDynAttr' @t listTag listAttrs' $
                selectViewListWithKey_ @t selected itemsInWindow $ \k v s -> do
                  let rowAttrs' = R.zipDynWith Map.union (R.constDyn (toHeightStyle rowPx)) rowAttrs
                  (li, _) <- elDynAttr' @t rowTag rowAttrs' $ itemBuilder k v s
                  pure $ fmap (P.const k) (domEvent Click li)
          pure lis
      selected <- HH.holdDynE @t (indexToKey i0) sel
      pb <- HH.getPostBuildE @t
      scrollPosition <- HH.holdDynE @t 0 $ R.leftmost
        [ P.round <$> domEvent Scroll container
        , fmap (P.const (i0 P.* rowPx)) pb
        ]
      let window = R.zipDynWith (findWindow rowPx) heightPx scrollPosition
          itemsInWindow = R.zipDynWith
            (\(_, (idx, num)) is ->
                Map.fromList $ P.map
                  (\i -> let ix = indexToKey i in (ix, Map.lookup ix is))
                  [idx .. idx P.+ num])
            window items
  postBuild <- HH.getPostBuildE @t
  ctx <- askJSM
  RPE.performEvent_ $ R.ffor (R.leftmost [setI, i0 <$ postBuild]) $ \i ->
    liftIO $ runJSM (Element.setScrollTop (_element_raw container) (i P.* rowPx)) ctx
  let indexAndLength = fmap P.snd window
  pure (indexAndLength, sel)
  where
    toStyleAttr m = Map.singleton (T.pack "style") $
      Map.foldrWithKey (\k v s -> k <> T.pack ":" <> v <> T.pack ";" <> s) T.empty m
    toViewport h = toStyleAttr $
      Map.singleton (T.pack "overflow") (T.pack "auto")
      <> Map.singleton (T.pack "position") (T.pack "absolute")
      <> Map.singleton (T.pack "left") (T.pack "0")
      <> Map.singleton (T.pack "right") (T.pack "0")
      <> Map.singleton (T.pack "height") (T.pack (P.show h) <> T.pack "px")
    toContainer h = toStyleAttr $
      Map.singleton (T.pack "position") (T.pack "relative")
      <> Map.singleton (T.pack "height") (T.pack (P.show h) <> T.pack "px")
    listWrapperStyle top_ = toStyleAttr $
      Map.singleton (T.pack "position") (T.pack "relative")
      <> Map.singleton (T.pack "top") (T.pack (P.show top_) <> T.pack "px")
    toHeightStyle h = toStyleAttr $
      Map.singleton (T.pack "height") (T.pack (P.show h) <> T.pack "px")
      <> Map.singleton (T.pack "overflow") (T.pack "hidden")
    findWindow sizeIncrement windowSize startingPosition =
      let (startingIndex, topOffsetPx) = startingPosition `divMod'` sizeIncrement
          topPx = startingPosition P.- topOffsetPx
          numItems = windowSize `P.div` sizeIncrement P.+ 1
          preItems = P.min startingIndex numItems
      in ( topPx P.- preItems P.* sizeIncrement
         , (startingIndex P.- preItems, preItems P.+ numItems P.* 2)
         )

virtualList
  :: forall t es k v a
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     , Ord k
     )
  => R.Dynamic t Int          -- ^ Visible region height (px)
  -> Int                      -- ^ Row height (px)
  -> R.Dynamic t Int          -- ^ Total items
  -> Int                      -- ^ Initial scroll row
  -> R.Event t Int            -- ^ Scroll-to events
  -> (k -> Int)               -- ^ Key → Index
  -> Map k v                  -- ^ Initial items
  -> R.Event t (Map k (Maybe v))  -- ^ Update event
  -> (k -> v -> R.Event t v -> Eff es a)  -- ^ Row builder
  -> Eff es (R.Dynamic t (Int, Int), R.Dynamic t (Map k a))
virtualList heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder = do
  let virtualH       = mkVirtualHeight <$> maxIndex
      containerStyle = fmap mkContainer heightPx
      viewportStyle  = fmap mkViewport  heightPx
  pb <- HH.getPostBuildE @t
  rec (viewport, result) <-
        elDynAttr @t "div" containerStyle $
          elDynAttr' @t "div" viewportStyle $
            elDynAttr @t "div" virtualH $
              listWithKeyShallowDiff @t items0 itemsUpdate $ \k v e ->
                elAttr "div" (mkRow k) $ itemBuilder k v e
      scrollPosition <- HH.holdDynE @t 0 $ R.leftmost
        [ P.round <$> domEvent Scroll viewport
        , fmap (P.const (i0 P.* rowPx)) pb
        ]
      let window = R.zipDynWith (findWindow rowPx) heightPx scrollPosition
  ctx <- askJSM
  RPE.performEvent_ $ R.ffor (R.leftmost [setI, i0 <$ pb]) $ \i ->
    liftIO $ runJSM (Element.setScrollTop (_element_raw viewport) (i P.* rowPx)) ctx
  uniqWindow <- RD.holdUniqDyn window
  pure (uniqWindow, result)
  where
    toStyleAttr m = Map.singleton (T.pack "style") $
      Map.foldrWithKey (\k v s -> k <> T.pack ":" <> v <> T.pack ";" <> s) T.empty m
    mkViewport h = toStyleAttr $
      Map.singleton (T.pack "overflow") (T.pack "auto")
      <> Map.singleton (T.pack "position") (T.pack "absolute")
      <> Map.singleton (T.pack "left") (T.pack "0")
      <> Map.singleton (T.pack "right") (T.pack "0")
      <> Map.singleton (T.pack "height") (T.pack (P.show h) <> T.pack "px")
    mkContainer h = toStyleAttr $
      Map.singleton (T.pack "position") (T.pack "relative")
      <> Map.singleton (T.pack "height") (T.pack (P.show h) <> T.pack "px")
    mkVirtualHeight h = let h' = h P.* rowPx
                        in toStyleAttr $
      Map.singleton (T.pack "height") (T.pack (P.show h') <> T.pack "px")
      <> Map.singleton (T.pack "overflow") (T.pack "hidden")
      <> Map.singleton (T.pack "position") (T.pack "relative")
    mkRow k = toStyleAttr $
      Map.singleton (T.pack "height") (T.pack (P.show rowPx) <> T.pack "px")
      <> Map.singleton (T.pack "top") (T.pack (P.show (keyToIndex k P.* rowPx)) <> T.pack "px")
      <> Map.singleton (T.pack "position") (T.pack "absolute")
      <> Map.singleton (T.pack "width") (T.pack "100%")
    findWindow sizeIncrement windowSize startingPosition =
      let (startingIndex, _) = startingPosition `divMod'` sizeIncrement
          numItems = (windowSize P.+ sizeIncrement P.- 1) `P.div` sizeIncrement
      in (startingIndex, numItems)

virtualListBuffered
  :: forall t es k v a
   . ( R.Reflex t, MonadFix (Eff es)
     , JSM' :> es, IOE :> es
     , HH.PostBuild t :> es
     , HH.PerformEventM (RH.HostFrame t) t :> es
     , HH.Adjustable t :> es
     , HH.NotReady t :> es
     , HH.Hold t :> es
     , HH.Sample t :> es
     , RPE.PerformEvent t (Eff es)
     , MonadIO (RPE.Performable (Eff es))
     , DomBuilderE t GhcjsDomSpace :> es
     , HasDocumentE GhcjsDomSpace :> es
     , DomBuilderSpaceE es ~ GhcjsDomSpace
     , DomBuilderTimelineE es ~ t
     , Ord k
     )
  => Int
  -> R.Dynamic t Int
  -> Int
  -> R.Dynamic t Int
  -> Int
  -> R.Event t Int
  -> (k -> Int)
  -> Map k v
  -> R.Event t (Map k (Maybe v))
  -> (k -> v -> R.Event t v -> Eff es a)
  -> Eff es (R.Event t (Int, Int), R.Dynamic t (Map k a))
virtualListBuffered buffer heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder = do
  (win, m) <- virtualList @t heightPx rowPx maxIndex i0 setI keyToIndex items0 itemsUpdate itemBuilder
  pb <- HH.getPostBuildE @t
  let extendWin o l = (P.max 0 (o P.- l P.* (buffer P.- 1) `P.div` 2), l P.* buffer)
  rec let winHitEdge = R.attachWithMaybe
            (\(oldOffset, oldLimit) (winOffset, winLimit) ->
                if winOffset P.> oldOffset P.&& winOffset P.+ winLimit P.< oldOffset P.+ oldLimit
                  then Nothing
                  else Just (extendWin winOffset winLimit))
            (R.current winBuffered) (R.updated win)
      winBuffered <- HH.holdDynE @t (0, 0) $ R.leftmost
        [ winHitEdge
        , RD.attachPromptlyDynWith (\(x, y) _ -> extendWin x y) win pb
        ]
  pure (R.updated winBuffered, m)

-- =====================================================================
-- EventM helpers that variant B's Reflex.Dom.Builder.Immediate defines
-- but does NOT export (they're used only by its internal
-- 'defaultDomEventHandler'). Kept here verbatim so our export surface
-- stays complete.
-- =====================================================================

{-# INLINABLE getPasteData #-}
getPasteData :: XhrEventM.EventM e ClipboardEvent (Maybe Text)
getPasteData = do
  e <- XhrEventM.event
  mdt <- ClipboardEvent.getClipboardData e
  case mdt of
    Nothing -> pure Nothing
    Just dt -> Just <$> DataTransfer.getData dt (T.pack "text")

{-# INLINABLE getWheelEvent #-}
getWheelEvent :: XhrEventM.EventM e WheelEvent WheelEventResult
getWheelEvent = do
  e <- XhrEventM.event
  dx <- WheelEvent.getDeltaX e
  dy <- WheelEvent.getDeltaY e
  dz <- WheelEvent.getDeltaZ e
  dm <- WheelEvent.getDeltaMode e
  pure WheelEventResult
    { _wheelEventResult_deltaX = dx
    , _wheelEventResult_deltaY = dy
    , _wheelEventResult_deltaZ = dz
    , _wheelEventResult_deltaMode = case dm of
        0 -> DeltaPixel
        1 -> DeltaLine
        2 -> DeltaPage
        _ -> DeltaPixel
    }
