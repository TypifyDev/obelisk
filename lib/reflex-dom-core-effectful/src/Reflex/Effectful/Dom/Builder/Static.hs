{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Honest Eff-native port of @Reflex.Dom.Builder.Static@.
--
-- Replaces the previous MTL @StaticDomBuilderT@ transformer + all its
-- typeclass instances with a single 'DomBuilder t StaticDomSpace'
-- interpreter, 'runDomBuilderStatic', and a companion
-- 'HasDocument StaticDomSpace' interpreter, 'runHasDocumentStatic'.
--
-- The static byte-accumulator lives on the effect row as
-- @State [Behavior t Builder] :> es@ (in REVERSE assembly order, per
-- upstream). Ambient config lives as @Reader (StaticDomBuilderEnv t) :> es@
-- and is scoped through 'Effectful.Reader.Static.local' rather than a
-- fresh @runReader@ frame — 'local' preserves the effect row shape
-- exactly, which is required inside an interpreter continuation.
--
-- Child scopes save/reset/restore the state list around the child
-- body (same pattern @StateT@ would use).
--
-- 'renderStatic' bootstraps the effect stack and returns
-- @(a, ByteString)@; the byte output is semantically identical to
-- upstream @Reflex.Dom.Builder.Static.renderStatic@ for the same
-- widget input (same escaping rules, same reverse-order concat, same
-- void-element handling, same @option@ selected-attr injection).
module Reflex.Effectful.Dom.Builder.Static
  ( StaticDomBuilderEnv (..)
  , StaticDomSpace
  , StaticDomEvent
  , StaticDomHandler (..)
  , StaticEventSpec (..)
  , StaticDomTimeline
  , StaticE
  , replaceStart
  , replaceEnd
    -- * Interpreters
  , runDomBuilderStatic
  , runHasDocumentStatic
  , runTriggerEventStatic
  , runNotReadyStatic
  , runAdjustableStatic
    -- * Entrypoint
  , renderStatic
  ) where

import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedText)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Lens ((^.), (%~), (&), (.~))
import Control.Monad (forM, join, void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Default (Default (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose (Compose (..), getCompose)
import Data.Functor.Constant (Constant (..))
import Data.GADT.Compare (GCompare)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Map.Misc (applyMap)
import Data.Maybe (fromMaybe)
import Data.Patch (Patch, PatchTarget)
import Data.Patch.DMap (mapPatchDMap)
import Data.Patch.DMapWithMove (mapPatchDMapWithMove)
import qualified Data.Set as Set
import Data.Functor.Misc (WrapArg (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import GHC.Generics (Generic)

import Effectful (Eff, IOE, (:>), runEff)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)
import Effectful.Reader.Static (Reader, ask, asks, local, runReader)
import Effectful.State.Static.Local (State, evalState, get, put, modify)

import Reflex.Class
  ( Behavior
  , Dynamic
  , Event
  , EventSelector (..)
  , Reflex
  , constant
  , current
  , currentIncremental
  , ffor
  , fmapCheap
  , never
  , pull
  )
import Reflex.Host.HeadlessEffectful
  ( Adjustable (..)
  , runWithReplaceE
  , traverseIntMapWithKeyWithAdjustE
  , traverseDMapWithKeyWithAdjustE
  , traverseDMapWithKeyWithAdjustWithMoveE
  , Hold, hold, holdDyn, holdIncremental, runHold
  , NotReady (..)
  , Sample, sample, runSample
  , runTimeline
  , TriggerEvent (..)
  )
-- 'KnownTimeline' referenced but not defined in the current port; removed.
-- Callsites that use it will need their signatures updated.
import Reflex.Effectful.Dom.Builder.Class
  ( DomBuilderTimeline, DomBuilder (..), DomBuilderSpace, HasDocument (..)
  , DomSpace (..)
  , TextNode (..), TextNodeConfig (..), textNodeConfig_initialContents, textNodeConfig_setContents
  , CommentNode (..), CommentNodeConfig (..), commentNodeConfig_initialContents
  , Element (..), ElementConfig (..)
  , elementConfig_initialAttributes, elementConfig_modifyAttributes
  , InputElement (..), InputElementConfig (..), inputElementConfig_initialValue
  , SelectElement (..), SelectElementConfig (..), selectElementConfig_initialValue
  , TextAreaElement (..), TextAreaElementConfig (..), textAreaElementConfig_initialValue
  , AttributeName (..), EventName, EventTag
  )
import Reflex.Spider.Internal (SpiderTimeline)
import Reflex.Dynamic (foldDyn)
import Reflex.Host.Class (newFanEventWithTrigger)
-- TriggerEvent (..) is imported from Reflex.Host.HeadlessEffectful (the effectful port). The MTL Reflex.TriggerEvent.Class variant is no longer needed here.
import Reflex.Spider.Internal (Global)
-- 'sample' was 'pull . const . sample' in old reflex; use 'pull . sample' inline.
-- 'KnownTimeline' referenced but not defined in the current port — see below.

-- Types + record selectors relocated to the effectful import above.
-- (Nothing else needed from Reflex.Dom.Core here.)

---------------------------------------------------------------------------
-- StaticDomSpace + supporting types
---------------------------------------------------------------------------

-- | Timeline used for static rendering. Pivoted from upstream's
-- @Spider@ to @SpiderTimeline Global@: the honest port replaces reflex's
-- Spider runtime with the native reflex-effectful timeline.
type StaticDomTimeline = SpiderTimeline Global

data StaticDomBuilderEnv t = StaticDomBuilderEnv
  { _staticDomBuilderEnv_shouldEscape :: !Bool
  , _staticDomBuilderEnv_selectValue :: !(Maybe (Behavior t Text))
  , _staticDomBuilderEnv_nextRunWithReplaceKey :: !(IORef Int)
  }

data StaticDomSpace

data StaticDomEvent (a :: k)

data StaticDomHandler (a :: k) (b :: k) = StaticDomHandler

data StaticEventSpec (er :: EventTag -> Type) = StaticEventSpec deriving (Generic)

instance Default (StaticEventSpec er)

instance DomSpace StaticDomSpace where
  type EventSpec StaticDomSpace = StaticEventSpec
  type RawDocument StaticDomSpace = ()
  type RawTextNode StaticDomSpace = ()
  type RawCommentNode StaticDomSpace = ()
  type RawElement StaticDomSpace = ()
  type RawInputElement StaticDomSpace = ()
  type RawTextAreaElement StaticDomSpace = ()
  type RawSelectElement StaticDomSpace = ()
  addEventSpecFlags _ _ _ _ = StaticEventSpec

---------------------------------------------------------------------------
-- Effect row required by the static interpreters
---------------------------------------------------------------------------

-- | Effect row consumed by 'runDomBuilderStatic'. Pinned to
-- @t ~ StaticDomTimeline@ so 'Hold' / 'Sample' / fan-event allocation
-- resolve to their concrete @SpiderTimeline Global@ interpreters.
type StaticE t es =
  ( t ~ StaticDomTimeline
  , Reflex t
  , Reader (StaticDomBuilderEnv t) :> es
  , State [Behavior t Builder] :> es
  , Hold t :> es
  , Sample t :> es
  , IOE :> es
  )

---------------------------------------------------------------------------
-- HasDocument interpreter
---------------------------------------------------------------------------

-- | @RawDocument StaticDomSpace ~ ()@ — 'askDocument' returns unit.
{-# INLINABLE runHasDocumentStatic #-}
runHasDocumentStatic
  :: forall es a
   . Eff (HasDocument StaticDomSpace : es) a
  -> Eff es a
runHasDocumentStatic = interpret $ \_ -> \case
  AskDocument -> pure ()

---------------------------------------------------------------------------
-- DomBuilder interpreter — StaticDomSpace
---------------------------------------------------------------------------

-- | Interpret @DomBuilder t StaticDomSpace@ over the ambient reader /
-- state / FRP row. Each arm ports the corresponding case of upstream's
-- @instance DomBuilder t (StaticDomBuilderT t m)@ verbatim in
-- semantics; only the location of the state (effect row vs. transformer
-- stack) differs.
{-# INLINABLE runDomBuilderStatic #-}
runDomBuilderStatic
  :: forall t es a
   . (StaticE t es)
  => Eff (DomBuilder t StaticDomSpace : es) a
  -> Eff es a
runDomBuilderStatic = interpret $ \env -> \case
  DB_TextNode cfg -> handleTextNode cfg
  DB_CommentNode cfg -> handleCommentNode cfg
  DB_Element tag cfg body ->
    localSeqUnlift env $ \unlift ->
      handleElement tag cfg (unlift body)
  DB_InputElement cfg -> handleInputElement cfg
  DB_TextAreaElement cfg -> handleTextAreaElement cfg
  DB_SelectElement cfg body ->
    localSeqUnlift env $ \unlift ->
      handleSelectElement cfg (unlift body)
  DB_PlaceRawElement () -> pure ()
  DB_WrapRawElement () _ ->
    pure (Element (EventSelector (const never)) ())

-- Note on 'liftIO (unlift body)': 'localSeqUnliftIO' hands us an
-- unlifter that reduces @Eff senderEs x@ to @IO x@ under the same
-- interpreter scope; 'liftIO' then re-lifts that IO into our
-- interpreter's row (which has @IOE :> es@ via 'StaticE'). Sequential
-- unlift matches the straight-line assembly order of DOM children —
-- children run in order and are never invoked concurrently with the
-- parent (mirrors upstream's ReaderT/StateT-style scoping).

---------------------------------------------------------------------------
-- Per-constructor helpers (≤10 lines per arm)
---------------------------------------------------------------------------

-- | Emit a text node's byte behavior into the accumulator.
handleTextNode
  :: forall t es
   . (StaticE t es)
  => TextNodeConfig t
  -> Eff es (TextNode StaticDomSpace t)
handleTextNode (TextNodeConfig initial mSet) = do
  escape <- escaper
  b <- case mSet of
    Nothing -> pure (constant (escape initial))
    Just ev -> hold @t (escape initial) (fmapCheap escape ev)
  modify @[Behavior t Builder] (b :)
  pure (TextNode ())

-- | Emit a comment node's byte behavior into the accumulator.
handleCommentNode
  :: forall t es
   . (StaticE t es)
  => CommentNodeConfig t
  -> Eff es (CommentNode StaticDomSpace t)
handleCommentNode (CommentNodeConfig initial mSet) = do
  escape <- escaper
  inner <- case mSet of
    Nothing -> pure (constant (escape initial))
    Just ev -> hold @t (escape initial) (fmapCheap escape ev)
  modify @[Behavior t Builder] (fmap (\c -> "<!--" <> c <> "-->") inner :)
  pure (CommentNode ())

-- | Read @_staticDomBuilderEnv_shouldEscape@ and pick the matching
-- text-to-Builder converter.
escaper :: forall t es. (StaticE t es) => Eff es (Text -> Builder)
escaper = do
  shouldEscape <- asks @(StaticDomBuilderEnv t) _staticDomBuilderEnv_shouldEscape
  pure $ if shouldEscape
    then fromHtmlEscapedText
    else byteString . encodeUtf8

-- | Enter a fresh child scope: save the outer state list, reset to
-- @[]@, run @child@ under a locally-modified reader env (fresh escape
-- rule based on the tag's void/no-escape classification, no ambient
-- select value), extract the child's accumulated bytes, restore the
-- outer state list.
withChildBuilder
  :: forall t es a
   . (StaticE t es)
  => Text
  -> Eff es a
  -> Eff es (a, Behavior t Builder)
withChildBuilder tag child = do
  outerAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] []
  let noEscape = Set.fromList ["style", "script"]
      shouldEscape = tag `Set.notMember` noEscape
      resetEnv old = old
        { _staticDomBuilderEnv_shouldEscape = shouldEscape
        , _staticDomBuilderEnv_selectValue = Nothing
        }
  result <- local @(StaticDomBuilderEnv t) resetEnv child
  childAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] outerAcc
  pure (result, mconcat (reverse childAcc))

-- | Emit an element: allocate the fan-event, recurse into the child
-- scope, splice open/close tags around the child bytes (or emit a
-- self-closing tag for HTML void elements).
handleElement
  :: forall t er es a
   . (StaticE t es)
  => Text
  -> ElementConfig er t StaticDomSpace
  -> Eff es a
  -> Eff es (Element er StaticDomSpace t, a)
handleElement tag cfg child = do
  es <- newFanEventStatic @t @er
  (result, innerHtml) <- withChildBuilder tag child
  emitElementBytes tag cfg innerHtml
  pure (Element es (), result)

-- | Allocate a fresh no-op fan-event trigger. Static elements never
-- dispatch subscribers, but the shape must be a real 'EventSelector'.
newFanEventStatic
  :: forall t er es
   . (StaticE t es)
  => Eff es (EventSelector t (WrapArg er EventName))
newFanEventStatic =
  newFanEventWithTrigger (\_ _ -> pure (pure ()))

-- | Splice open/close bytes around the child bytes; handle HTML void
-- elements as self-closing; inject a @selected@ attribute for
-- @<option>@ children of a @<select>@ whose ambient value matches.
emitElementBytes
  :: forall t er es
   . (StaticE t es)
  => Text
  -> ElementConfig er t StaticDomSpace
  -> Behavior t Builder
  -> Eff es ()
emitElementBytes tag cfg innerHtml = do
  attrs2 <- attrsBehavior tag cfg
  let tagBS = encodeUtf8 tag
  if Set.member tag voidElements
    then modify @[Behavior t Builder] $ (:) $
      mconcat [constant ("<" <> byteString tagBS), attrs2, constant (byteString " />")]
    else do
      let open = mconcat [constant ("<" <> byteString tagBS), attrs2, constant (byteString ">")]
          close = constant $ byteString $ "</" <> tagBS <> ">"
      modify @[Behavior t Builder] $ (:) $ mconcat [open, innerHtml, close]

-- | Fold the modify-attributes event over the initial attributes;
-- inject the @selected@ attribute at matching @option@ children of a
-- @select@; serialize to a byte 'Builder'.
attrsBehavior
  :: forall t er es
   . (StaticE t es)
  => Text
  -> ElementConfig er t StaticDomSpace
  -> Eff es (Behavior t Builder)
attrsBehavior tag cfg = do
  attrs0 <- foldDyn applyMap (cfg ^. elementConfig_initialAttributes)
    (fromMaybe never (_elementConfig_modifyAttributes cfg))
  selectValue <- asks @(StaticDomBuilderEnv t) _staticDomBuilderEnv_selectValue
  let attrs1 = case (tag, selectValue) of
        ("option", Just sv) -> pull $ do
          a <- sample (current attrs0)
          v <- sample sv
          pure (addSelectedAttr a v)
        _ -> current attrs0
  pure $ fmap (mconcat . fmap (\(k, v) -> " " <> toAttr k v) . Map.toList) attrs1

-- | Serialize a single @key="value"@ attribute pair to a 'Builder'.
toAttr :: AttributeName -> Text -> Builder
toAttr (AttributeName _mns k) v =
  byteString (encodeUtf8 k)
    <> byteString "=\""
    <> fromHtmlEscapedText v
    <> byteString "\""

-- | If the parent select's current value matches this option's value,
-- inject a @selected@ attribute; otherwise strip any stale @selected@.
addSelectedAttr
  :: Map.Map AttributeName Text -> Text -> Map.Map AttributeName Text
addSelectedAttr attrs sel = case Map.lookup "value" attrs of
  Just v | v == sel -> attrs <> Map.singleton "selected" ""
  _ -> Map.delete "selected" attrs

-- | HTML void elements — no closing tag.
voidElements :: Set.Set Text
voidElements = Set.fromList
  [ "area", "base", "br", "col", "command", "embed", "hr", "img"
  , "input", "keygen", "link", "meta", "param", "source", "track", "wbr"
  ]

---------------------------------------------------------------------------
-- Input / textarea / select
---------------------------------------------------------------------------

-- | Emit an @<input>@ element, threading the initial/updated value +
-- checked state into the element's attribute stream.
handleInputElement
  :: forall t er es
   . (StaticE t es)
  => InputElementConfig er t StaticDomSpace
  -> Eff es (InputElement er StaticDomSpace t)
handleInputElement cfg = do
  let adjustedConfig = inputConfigWithValueAttrs cfg
  (e, ()) <- handleElement @t @er "input" adjustedConfig (pure ())
  v <- valueDyn cfg
  c <- checkedDyn cfg
  pure InputElement
    { _inputElement_value = v
    , _inputElement_checked = c
    , _inputElement_checkedChange = never
    , _inputElement_input = never
    , _inputElement_hasFocus = pure False
    , _inputElement_element = e
    , _inputElement_raw = ()
    , _inputElement_files = pure mempty
    }

-- | Merge @value@ / @checked@ config knobs into the underlying
-- element config so serialization emits them as attributes. The lens
-- 'elementConfig_modifyAttributes' presents the modify event as an
-- 'Event t' (unwrapping the internal 'Maybe' with 'never' on the empty
-- side), so 'setUpdatedValue' / 'setUpdatedChecked' operate on
-- @Event t ...@ directly and merge in any @setValue@ / @setChecked@
-- knobs via '<>'.
inputConfigWithValueAttrs
  :: forall t er
   . (Reflex t)
  => InputElementConfig er t StaticDomSpace
  -> ElementConfig er t StaticDomSpace
inputConfigWithValueAttrs cfg =
  _inputElementConfig_elementConfig cfg
    & elementConfig_initialAttributes %~ setInitialValue . setInitialChecked
    & elementConfig_modifyAttributes %~ setUpdatedValue . setUpdatedChecked
  where
    setInitialValue = Map.insert "value" (_inputElementConfig_initialValue cfg)
    setInitialChecked = if _inputElementConfig_initialChecked cfg
      then Map.insert "checked" "checked"
      else id
    setUpdatedValue
      :: Event t (Map.Map AttributeName (Maybe Text))
      -> Event t (Map.Map AttributeName (Maybe Text))
    setUpdatedValue updatedAttrs = case _inputElementConfig_setValue cfg of
      Nothing -> updatedAttrs
      Just e -> (Map.singleton "value" . Just <$> e) <> updatedAttrs
    setUpdatedChecked
      :: Event t (Map.Map AttributeName (Maybe Text))
      -> Event t (Map.Map AttributeName (Maybe Text))
    setUpdatedChecked updatedAttrs = case _inputElementConfig_setChecked cfg of
      Nothing -> updatedAttrs
      Just e -> (Map.singleton "checked" (Just "checked") <$ e) <> updatedAttrs

-- | Track the input's current value as a 'Dynamic'.
valueDyn
  :: forall t er es
   . (StaticE t es)
  => InputElementConfig er t StaticDomSpace
  -> Eff es (Dynamic t Text)
valueDyn cfg = case _inputElementConfig_setValue cfg of
  Nothing -> pure (pure (cfg ^. inputElementConfig_initialValue))
  Just ev -> holdDyn @t (cfg ^. inputElementConfig_initialValue) ev

-- | Track the input's current @checked@ state as a 'Dynamic'.
checkedDyn
  :: forall t er es
   . (StaticE t es)
  => InputElementConfig er t StaticDomSpace
  -> Eff es (Dynamic t Bool)
checkedDyn cfg = case _inputElementConfig_setChecked cfg of
  Nothing -> pure (pure (_inputElementConfig_initialChecked cfg))
  Just ev -> holdDyn @t (_inputElementConfig_initialChecked cfg) ev

-- | Emit a @<textarea>@ element with the initial value emitted as a
-- child text node (matches upstream).
handleTextAreaElement
  :: forall t er es
   . (StaticE t es)
  => TextAreaElementConfig er t StaticDomSpace
  -> Eff es (TextAreaElement er StaticDomSpace t)
handleTextAreaElement cfg = do
  (e, ()) <- handleElement @t @er "textarea"
    (_textAreaElementConfig_elementConfig cfg) $
      void $ handleTextNode @t $
        def & textNodeConfig_initialContents .~ _textAreaElementConfig_initialValue cfg
            & textNodeConfig_setContents .~ fromMaybe never (_textAreaElementConfig_setValue cfg)
  v <- case _textAreaElementConfig_setValue cfg of
    Nothing -> pure (pure (cfg ^. textAreaElementConfig_initialValue))
    Just ev -> holdDyn @t (cfg ^. textAreaElementConfig_initialValue) ev
  pure TextAreaElement
    { _textAreaElement_value = v
    , _textAreaElement_input = never
    , _textAreaElement_hasFocus = pure False
    , _textAreaElement_element = e
    , _textAreaElement_raw = ()
    }

-- | Emit a @<select>@ element. Mirrors upstream
-- @Reflex.Dom.Builder.Static.selectElement@ byte-for-byte.
--
-- Upstream uses a DOUBLE-scoping pattern (outer 'element' scope +
-- inner @runStaticDomBuilderT@ with a fresh env) so the child body
-- runs under @StaticDomBuilderEnv False (Just (current v)) nextKey@ —
-- i.e. @shouldEscape = False@ AND the ambient select value installed.
-- The child's accumulated bytes are then re-pushed onto the element's
-- accumulator via an explicit @modify $ (:) innerHtml@ so 'handleElement'
-- picks them up when it splices open/close tags.
--
-- We CANNOT reuse 'handleElement'\''s 'withChildBuilder' helper here:
-- 'withChildBuilder' derives @shouldEscape@ from the tag
-- (@tag \`notMember\` {"style","script"}@), which for @"select"@ yields
-- @True@ — diverging from upstream's @False@. Instead we do the
-- save/reset/local/restore dance manually with the exact env upstream
-- installs, then hand the captured bytes to 'emitElementBytes' just
-- like 'handleElement' would.
handleSelectElement
  :: forall t er es a
   . (StaticE t es)
  => SelectElementConfig er t StaticDomSpace
  -> Eff es a
  -> Eff es (SelectElement er StaticDomSpace t, a)
handleSelectElement cfg child = do
  v <- holdDyn @t (cfg ^. selectElementConfig_initialValue)
    (fromMaybe never (_selectElementConfig_setValue cfg))
  es <- newFanEventStatic @t @er
  -- Manual child scope matching upstream's inner runStaticDomBuilderT:
  -- save the outer accumulator, reset to [], run child under
  -- @shouldEscape = False@ + @selectValue = Just (current v)@,
  -- capture child bytes, restore outer accumulator, then re-push the
  -- child bytes so 'emitElementBytes' can splice them.
  outerAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] []
  let selectEnv old = old
        { _staticDomBuilderEnv_shouldEscape = False
        , _staticDomBuilderEnv_selectValue = Just (current v)
        }
  result <- local @(StaticDomBuilderEnv t) selectEnv child
  childAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] outerAcc
  let innerHtml = mconcat (reverse childAcc)
  -- Upstream does @modify $ (:) innerHtml@ inside the element's inner
  -- StaticDomBuilderT scope, then the enclosing 'element' splices
  -- open/close around that inner state's accumulated bytes. Net effect:
  -- @innerHtml@ ends up between the open and close tags spliced onto
  -- the outer accumulator. We produce the same net effect by handing
  -- @innerHtml@ directly to 'emitElementBytes', which performs exactly
  -- that open/close splice against the outer accumulator.
  emitElementBytes @t @er "select"
    (_selectElementConfig_elementConfig cfg) innerHtml
  let e = Element es ()
      wrapped = SelectElement
        { _selectElement_value = v
        , _selectElement_change = never
        , _selectElement_hasFocus = pure False
        , _selectElement_element = e
        , _selectElement_raw = ()
        }
  pure (wrapped, result)

---------------------------------------------------------------------------
-- Run-with-replace markers
---------------------------------------------------------------------------

-- | Emit a start marker and return the fresh key.
replaceStart
  :: forall t es
   . (StaticE t es)
  => StaticDomBuilderEnv t
  -> Eff es Text
replaceStart env = do
  str <- liftIO $ show <$>
    atomicModifyIORef (_staticDomBuilderEnv_nextRunWithReplaceKey env)
      (\k -> (succ k, k))
  let key = "-" <> T.pack str
  _ <- handleCommentNode @t $
    def & commentNodeConfig_initialContents .~ ("replace-start" <> key)
  pure key

-- | Emit the matching end marker.
replaceEnd
  :: forall t es
   . (StaticE t es)
  => Text
  -> Eff es ()
replaceEnd key = void $ handleCommentNode @t $
  def & commentNodeConfig_initialContents .~ ("replace-end" <> key)

---------------------------------------------------------------------------
-- renderStatic bootstrap
---------------------------------------------------------------------------

-- | Render a widget to a UTF-8 HTML byte string. Semantically
-- byte-for-byte equivalent to upstream 'Reflex.Dom.Builder.Static.renderStatic'
-- for the same widget input.
--
-- Stack composition:
--
-- @
--   runEff
--     . runHold                          -- Hold t
--     . runSample                        -- Sample t
--     . runReader env0                   -- Reader (StaticDomBuilderEnv t)
--     . evalState []                     -- State [Behavior t Builder]
--     . runHasDocumentStatic             -- HasDocument StaticDomSpace
--     . runDomBuilderStatic              -- DomBuilder t StaticDomSpace
--     $ (widget >>= extractBytes)
-- @
--
-- The bytes are extracted inside the pipeline (before Sample/Hold pop)
-- by sampling the reverse-mconcat of the accumulator behavior.
--
-- Note: this bootstrap deliberately does NOT stack 'PostBuild' /
-- 'PerformEvent' / 'TriggerEvent' / 'NotReady' / 'Adjustable' — those
-- are consumer concerns. Widgets that require those rows compose the
-- appropriate 'run*' interpreters between 'renderStatic' and their
-- widget body.
{-# INLINABLE renderStatic #-}
renderStatic
  :: (forall es.
        ( DomBuilder StaticDomTimeline StaticDomSpace :> es
        , HasDocument StaticDomSpace :> es
        , Hold StaticDomTimeline :> es
        , Sample StaticDomTimeline :> es
        , IOE :> es
        , DomBuilderTimeline es ~ StaticDomTimeline
        , DomBuilderSpace    es ~ StaticDomSpace
        ) => Eff es a)
  -> IO (a, ByteString)
renderStatic widget = do
  nextKey <- newIORef 0
  let env0 :: StaticDomBuilderEnv StaticDomTimeline
      env0 = StaticDomBuilderEnv True Nothing nextKey
  (a, bs) <- runEff
    . runHold
    . runSample
    . runTimeline @StaticDomTimeline
    . runReader env0
    . evalState @[Behavior StaticDomTimeline Builder] []
    . runHasDocumentStatic
    . runDomBuilderStatic @StaticDomTimeline
    $ do
        r <- widget
        acc <- get @[Behavior StaticDomTimeline Builder]
        finalBytes <- sample @StaticDomTimeline (mconcat (reverse acc))
        pure (r, finalBytes)
  pure (a, LBS.toStrict (toLazyByteString bs))

---------------------------------------------------------------------------
-- TriggerEvent interpreter — static context (upstream Static.hs L129)
---------------------------------------------------------------------------

-- | Static-mode 'TriggerEvent' semantics: no events ever fire. Every
-- constructor returns a 'never' event + a no-op fire callback, matching
-- upstream:
--
-- @
-- newTriggerEvent = return (never, const $ return ())
-- newTriggerEventWithOnComplete = return (never, \\_ _ -> return ())
-- newEventWithLazyTriggerWithOnComplete _ = return never
-- @
--
-- The port-only 'NewTriggerEventChan' constructor allocates a real
-- 'TChan' (to satisfy the return type) but with a no-op push function
-- and no drain — matching the "never fires" semantics of the other
-- constructors.
{-# INLINABLE runTriggerEventStatic #-}
runTriggerEventStatic
  :: forall t es a
   . (Reflex t, IOE :> es)
  => Eff (TriggerEvent t : es) a
  -> Eff es a
runTriggerEventStatic = interpret $ \_ -> \case
  NewTriggerEvent -> pure (never, const (pure ()))
  NewTriggerEventWithOnComplete -> pure (never, \_ _ -> pure ())
  NewEventWithLazyTriggerWithOnComplete _ -> pure never

---------------------------------------------------------------------------
-- NotReady interpreter — static context (upstream Static.hs L262)
---------------------------------------------------------------------------

-- | Static-mode 'NotReady' semantics: both operations are no-ops
-- (static rendering has no "not-ready" state to wait on). Matches
-- upstream verbatim:
--
-- @
-- notReadyUntil _ = pure ()
-- notReady        = pure ()
-- @
{-# INLINABLE runNotReadyStatic #-}
runNotReadyStatic
  :: forall t es a
   . Eff (NotReady t : es) a
  -> Eff es a
runNotReadyStatic = interpret $ \_ -> \case
  NotReadyUntil _ -> pure ()
  NotReady        -> pure ()

---------------------------------------------------------------------------
-- Adjustable interpreter — static context (upstream Static.hs L174)
---------------------------------------------------------------------------

-- | Static-mode 'Adjustable': dispatches widget-scope constructors to
-- the ambient widget-scope 'Adjustable' handler (@runAdjustable@) with
-- per-child byte-behavior collection, and emits @replace-start@ /
-- @replace-end@ comment markers around each cohort.
--
-- Semantics port of upstream:
--
-- @
-- instance ... => Adjustable t (StaticDomBuilderT t m) where
--   runWithReplaceE a0 a' = do
--     e <- StaticDomBuilderT ask
--     key <- replaceStart e
--     (result0, result') <- lift $ runWithReplaceE
--       (runStaticDomBuilderT a0 e)
--       (flip runStaticDomBuilderT e <$> a')
--     o <- hold (snd result0) $ fmapCheap snd result'
--     StaticDomBuilderT $ modify $ (:) $ join o
--     replaceEnd key
--     return (fst result0, fmapCheap fst result')
--   traverseIntMapWithKeyWithAdjustE     = hoistIntMapWithKeyWithAdjust
--     traverseIntMapWithKeyWithAdjustE
--   traverseDMapWithKeyWithAdjustE       = hoistDMapWithKeyWithAdjust
--     traverseDMapWithKeyWithAdjustE     mapPatchDMap
--   traverseDMapWithKeyWithAdjustWithMoveE = hoistDMapWithKeyWithAdjust
--     traverseDMapWithKeyWithAdjustWithMoveE mapPatchDMapWithMove
-- @
--
-- Design note: the interpreter requires an OUTER
-- @Adjustable t :> es@ handler (typically 'runAdjustable' at the
-- runtime bootstrap). The static interpreter dispatches each widget-scope
-- constructor to the outer handler via the widget-scope send-wrappers
-- ('runWithReplaceE', 'traverseIntMapWithKeyWithAdjustE',
-- 'traverseDMapWithKeyWithAdjustE',
-- 'traverseDMapWithKeyWithAdjustWithMoveE'), threading a per-child
-- byte-accumulator scope through each child body. Native (EventME-shaped)
-- constructors are not part of the static widget surface; if a caller
-- ever sends one through the static interpreter it re-fires against the
-- outer handler unchanged.
{-# INLINABLE runAdjustableStatic #-}
runAdjustableStatic
  :: forall t es a
   . ( t ~ StaticDomTimeline
     , Reflex t
     , Reader (StaticDomBuilderEnv t) :> es
     , State [Behavior t Builder] :> es
     , Hold t :> es
     , Adjustable t :> es
     , IOE :> es
     )
  => Eff (Adjustable t : es) a
  -> Eff es a
runAdjustableStatic = interpret $ \env -> \case
  RunWithReplace a0 a' ->
    localSeqUnlift env $ \unlift ->
      runWithReplaceEStatic @t (unlift a0) (fmap unlift a')
  TraverseIntMapWithKeyWithAdjustOp f im0 im' ->
    localSeqUnlift env $ \unlift ->
      hoistIntMapWithKeyWithAdjustStatic @t
        traverseIntMapWithKeyWithAdjustE
        (\k v -> unlift (f k v))
        im0 im'
  TraverseDMapWithKeyWithAdjustOp f dm0 dm' ->
    localSeqUnlift env $ \unlift ->
      hoistDMapWithKeyWithAdjustStatic @t
        traverseDMapWithKeyWithAdjustE
        mapPatchDMap
        (\k v -> unlift (f k v))
        dm0 dm'
  TraverseDMapWithKeyWithAdjustWithMoveOp f dm0 dm' ->
    localSeqUnlift env $ \unlift ->
      hoistDMapWithKeyWithAdjustStatic @t
        traverseDMapWithKeyWithAdjustWithMoveE
        mapPatchDMapWithMove
        (\k v -> unlift (f k v))
        dm0 dm'

-- | Body of static @runWithReplaceE@ (widget-scope). Emits the
-- @replace-start@ marker, calls 'runWithReplaceE' with each child
-- run under a fresh byte-accumulator scope, holds the per-cohort byte
-- behavior, pushes @join o@ onto the outer accumulator, emits the
-- @replace-end@ marker.
runWithReplaceEStatic
  :: forall t es a b
   . ( t ~ StaticDomTimeline
     , Reader (StaticDomBuilderEnv t) :> es
     , State [Behavior t Builder] :> es
     , Hold t :> es
     , Adjustable t :> es
     , IOE :> es
     )
  => Eff es a
  -> Event t (Eff es b)
  -> Eff es (a, Event t b)
runWithReplaceEStatic a0 a' = do
  e <- ask @(StaticDomBuilderEnv t)
  key <- replaceStart @t e
  (result0, result') <- runWithReplaceE @t
    (runChildBytes @t a0)
    (fmap (runChildBytes @t) a')
  o <- hold @t (snd result0) (fmapCheap snd result')
  modify @[Behavior t Builder] (join o :)
  replaceEnd @t key
  pure (fst result0, fmapCheap fst result')

-- | Port of upstream @hoistIntMapWithKeyWithAdjust@ (Static.hs L197).
-- Delegates to the widget-scope base traversal, wrapping each child in
-- 'runChildBytes' so the child's byte-behavior is returned alongside
-- its result; holds the incremental of byte-behaviors and pushes a
-- @pull@-computed byte-concatenation onto the outer accumulator.
hoistIntMapWithKeyWithAdjustStatic
  :: forall t p a b es
   . ( t ~ StaticDomTimeline
     , Reader (StaticDomBuilderEnv t) :> es
     , State [Behavior t Builder] :> es
     , Hold t :> es
     , IOE :> es
     , Functor p
     , Patch (p (Behavior t Builder))
     , PatchTarget (p (Behavior t Builder)) ~ IntMap (Behavior t Builder)
     )
  => ( forall x
     . (Int -> a -> Eff es x)
    -> IntMap a
    -> Event t (p a)
    -> Eff es (IntMap x, Event t (p x))
     )
     -- ^ Base widget-scope traversal (outer 'Adjustable' handler).
  -> (Int -> a -> Eff es b)
  -> IntMap a
  -> Event t (p a)
  -> Eff es (IntMap b, Event t (p b))
hoistIntMapWithKeyWithAdjustStatic base f im0 im' = do
  (children0, children') <- base
    (\k v -> runChildBytes @t (f k v))
    im0 im'
  let result0 = IntMap.map fst children0
      result' = (fmap . fmap) fst children'
      outputs0 :: IntMap (Behavior t Builder)
      outputs0 = IntMap.map snd children0
      outputs' :: Event t (p (Behavior t Builder))
      outputs' = (fmap . fmap) snd children'
  outputs <- holdIncremental @t outputs0 outputs'
  modify @[Behavior t Builder] $ (:) $ pull $ do
    os <- sample (currentIncremental outputs)
    fmap mconcat $ forM (IntMap.toList os) $ \(_, o) -> sample o
  pure (result0, result')

-- | Port of upstream @hoistDMapWithKeyWithAdjust@ (Static.hs L229).
-- Delegates to the widget-scope base traversal, using
-- @Compose . swap@ to re-shape each child's @(result, Behavior)@ pair
-- into @Compose (Behavior, result')@ so the DMap traversal can carry
-- both; holds the incremental of byte-behaviors and pushes a
-- @pull@-computed byte-concatenation onto the outer accumulator.
hoistDMapWithKeyWithAdjustStatic
  :: forall (k :: Type -> Type) v v' t p es
   . ( t ~ StaticDomTimeline
     , GCompare k
     , Reader (StaticDomBuilderEnv t) :> es
     , State [Behavior t Builder] :> es
     , Hold t :> es
     , IOE :> es
     , PatchTarget (p k (Constant (Behavior t Builder)))
         ~ DMap k (Constant (Behavior t Builder))
     , Patch (p k (Constant (Behavior t Builder)))
     )
  => ( forall vv vv'
     . (forall x. k x -> vv x -> Eff es (vv' x))
    -> DMap k vv
    -> Event t (p k vv)
    -> Eff es (DMap k vv', Event t (p k vv'))
     )
     -- ^ Base widget-scope traversal (outer 'Adjustable' handler).
  -> (forall vv vv'. (forall x. vv x -> vv' x) -> p k vv -> p k vv')
     -- ^ Patch mapper (@mapPatchDMap@ / @mapPatchDMapWithMove@).
  -> (forall x. k x -> v x -> Eff es (v' x))
  -> DMap k v
  -> Event t (p k v)
  -> Eff es (DMap k v', Event t (p k v'))
hoistDMapWithKeyWithAdjustStatic base mapPatch f dm0 dm' = do
  (children0, children') <- base
    (\k v -> fmap (Compose . swap) (runChildBytes @t (f k v)))
    dm0 dm'
  let result0 = DMap.map (snd . getCompose) children0
      result' = ffor children' $ mapPatch (snd . getCompose)
      outputs0 :: DMap k (Constant (Behavior t Builder))
      outputs0 = DMap.map (Constant . fst . getCompose) children0
      outputs' :: Event t (p k (Constant (Behavior t Builder)))
      outputs' = ffor children' $ mapPatch (Constant . fst . getCompose)
  outputs <- holdIncremental @t outputs0 outputs'
  modify @[Behavior t Builder] $ (:) $ pull $ do
    os <- sample (currentIncremental outputs)
    fmap mconcat $ forM (DMap.toList os) $ \(_ :=> Constant o) ->
      sample o
  pure (result0, result')

-- | Run a child widget in a fresh byte-accumulator scope, returning
-- @(childResult, childBytes)@ where @childBytes@ is
-- @mconcat (reverse childAcc)@ — the same shape upstream's
-- @runStaticDomBuilderT@ returns.
--
-- Unlike 'withChildBuilder', this does NOT modify the reader env: the
-- child inherits the parent's escape rule and select context
-- unchanged, matching upstream's @flip runStaticDomBuilderT e@ pattern
-- (which reuses the parent env @e@ verbatim).
runChildBytes
  :: forall t es a
   . ( State [Behavior t Builder] :> es
     , Reflex t
     )
  => Eff es a
  -> Eff es (a, Behavior t Builder)
runChildBytes child = do
  outerAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] []
  result <- child
  childAcc <- get @[Behavior t Builder]
  put @[Behavior t Builder] outerAcc
  pure (result, mconcat (reverse childAcc))
