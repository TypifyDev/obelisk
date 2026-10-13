{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}

-- |
-- Effectful port of @Reflex.Dom.Android.MainWidget@.
--
-- Bootstraps the Android 'WebView' + 'jsaddle' bridge for a Reflex-Dom
-- widget when running as an Android app. Mirrors the MTL upstream at
-- @reflex-dom/src-android/Reflex/Dom/Android/MainWidget.hsc@ shape-for-shape:
-- the FFI bindings, the 'JSaddleCallbacks' marshalling, and the global
-- 'JSExecutor' cache are all preserved verbatim because they interoperate
-- with the Java bridge at
-- @reflex-dom/java/org/reflexfrp/reflexdom/MainWidget.java@ and with the C
-- shim at @reflex-dom/cbits/MainWidget.c@ (both of which use the same
-- @Reflex_Dom_Android_MainWidget_*@ symbol names and JNI signatures).
--
-- The only Effectful-side reshapes:
--
--   * 'withGlobalJSExecutorEff' — 'IO'-in, 'Eff'-out variant of the MTL
--     'withGlobalJSExecutor'. The callback is still @'JSExecutor' -> 'IO' ()@
--     because the executor handle is a raw @jobject@ pointer consumed by
--     'goBack' / 'runJS', neither of which lives in 'JSM'.
--   * 'startMainWidgetEff' — same signature as 'startMainWidget' lifted
--     into 'Eff'. The user-supplied bootstrap is still a @'JSM' ()@
--     because the 'JSContextRef' the surrounding 'runJSM'' interpreter
--     would need does not exist until 'runJavaScript' has finished
--     spinning up the executor. Callers who have their widget in 'Eff'
--     should construct a @'JSM' ()@ that calls 'runJSM'' internally
--     against the ctx captured inside 'runJavaScript'.
--
-- The original 'withGlobalJSExecutor' and 'startMainWidget' remain
-- exported unchanged (as 'IO'-monad functions) so this module is a strict
-- superset of the MTL surface — @Reflex.Dom.Location.Platform@ and any
-- other legacy caller still compile against it.
module Reflex.Effectful.Dom.Android.MainWidget
  ( startMainWidget
  , startMainWidgetEff
  , goBack
  , JSExecutor (..)
  , withGlobalJSExecutor
  , withGlobalJSExecutorEff
  ) where

import           Android.HaskellActivity
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Aeson
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Unsafe            as BSU
import           Data.IORef
import           Data.Monoid
import           Effectful                         (Eff, IOE, (:>))
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Javascript.JSaddle       (JSM)
import           Language.Javascript.JSaddle.Run   (runJavaScript)
import           Language.Javascript.JSaddle.Run.Files (initState, runBatch,
                                                         ghcjsHelpers)
import           System.IO.Unsafe

#include "MainWidget.h"

-- | Bootstrap the Android WebView with the supplied bootstrap JSM
-- computation and load the given URL. The JSM runs against the
-- 'JSContextRef' minted by 'runJavaScript' during the bring-up. Preserves
-- the MTL upstream signature verbatim so existing callers (e.g. the
-- @#if defined(ANDROID)@ branch of @Reflex.Dom.Location.Platform@) can
-- keep using this module unchanged.
startMainWidget :: HaskellActivity -> ByteString -> JSM () -> IO ()
startMainWidget a url jsm = do
  --TODO: Find a way to eventually release this
  executorRef <- newIORef $ error "startMainWidget: executor not created yet"
  let go batch = do
        executor <- readIORef executorRef
        BSU.unsafeUseAsCStringLen (LBS.toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $ \(cstr, len) -> do
          runJS executor cstr (fromIntegral len)
  (processResult, processSyncResult, start) <- runJavaScript go jsm
  callbacks <- new <=< jsaddleCallbacksToPtrs $ JSaddleCallbacks
    { _jsaddleCallbacks_jsaddleStart = void $ forkIO start
    , _jsaddleCallbacks_jsaddleResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> processResult r
    , _jsaddleCallbacks_jsaddleSyncResult = \s -> do
        case decode $ LBS.fromStrict s of
          Nothing -> error $ "jsaddle message decode failed: " <> show s
          Just r -> LBS.toStrict . encode <$> processSyncResult r
    , _jsaddleCallbacks_jsaddleJsData = LBS.toStrict $ ghcjsHelpers <> "\
        \runJSaddleBatch = (function() {\n\
        \ " <> initState <> "\n\
        \ return function(batch) {\n\
        \ " <> runBatch (\a' -> "jsaddle.postMessage(JSON.stringify(" <> a' <> "));")
                  (Just (\a' -> "JSON.parse(jsaddle.syncMessage(JSON.stringify(" <> a' <> ")))")) <> "\
        \ };\n\
        \})();\n\
        \jsaddle.postReady();\n"
    }
  BS.useAsCString url $ \curl -> do
    executor <- startMainWidget_ a curl callbacks
    writeIORef executorRef executor
    writeIORef globalJSExecutor $ Just executor

-- | Effectful variant of 'startMainWidget'. Same shape and semantics;
-- the body is lifted through 'IOE'. The bootstrap remains 'JSM' rather
-- than @'Eff' es@ because the surrounding jsaddle 'JSContextRef' is
-- minted *inside* 'runJavaScript' and does not exist before this call
-- returns — there is nothing to bind a 'JSM'' interpreter against yet.
startMainWidgetEff
  :: (IOE :> es)
  => HaskellActivity -> ByteString -> JSM () -> Eff es ()
startMainWidgetEff a url jsm = liftIO (startMainWidget a url jsm)

newtype JSExecutor = JSExecutor { unJSExecutor :: Ptr JSExecutor }

-- Ugh. But not doing this is also unfortunate for being able to clear the history so that the "back" list is good.
globalJSExecutor :: IORef (Maybe JSExecutor)
globalJSExecutor = unsafePerformIO $ newIORef Nothing
{-# NOINLINE globalJSExecutor #-}

-- | Run the supplied 'IO' callback against the currently-live global
-- 'JSExecutor', if one has been installed by a prior 'startMainWidget'
-- call. If no executor has been installed yet, the callback is skipped
-- (matches upstream MTL behaviour). Preserved verbatim so downstream
-- 'MonadIO' callers keep compiling.
withGlobalJSExecutor :: MonadIO m => (JSExecutor -> IO ()) -> m ()
withGlobalJSExecutor f = liftIO $ do
  executor <- readIORef globalJSExecutor
  case executor of
    Just e -> f e
    Nothing -> pure ()

-- | Effectful variant of 'withGlobalJSExecutor'. Same skip-if-missing
-- semantics; body lifted through 'IOE'. Suitable for use inside
-- 'performEvent_' via @EventME . withGlobalJSExecutorEff@ once callers
-- have 'PerformEvent t :> es' + 'IOE :> es' in their row (see
-- @Reflex.Dom.Location.Platform@'s Android branch).
withGlobalJSExecutorEff
  :: (IOE :> es)
  => (JSExecutor -> IO ()) -> Eff es ()
withGlobalJSExecutorEff f = liftIO $ do
  executor <- readIORef globalJSExecutor
  case executor of
    Just e -> f e
    Nothing -> pure ()

foreign import ccall safe "Reflex_Dom_Android_MainWidget_start" startMainWidget_ :: HaskellActivity -> CString -> Ptr JSaddleCallbacksPtrs -> IO JSExecutor

foreign import ccall safe "Reflex_Dom_Android_MainWidget_runJS" runJS :: JSExecutor -> CString -> CSize -> IO ()
foreign import ccall safe "Reflex_Dom_Android_MainWidget_back" goBack :: JSExecutor -> IO ()

data JSaddleCallbacks = JSaddleCallbacks
  { _jsaddleCallbacks_jsaddleStart      :: IO ()
  , _jsaddleCallbacks_jsaddleResult     :: ByteString -> IO ()
  , _jsaddleCallbacks_jsaddleSyncResult :: ByteString -> IO ByteString
  , _jsaddleCallbacks_jsaddleJsData     :: ByteString
  }

data JSaddleCallbacksPtrs = JSaddleCallbacksPtrs
  { _jsaddleCallbacksPtrs_jsaddleStart      :: !(FunPtr (IO ()))
  , _jsaddleCallbacksPtrs_jsaddleResult     :: !(FunPtr (CString -> CSize -> IO ()))
  , _jsaddleCallbacksPtrs_jsaddleSyncResult :: !(FunPtr (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()))
  , _jsaddleCallbacksPtrs_jsaddleJsData     :: !CString
  }

foreign import ccall "wrapper" wrapIO :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper" wrapCStringCSizeIO :: (CString -> CSize -> IO ()) -> IO (FunPtr (CString -> CSize -> IO ()))
foreign import ccall "wrapper" wrapCStringCSizeCStringCSizeIO :: (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()) -> IO (FunPtr (CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()))

newCStringFromByteString :: ByteString -> IO (CString, CSize)
newCStringFromByteString bs = BSU.unsafeUseAsCStringLen bs $ \(src, len) -> do
  dest <- mallocArray0 len
  copyArray dest src len
  poke (advancePtr dest len) 0
  return (dest, fromIntegral len)

jsaddleCallbacksToPtrs :: JSaddleCallbacks -> IO JSaddleCallbacksPtrs
jsaddleCallbacksToPtrs jc = JSaddleCallbacksPtrs
  <$> wrapIO (_jsaddleCallbacks_jsaddleStart jc)
  <*> wrapCStringCSizeIO (\cstr len -> _jsaddleCallbacks_jsaddleResult jc =<< BS.packCStringLen (cstr, fromIntegral len))
  <*> wrapCStringCSizeCStringCSizeIO (\cstr len result resultLen -> do
                                         inStr <- BS.packCStringLen (cstr, fromIntegral len)
                                         outStr <- _jsaddleCallbacks_jsaddleSyncResult jc inStr
                                         (outStrBytes, outStrLen) <- newCStringFromByteString outStr
                                         poke result outStrBytes
                                         poke resultLen outStrLen)
  <*> (fst <$> newCStringFromByteString (_jsaddleCallbacks_jsaddleJsData jc))

instance Storable JSaddleCallbacksPtrs where
  sizeOf _ = #{size JSaddleCallbacks}
  alignment _ = #{alignment JSaddleCallbacks}
  poke p jc = do
    #{poke JSaddleCallbacks, jsaddleStart} p $ _jsaddleCallbacksPtrs_jsaddleStart jc
    #{poke JSaddleCallbacks, jsaddleResult} p $ _jsaddleCallbacksPtrs_jsaddleResult jc
    #{poke JSaddleCallbacks, jsaddleSyncResult} p $ _jsaddleCallbacksPtrs_jsaddleSyncResult jc
    #{poke JSaddleCallbacks, jsaddleJsData} p $ _jsaddleCallbacksPtrs_jsaddleJsData jc
  peek p = JSaddleCallbacksPtrs
    <$> #{peek JSaddleCallbacks, jsaddleStart} p
    <*> #{peek JSaddleCallbacks, jsaddleResult} p
    <*> #{peek JSaddleCallbacks, jsaddleSyncResult} p
    <*> #{peek JSaddleCallbacks, jsaddleJsData} p
