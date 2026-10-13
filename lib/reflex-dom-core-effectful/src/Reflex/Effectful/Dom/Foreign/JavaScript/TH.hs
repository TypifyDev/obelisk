{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif
#endif

-- | Ported from upstream @Foreign.JavaScript.TH@.
--
-- The upstream module bundled a 'JSContextSingleton' phantom-tagged
-- newtype (used to statically prevent JSContexts from being mixed
-- across pages) with a 'WithJSContextSingleton' monad-transformer
-- wrapper that lifted a stack of Reflex MTL classes
-- (@PerformEvent@, @MonadHold@, @MonadSample@, @MonadJSM@,
-- @MonadReflexHost@, ...) through a @ReaderT (JSContextSingleton x)@.
--
-- The transformer wrapper is deleted in the Eff port: all effects run
-- in a single 'Eff' row, so there is no MTL stack to lift through and
-- no @Performable m@ subtree to split off. Access to the JS context in
-- Eff-shape widgets goes through @IOE :> es@ + 'askJSM' directly; the
-- two @with*@ helpers below take an @Eff es@ continuation and use the
-- same 'askJSM' path the transformer instance used to.
module Reflex.Effectful.Dom.Foreign.JavaScript.TH
  ( module Reflex.Dom.Core
#ifdef USE_TEMPLATE_HASKELL
  , Safety (..)
#endif
  ) where

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
#endif

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Static (unsafeEff_)

import GHCJS.DOM.Types (JSContextRef, JSM, runJSM)

import Reflex.Effectful.DomHost (JSM', askJSM)
#ifdef ghcjs_HOST_OS
#ifdef __GHCJS__
import qualified GHCJS.DOM.Types as JS
import qualified GHCJS.Foreign as JS
#endif
#if __GLASGOW_HASKELL__ < 900
import qualified GHCJS.Foreign.Callback as JS
import qualified GHCJS.Foreign.Callback.Internal (Callback (..))
import qualified JavaScript.Array as JS
import qualified JavaScript.Array.Internal (SomeJSArray (..))
import qualified JavaScript.Object as JS
import qualified JavaScript.Object.Internal (Object (..))
import qualified JavaScript.TypedArray.ArrayBuffer as JSArrayBuffer

import Data.Hashable
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Text.Encoding.Z
#endif
#endif

-- | A singleton type for a given JSContext; the phantom @x@ is used to
-- statically guarantee different JSContexts don't get mixed up.
newtype JSContextSingleton x = JSContextSingleton { unJSContextSingleton :: JSContextRef }

-- | Grab the ambient 'JSContextRef' (via 'askJSM' in 'IO') and hand it
-- back to the caller wrapped in a fresh 'JSContextSingleton' with a
-- rank-2 phantom, preventing the caller from leaking the tag out.
withJSContextSingleton
  :: (JSM' :> es)
  => (forall x. JSContextSingleton x -> Eff es r) -> Eff es r
withJSContextSingleton f = do
  ctx <- askJSM
  f (JSContextSingleton ctx)

-- | Warning: 'withJSContextSingletonMono' does not provide the same
-- rank-2 guarantee that 'withJSContextSingleton' does — the phantom is
-- fixed to @()@, so tags CAN leak. Use the polymorphic version unless
-- you have a specific reason not to.
withJSContextSingletonMono
  :: (JSM' :> es)
  => (JSContextSingleton () -> Eff es r) -> Eff es r
withJSContextSingletonMono f = do
  ctx <- askJSM
  f (JSContextSingleton ctx)

-- | Run a 'JSM' action against a specific 'JSContextSingleton', lifting
-- the result into the effect row. Replaces the transformer-based
-- @runWithJSContextSingleton@ / @MonadJSM (WithJSContextSingleton x m)@
-- pathway.
runInJSContextSingleton
  :: (IOE :> es)
  => JSContextSingleton x -> JSM r -> Eff es r
runInJSContextSingleton (JSContextSingleton ctx) act =
  unsafeEff_ (runJSM act ctx)
