{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Effectful.Effect.JSM
  ( -- * Effect
    JSM'(..)

    -- * Operations
  , liftJSM
  ) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)

import           GHCJS.DOM.Types          (JSM)

-- | Effect for running JavaScript operations via JSaddle.
-- Replaces @MonadJSM m@.
--
-- This gives you full access to the JavaScript runtime:
-- DOM manipulation, calling JS functions, reading JS values, etc.
--
-- Named @JSM'@ (with prime) to avoid collision with the @JSM@ monad type.
data JSM' :: Effect where
  -- | Lift a JSaddle computation into the effect stack.
  LiftJSM :: JSM a -> JSM' m a

type instance DispatchOf JSM' = 'Dynamic

-- | Run a 'JSM' (JSaddle) computation.
--
-- @
-- myWidget :: (Dom :> es, JSM' :> es) => Eff es ()
-- myWidget = do
--   doc <- liftJSM $ currentDocumentUnchecked
--   liftJSM $ consoleLog ("hello from JS" :: Text)
-- @
liftJSM :: JSM' :> es => JSM a -> Eff es a
liftJSM = send . LiftJSM
