{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Port of Reflex.NotReady.Class into reflex-effectful.
--
-- The upstream module also provides NotReady instances for the reflex
-- transformer types (PostBuildT, EventWriterT, DynamicWriterT,
-- BehaviorWriterT, QueryT, PerformEventT, RequesterT, TriggerEventT).
-- We do NOT port those instances: the reflex-effectful universe uses
-- the effectful @es@-row (Reflex.Effectful.Effect.*) instead of MTL
-- transformers, so those instances have no consumer here and porting
-- them would require ALSO porting the reflex @Base@ modules that
-- define the transformers themselves.
--
-- The two standard MTL instances (ReaderT, WriterT) are kept because
-- they have no dependency on the reflex-specific transformers.
module Reflex.Effectful.Class.NotReady.Class
  ( NotReady(..)
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Writer (WriterT)

import Reflex.Effectful.Class

class Monad m => NotReady t m | m -> t where
  notReadyUntil :: Event t a -> m ()
  default notReadyUntil :: (MonadTrans f, m ~ f m', NotReady t m') => Event t a -> m ()
  notReadyUntil = lift . notReadyUntil

  notReady :: m ()
  default notReady :: (MonadTrans f, m ~ f m', NotReady t m') => m ()
  notReady = lift notReady

instance NotReady t m => NotReady t (ReaderT r m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance (NotReady t m, Monoid w) => NotReady t (WriterT w m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady
