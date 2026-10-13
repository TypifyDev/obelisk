{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- | Ported from upstream @Reflex.Dom.Modals.Class@.
--
-- The upstream shape was @class ModalOpener t m@ where the widget
-- action was @m (Event t a)@. In the Eff port the widget row is a
-- concrete @Eff es@, so the class is keyed on the effect row @es@
-- instead of an abstract monad @m@; the requested widget action is
-- @Eff es (Event t a)@.
module Reflex.Effectful.Dom.Modals.Class where

import Effectful (Eff)

import Reflex.Class

class ModalOpener t es where
  requestingModal :: Event t (Eff es (Event t a)) -> Eff es (Event t (Maybe a))
