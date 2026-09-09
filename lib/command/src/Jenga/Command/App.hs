{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful port of upstream @Obelisk.App.HasObelisk@.
--
-- Not a new type, not a new effect — just the built-in
-- 'Effectful.Reader.Static.Reader' parameterised on the existing
-- upstream 'Obelisk' newtype. 'askObelisk' is the Effectful sibling of
-- upstream's @getObelisk@; 'runObelisk' installs the ambient value.
module Jenga.Command.App
  ( Obelisk
  , askObelisk
  , runObelisk
  ) where

import           Effectful                        (Eff, (:>))
import qualified Effectful.Reader.Static          as R
import           Obelisk.App                      (Obelisk)

-- | Read the ambient 'Obelisk' record. Direct analog of upstream
-- @getObelisk@ from @Obelisk.App.HasObelisk@.
askObelisk :: R.Reader Obelisk :> es => Eff es Obelisk
askObelisk = R.ask

-- | Install an 'Obelisk' record and interpret the ambient 'Reader'
-- against it. Direct analog of upstream @runObelisk@'s @ReaderT@ layer
-- (the @CliT@ layer isn't ported here — that lives in @cli-extras@).
runObelisk :: Obelisk -> Eff (R.Reader Obelisk : es) a -> Eff es a
runObelisk = R.runReader
