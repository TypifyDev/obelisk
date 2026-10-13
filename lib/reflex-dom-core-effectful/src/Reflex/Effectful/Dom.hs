-- |
-- Umbrella entry-point for reflex-dom-core-effectful.
--
-- Content relocated to 'Reflex.Effectful.DomHost' (authoritative
-- effectful implementation). This module is now a bare re-export
-- shim so existing @import Reflex.Effectful.Dom@ callsites keep
-- working transparently.
module Reflex.Effectful.Dom (module Reflex.Effectful.DomHost) where

import Reflex.Effectful.DomHost
