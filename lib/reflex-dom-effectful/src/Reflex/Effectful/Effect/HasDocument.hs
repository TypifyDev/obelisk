module Reflex.Effectful.Effect.HasDocument (HasDocument(..), askDocument) where

import           Effectful                (Effect, Dispatch(..), DispatchOf, (:>), Eff)
import           Effectful.Dispatch.Dynamic (send)
import qualified Reflex.Dom.Builder.Class as DBC
import           Reflex.Effectful.Types   (GhcjsDomSpace)

data HasDocument :: Effect where
  AskDocument :: HasDocument m (DBC.RawDocument GhcjsDomSpace)

type instance DispatchOf HasDocument = 'Dynamic

askDocument :: HasDocument :> es => Eff es (DBC.RawDocument GhcjsDomSpace)
askDocument = send AskDocument
