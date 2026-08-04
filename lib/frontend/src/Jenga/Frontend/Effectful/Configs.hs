-- | Jenga's app-config effect (previously
-- 'Reflex.Effectful.Effect.Jenga.Configs'). Provides read-only access to
-- the config map baked into the frontend at build time.
module Jenga.Frontend.Effectful.Configs
  ( Configs(..)
  , getConfigs
  , getConfig
  , getTextConfig
  ) where

import           Data.ByteString          (ByteString)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T

import qualified Effectful                as Eff
import           Effectful                ((:>))
import qualified Effectful.Dispatch.Dynamic as Eff

data Configs :: Eff.Effect where
  GetConfigs :: Configs m (Map Text ByteString)

type instance Eff.DispatchOf Configs = 'Eff.Dynamic

getConfigs :: Configs :> es => Eff.Eff es (Map Text ByteString)
getConfigs = Eff.send GetConfigs

getConfig :: Configs :> es => Text -> Eff.Eff es (Maybe ByteString)
getConfig k = Map.lookup k <$> getConfigs

getTextConfig :: Configs :> es => Text -> Eff.Eff es (Maybe Text)
getTextConfig k = fmap T.decodeUtf8 <$> getConfig k
