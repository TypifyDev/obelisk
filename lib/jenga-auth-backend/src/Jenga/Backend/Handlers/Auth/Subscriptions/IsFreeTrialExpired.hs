module Jenga.Backend.Handlers.Auth.Subscriptions.IsFreeTrialExpired where

import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Schema
--import Common.Constants (freeTrialLength, freeDiscountCode)
-- import Common.Types
import Jenga.Common.Errors
import Jenga.Common.BeamExtras
import Rhyolite.Account

import Database.Beam.Postgres
import Database.Beam.Schema

import Control.Monad.IO.Class
import Data.Pool
import Data.Time.Clock
import Data.Functor.Identity

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

isFreeTrialExpiredHandler
  :: forall db cfg es.
     ( IOE :> es
     , Database Postgres db
     , HasJengaTable Postgres db FreeTrial
     , Reader cfg :> es, HasConfig cfg FreeTrialInfo
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     )
  => Id Account
  -> Eff es (Either (BackendError ()) Bool)
isFreeTrialExpiredHandler acctID = do
  freeTrialInfo <- asksM @FreeTrialInfo @cfg
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (withDbEnv @cfg $ lookupFreeTrial freeTrialTbl acctID) >>= \case
    Nothing -> pure $ Right False
    Just freeTrial -> do
      case (== getFreeTrialCode freeTrialInfo) <$> _freeTrial_discountCode freeTrial of
        Just True -> pure $ Right False
        _ -> do
          tNow <- liftIO $ getCurrentTime
          pure . Right $ isExpired tNow freeTrial freeTrialInfo

isExpired :: UTCTime -> FreeTrial Identity -> FreeTrialInfo -> Bool
isExpired tNow freeTrial freeTrialInfo =
  diffUTCTime tNow (_freeTrial_startDate freeTrial) > (getFreeTrialLength freeTrialInfo)
