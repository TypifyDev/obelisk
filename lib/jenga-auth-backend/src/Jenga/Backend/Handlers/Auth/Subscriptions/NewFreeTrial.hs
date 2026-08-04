module Jenga.Backend.Handlers.Auth.Subscriptions.NewFreeTrial where

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.Subscriptions
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Database.Beam.Postgres
import Database.Beam.Schema
import Rhyolite.Account
import Jenga.Route

import Control.Monad.IO.Class ()
import Data.Signed
import Data.Pool
import Web.ClientSession as CS
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()



newFreeTrialHandler
  :: forall api db cfg be es n frontendRoute x.
     ( IOE :> es
     --, EmailM es db n be
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db FreeTrial
     )
  => Proxy api
  -> (Maybe T.Text, Email)
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> Eff es (Either (BackendError FreeTrialError) ())
newFreeTrialHandler proxy (mCode,email) resetRoute mkEmail = do
  (freeTrialTbl :: PgTable Postgres db FreeTrial) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  case validate . T.encodeUtf8 . unEmail $ email of
    Left _ -> pure $ Left . BUserError $ InvalidEmail_FreeTrial
    Right validatedEmail -> do
      createNewAccountWithSetupEmail @api @db @cfg proxy validatedEmail IsSelf resetRoute mkEmail >>= \case
        Left e -> pure $ Left $ FreeTrial_Signup <$> e
        Right () -> do
          (withDbEnv @cfg $ getUserByEmail acctsTbl $ unEmail email) >>= \case
            Nothing -> pure $ Left . BCritical $ NoUserForFreeTrial-- "Error creating user: on get user: not found"
            Just user -> do
              withDbEnv @cfg $ putNewFreeTrial freeTrialTbl (pk user) mCode
              pure $ Right ()
