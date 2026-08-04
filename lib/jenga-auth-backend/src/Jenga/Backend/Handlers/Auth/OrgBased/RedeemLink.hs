module Jenga.Backend.Handlers.Auth.OrgBased.RedeemLink where

import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Account
import Jenga.Route
import Database.Beam.Postgres
import Database.Beam.Schema

import Web.ClientSession as CS
import Data.Signed
import Data.Pool
import Control.Monad.IO.Class ()
import Text.Email.Validate
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

redeemLinkHandler
  :: forall api db cfg frontendRoute be es x n.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , HasJengaTable Postgres db InviteLink
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Proxy api
  -> (T.Text, Email)
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> Eff es (Either (BackendError RedeemLinkError) ())
redeemLinkHandler proxy (codeLink, Email email) resetRoute mkEmail = do
  (inviteTbl :: PgTable Postgres db InviteLink) <- asksTableM

  withDbEnv @cfg (getInviteLinkByCode inviteTbl codeLink) >>= \case
    Nothing -> pure $ Left . BUserError $ NoOrgCode_RedeemLink
    Just (InviteLink orgName code' mNLeft)
      | mNLeft == (Just 0) -> pure $ Left . BUserError $ CreditsDepleted orgName
      | otherwise -> do
          case validate . T.encodeUtf8 $ email of
            Left _ -> pure $ Left . BUserError $ InvalidEmail_RedeemLink
            Right email' -> do
              createNewAccountWithSetupEmail @api @db @cfg proxy email' (IsGroupUser email' orgName) resetRoute mkEmail >>= \case
                Left e -> pure $ Left $ RedeemLink_Signup <$> e
                Right () -> do
                  case mNLeft of
                    Nothing -> pure ()
                    Just nLeft -> withDbEnv @cfg $ setLinkNumLeft inviteTbl code' (nLeft - 1)
                  pure $ Right ()
