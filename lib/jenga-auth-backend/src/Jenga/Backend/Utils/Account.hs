module Jenga.Backend.Utils.Account (createNewAccount, createNewAccountWithSetupEmail) where

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasTable (PgTable, HasJengaTable, asksTableM, withDbEnv)
import Jenga.Common.Schema

import Web.ClientSession as CS

import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Common.Errors
import Jenga.Common.Auth


import Rhyolite.Account
import Rhyolite.Backend.Account
import Jenga.Route
import Database.Beam.Postgres
import Database.Beam
import Network.Mail.Mime
import Data.Signed

import Data.Pool
import Data.Bifunctor
import Text.Email.Validate
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()



createNewAccount
  :: forall api db cfg frontendRoute es .
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     )
  => Proxy api
  -> EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> Eff es (Either (BackendError UserSignupError) Link)
createNewAccount proxy email isUserType resetRoute = do
  csk <- asksM @CS.Key @cfg
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (orgTbl :: PgTable Postgres db OrganizationEmails) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (accountsTable :: PgTable Postgres db Account) <- asksTableM
  (withDbEnv @cfg $ ensureAccountExists' acctsTbl $ T.decodeUtf8 . toByteString $ email) >>= \case
    (False, _) -> pure $ Left . BUserError $ AccountExists
    (True, aid) -> do
      (withDbEnv @cfg $ putAccountRelations uTypeTbl orgTbl aid isUserType) >>= \case
        Left noOrgErr -> pure . Left . BCritical $ noOrgErr
        Right () -> do
          (withDbEnv @cfg $ newNonce accountsTable aid) >>= \case
            Nothing -> pure $ Left . BUserError $ FailedMakeResetToken
            Just nonce -> do
              token <- withDbEnv @cfg $ passwordResetToken csk aid nonce
              resetLink <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy $ ((resetRoute :/ token) :: R frontendRoute)
              pure $ Right resetLink

createNewAccountWithSetupEmail
  :: forall api db cfg es be n frontendRoute x.
     ( IOE :> es
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
     )
  => Proxy api
  -> EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> Eff es (Either (BackendError UserSignupError)  ())
createNewAccountWithSetupEmail proxy email isUserType resetRoute mkEmail = do
  (createNewAccount @api @db @cfg proxy email isUserType resetRoute) >>= (\case
    Left e -> pure $ Left e
    Right link_ -> do
      x <- newMkEmailHtml @db @cfg [to] $ mkEmail link_
      pure $ first (\_ -> BCritical NoEmailSent) x)
  where
    to = Address
      { addressName = Nothing
      , addressEmail = T.decodeUtf8 . toByteString $ email
      }
