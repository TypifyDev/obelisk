{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Jenga.Backend.Utils.Account
  ( createNewAccount
  , createNewAccountWithSetupEmail
  , createNewAccountMTL
  , createNewAccountWithSetupEmailMTL
  ) where

import Data.Proxy (Proxy)
import Effectful (Eff, (:>), IOE)
import qualified Effectful.Reader.Static as ER

import Jenga.Backend.DB.Auth
import Jenga.Backend.DB.OrgBased
import Jenga.Backend.Utils.HasTable (PgTable, HasJengaTable, asksTableM, withDbEnv)
import Jenga.Common.Schema
import Control.Monad.Trans.Reader

import Web.ClientSession as CS

import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Common.Errors
import Jenga.Common.Auth


import Rhyolite.Account
import Rhyolite.Backend.Account
import Jenga.Route.Core (R)
import Jenga.Route (pattern (:/), FullRouteEncoder)
import Database.Beam.Postgres
import Database.Beam
import Network.Mail.Mime
import Data.Signed

import Data.Pool
import Data.Bifunctor
import Text.Email.Validate
import qualified Data.Text.Encoding as T



createNewAccountMTL
  :: forall db beR frontendRoute m cfg .
     ( MonadIO m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> ReaderT cfg m (Either (BackendError UserSignupError) Link)
createNewAccountMTL email isUserType resetRoute = do
  csk <- asksM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  (orgTbl :: PgTable Postgres db OrgOwnedUsers) <- asksTableM
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (accountsTable :: PgTable Postgres db Account) <- asksTableM
  (withDbEnv $ ensureAccountExists' acctsTbl $ T.decodeUtf8 . toByteString $ email) >>= \case
    Left (EnsureAccount_InsertReturnedUnexpectedRows n) -> pure $ Left . BCritical $ AccountInsertFailed n
    Right (False, _) -> pure $ Left . BUserError $ AccountExists
    Right (True, aid) -> do
      (withDbEnv $ putAccountRelations uTypeTbl orgTbl aid isUserType) >>= \case
        Left noOrgErr -> pure . Left . BCritical $ noOrgErr
        Right () -> do
          (withDbEnv $ newNonce accountsTable aid) >>= \case
            Nothing -> pure $ Left . BUserError $ FailedMakeResetToken
            Just nonce -> do
              token <- withDbEnv $ passwordResetToken csk aid nonce
              resetLink <- renderFullRouteFE @beR $ ((resetRoute :/ token) :: R frontendRoute)
              pure $ Right resetLink

createNewAccountWithSetupEmailMTL
  :: forall db beR cfg be m n frontendRoute x.
     ( MonadIO m
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder beR frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )
  => EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> ReaderT cfg m (Either (BackendError UserSignupError)  ())
createNewAccountWithSetupEmailMTL email isUserType resetRoute mkEmail = do
  (createNewAccountMTL @db @beR email isUserType resetRoute) >>= (\case
    Left e -> pure $ Left e
    Right link_ -> do
      x <- newMkEmailHtml @db [to] $ mkEmail link_
      pure $ first (\_ -> BCritical NoEmailSent) x)
  where
    to = Address
      { addressName = Nothing
      , addressEmail = T.decodeUtf8 . toByteString $ email
      }

-- | Effectful wrapper: bridges 'createNewAccountMTL' into an 'Eff es'
-- context by drawing the config from 'ER.Reader cfg' and running the
-- MTL body under 'runReaderT' inside 'liftIO'. Type-app order is
-- @\@api \@db \@cfg@ to match the effectful call sites.
createNewAccount
  :: forall api db cfg frontendRoute es.
     ( IOE :> es
     , ER.Reader cfg :> es
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     )
  => Proxy api
  -> EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> Eff es (Either (BackendError UserSignupError) Link)
createNewAccount _proxy email isUserType resetRoute = do
  cfg <- ER.ask @cfg
  liftIO $ flip runReaderT cfg $
    createNewAccountMTL @db @api @frontendRoute email isUserType resetRoute

-- | Effectful wrapper for 'createNewAccountWithSetupEmailMTL'.
createNewAccountWithSetupEmail
  :: forall api db cfg be frontendRoute n es x.
     ( IOE :> es
     , ER.Reader cfg :> es
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , HasConfig cfg AdminEmail
     , HasConfig cfg CS.Key
     , HasConfig cfg (Pool Connection)
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasConfig cfg BaseURL
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )
  => Proxy api
  -> EmailAddress
  -> IsUserType
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> Eff es (Either (BackendError UserSignupError) ())
createNewAccountWithSetupEmail _proxy email isUserType resetRoute mkEmail = do
  cfg <- ER.ask @cfg
  liftIO $ flip runReaderT cfg $
    createNewAccountWithSetupEmailMTL @db @api @cfg @be @IO @n @frontendRoute @x
      email isUserType resetRoute mkEmail
