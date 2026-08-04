{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Jenga.Backend.Handlers.Auth.Login where


-- import Backend.DB (db)
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Cookies (addAuthCookieHeader)
-- import Common.Constants (clientTypeHeader)
import Jenga.Common.Schema
-- import Common.Constants (authCookieName)
-- import Common.Types
-- import Common.ChatSchema
import Jenga.Common.BeamExtras
import Jenga.Common.Auth
import Jenga.Common.Errors

import Rhyolite.Backend.Account
import Rhyolite.Account

import qualified Snap

import Database.Beam.Postgres
import Database.Beam.Schema
import Data.Signed.ClientSession

import Control.Monad.IO.Class
import Data.Pool
import Data.Signed
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


-- | Note that the login function from rhyolite really just returns the ID
loginHandler
  :: forall db cfg es.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db Account
     )
  => (T.Text, Password)
  -> Eff es (Either (BackendError LoginError) (Signed (Id Account), UserType, Snap.Snap ()))
loginHandler (email, Password pass) = do
  tryLogin @db @cfg (email, Password pass) >>= \case
    Left bError -> pure $ Left bError
    Right (usersAccountId, userType) -> do
      csk <- asksM @Key @cfg
      authCookieName <- getAuthCookieName <$> asksM @AuthCookieName @cfg
      signedTokenUserID_  <- liftIO $ signWithKey csk usersAccountId
      domainOpts <- asksM @DomainOption @cfg
      pure $ Right (signedTokenUserID_, userType, addAuthCookieHeader csk domainOpts authCookieName usersAccountId)

-- | If we are using websockets, this is all we need
tryLogin
  :: forall db cfg es.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db Account
     )
  => (T.Text, Password)
  -> Eff es (Either
                    (BackendError LoginError)
                    (Id Account, UserType)
                   )
tryLogin (email, Password pass) = do
  (acctsTbl :: PgTable Postgres db Account) <- asksTableM
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  mAcct <- withDbEnv @cfg $ getUserByEmail acctsTbl email
  case mAcct of
    Nothing -> pure . Left . BUserError $ UnrecognizedEmail email
    Just acctRow@(Rhyolite.Account.Account _ userEmail _ _) -> do
      loginResult <- withDbEnv @cfg $ login acctsTbl (T.toLower userEmail) pass
      case loginResult of
        Nothing -> pure . Left . BUserError $ IncorrectPassword
        Just usersAccountId -> do
          userType' <- withDbEnv @cfg $ getUserType uTypeTbl $ pk acctRow--aid
          case userType' of
            Nothing -> pure . Left . BCritical $ NoUserTypeFound
            Just uType -> do
              pure $ Right (usersAccountId, uType)
