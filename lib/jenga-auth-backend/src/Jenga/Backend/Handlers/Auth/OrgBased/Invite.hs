module Jenga.Backend.Handlers.Auth.OrgBased.Invite where

import Jenga.Backend.Handlers.Auth.UserSignup (userSignupHandler)
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.Instances ()
import Jenga.Common.BeamExtras
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Jenga.Route
import Database.PostgreSQL.Simple
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Query

import Web.ClientSession as CS
import Data.Pool
import Control.Monad.IO.Class ()
import Data.Signed
import Data.Bifunctor
import Data.Maybe
import Text.Email.Validate as EmailValidate
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

inviteHandler
  :: forall api db cfg be frontendRoute es x n.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n

     )
  => Proxy api
  -> EmailValidate.EmailAddress
  -> Id Account
  -> frontendRoute (Signed PasswordResetToken)
  -> (T.Text -> Link -> MkEmail x)  -- ^ email body
  -> Eff es (Either (BackendError InviteError) ())
inviteHandler proxy email inviter resetRoute mkBody = do
  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  user <- (fmap.fmap) _account_email $ withDbEnv @cfg $ runSelectReturningOne $ lookup_ acctTbl inviter
  signupRes <- userSignupHandler @api @db @cfg
    proxy
    email
    resetRoute
    (\link_ -> mkBody (fromMaybe "another user" user) link_)
  pure $ first (fmap InviteSignupFailed) signupRes
