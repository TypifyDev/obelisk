module Jenga.Backend.Handlers.Auth.OrgBased.AdminSignup where

import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Schema
import Jenga.Common.Auth

import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres
import Rhyolite.Account
import Jenga.Route

import Web.ClientSession as CS
import Data.Pool
import Data.Signed
import Text.Email.Validate
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

adminSignupHandler
  :: forall api db cfg frontendRoute es x n.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg CompanySignupCode
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrganizationEmails
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => Proxy api
  -> NewCompanyEmail
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -> Eff es (Either (BackendError AdminSignupError) ())
adminSignupHandler proxy (NewCompanyEmail email orgName code) resetRoute mkEmail = do
  matchesCompanyCodeEnv @cfg code >>= \case
    False -> pure $ Left . BUserError $ InvalidAdminCode
    True -> do
      createNewAccount @api @db @cfg proxy email (IsCompany orgName) resetRoute >>= \case
        Left beErr -> pure $ Left $ fmap AdminSignupError $ beErr
        Right link -> do
          let
            to = Address
                 { addressName = Nothing
                 , addressEmail = T.decodeUtf8 . toByteString $ email
                 } -- recipients Address
          eRes <- newMkEmailHtml @db @cfg [to] $ mkEmail link
          case eRes of
            Left _ -> pure $ Left . BCritical . AdminSignupError $ NoEmailSent
            Right () -> do
              pure $ Right ()

--  do
--                 Rfx.el "div" $ do
--                   Rfx.el "div" $ Rfx.text "New account creation requested"
--                   Rfx.el "div" $ Rfx.text "go the following address to set your password:"
--                   Rfx.el "div" $ Rfx.text link
--                   Rfx.el "div" $ Rfx.text ""
--                   Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."

      -- (isNew, aid) <- withDbEnv $ do
      --   ensureAccountExists' acctTbl $ T.decodeUtf8 . toByteString $ email
      -- case isNew of
      --   False -> pure $ Left . BUserError $ AlreadySignedUp
      --   True -> do
      --     mNonce <- withDbEnv $ do
      --       putAccountRelations uTypeTbl orgTbl aid (IsCompany orgName)
      --       newNonce acctTbl aid
      --     case mNonce of
      --       Nothing -> pure $ Left . BCritical $ FailedMkNonce_AdminSignup
      --       Just noncense -> do
      --         csk <- asksM
      --         resetToken <- withDbEnv $ passwordResetToken csk aid noncense
      --         link <- renderFullRouteFE @beR $ resetRoute :/ resetToken
      --         let
      --           to = Address
      --                { addressName = Nothing
      --                , addressEmail = T.decodeUtf8 . toByteString $ email
      --                } -- recipients Address
      --         eRes <- newEmailHtml @db [to] "Admin Email Confirmation" $ mkEmail link
      --         case eRes of
      --           Left _ -> pure $ Left . BCritical $ FailedMkEmail_AdminSignup
      --           Right () -> do
      --             pure $ Right ()
