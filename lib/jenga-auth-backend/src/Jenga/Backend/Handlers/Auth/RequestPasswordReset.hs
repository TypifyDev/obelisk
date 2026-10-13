{-# LANGUAGE KindSignatures #-}
module Jenga.Backend.Handlers.Auth.RequestPasswordReset where

import Data.Kind (Type)

-- import Backend.DB (db)
import Jenga.Backend.DB.Auth

-- import Common.Schema
-- import Common.Route
-- import Backend.Config
-- import Backend.Utils.Email
import Jenga.Backend.Utils.HasTable
import Effectful.Reader.Static (Reader)
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Backend.Account
import Rhyolite.Account
import Jenga.Route
import Jenga.Route.Servant (HasRoute)
import Data.Proxy (Proxy(..))
import Reflex.Dom.Core
import Database.Beam.Schema
import Database.Beam.Postgres (Postgres, Connection)

import Web.ClientSession as CS
import Network.Mail.Mime
import Data.Pool
import Data.Signed
import Control.Monad.IO.Class ()
import qualified Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

requestPasswordResetHandler
  :: forall (api :: Type -> Type) db cfg n es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , HasConfig cfg (Jenga.Backend.Utils.HasConfig.FullRouteEncoder api frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl Postgres SendEmailTask n
     )
  => Proxy api
  -> frontendRoute (Signed PasswordResetToken)
  -> Email
  -> Eff es (Either (BackendError RequestPasswordResetError) ())
requestPasswordResetHandler _proxy resetRoute (Email rawEmail) = do
  csk <- asksME @CS.Key @cfg -- Cfg _clientSessionKey

  (acctTbl :: PgTable Postgres db Account) <- asksTableM
  let email' = (\x -> T.decodeUtf8 $ EmailValidate.toByteString x) <$> (EmailValidate.validate $ T.encodeUtf8 rawEmail)
  case email' of
    Left _ -> pure . Left . BUserError $ InvalidEmail
    Right email_ -> do
      isNew <- withDbEnvE @cfg $ doesAccountExist acctTbl email_
      case isNew of
        Nothing -> pure . Left . BUserError $ NotSignedUp -- "An account with that email does not exist, please signup first"
        Just aid -> do
          mNonce <- withDbEnvE @cfg $ newNonce acctTbl aid
          case mNonce of
            Nothing -> pure . Left . BCritical $ FailedMakeNonce
            Just noncense -> do
              token <- withDbEnvE @cfg $ passwordResetToken csk aid noncense
              resetLink <- renderFullRouteFEE @api @frontendRoute @cfg $ resetRoute :/ token
              let
                to = Address
                     { addressName = Nothing
                     , addressEmail = email_
                     } -- recipients Address
              _ <- newEmailHtmlE @db @Postgres @cfg @n [to] "Password Reset Request" $ do
                el "div" $ do
                  el "div" $ text "Password reset requested"
                  el "div" $ text "go the following address to reset your password:"
                  el "div" $ text $ getLink resetLink
                  el "div" $ text "if you didn't request this action, please ignore this email."
              pure $ Right ()
