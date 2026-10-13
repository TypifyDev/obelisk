module Jenga.Backend.Handlers.Auth.UserSignup where

import Data.Kind (Type)
import Effectful.Reader.Static (Reader)
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema

import Rhyolite.Account
import Jenga.Route
import Jenga.Route.Servant (HasRoute)
import Data.Proxy (Proxy)
import Network.Mail.Mime
import Database.Beam
import Database.Beam.Postgres

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Data.Bifunctor
import Text.Email.Validate as EmailValidate
import qualified Data.Text.Encoding as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()

-- userSignup
--   :: MonadIO m
--   => T.Text -- ^ email subject
--   -> T.Text -- ^ email address
--   -> frontendRoute (Signed PasswordResetToken)
--   -> (T.Text -> StaticWidget x ())
--   -> ReaderT cfg m (Either (BackendError UserSignupError) ())
-- userSignup subject address resetR mkEmail = userSignup'
--   subject
--   address
--   resetR
--   mkEmail


--let mkBody = body <> "\n" <> link -- plaintext emali
userSignupHandler
  :: forall (api :: Type -> Type) db cfg be frontendRoute es x n.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Proxy api
  -> EmailValidate.EmailAddress -- ^ email address
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)  -- ^ email body
  -> Eff es (Either (BackendError UserSignupError) ())
userSignupHandler proxy email resetRoute mkBody = do
  case EmailValidate.emailAddress (EmailValidate.toByteString email) of
    Nothing -> pure . Left . BUserError $ BadSignupEmail
    Just emailParsed -> do
      createNewAccount @api @db @cfg proxy (emailParsed) IsSelf resetRoute >>= \case
        Left e -> pure $ Left e
        Right link -> do
          let
            to = Address
              { addressName = Nothing
              , addressEmail = T.decodeUtf8 . toByteString $ email
              }
          x <- newMkEmailHtmlE @db @be @cfg @n [to] $ mkBody link
          pure $ first (\_ -> BCritical NoEmailSent) x
