module Jenga.Backend.Handlers.Auth.OrgBased.AddUsers where

-- import Backend.DB.OrgBased
-- import Backend.Config
-- import Common.Types
-- import Backend.Utils.Account
import Effectful.Reader.Static (Reader)
import Jenga.Backend.Utils.Account
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.OrgBased
import Jenga.Common.Errors
import Jenga.Common.BeamExtras
import Jenga.Common.Schema
import Jenga.Common.Auth

import Rhyolite.Account
import Jenga.Route
import Jenga.Route.Servant (HasRoute)
import Data.Proxy (Proxy)
import Data.Kind (Type)
import Database.Beam.Schema
import Database.Beam.Postgres
import Snap

import Data.Pool
import Web.ClientSession as CS
import Data.Signed
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (some)
import Text.Parsec
import Text.Email.Validate
import Data.Either (partitionEithers)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


type AddUsersConstraint api db cfg be es n frontendRoute =
     ( IOE :> es
     , MonadSnap (Eff es)
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )


addUsersHandler
  :: forall (api :: Type -> Type) db cfg be es n frontendRoute x.
     ( IOE :> es
     , MonadSnap (Eff es)
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )
  => Proxy api
  -> Id Account
  -> T.Text
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -- ^ Email to send user
  -> Eff es (Either (BackendError AddUsersError) ())
addUsersHandler proxy acctID emails resetRoute mkEmail = do
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  case sepByCommas emails of
    Left _ -> pure $ Left . BUserError $ NoCommas -- "Error reading list, please ensure all emails are separated by commas"
    Right rawEmails -> do
      setTimeout $ length rawEmails + 10
      liftIO $ print rawEmails
      let (badEmails, goodEmails) = partitionEithers $
            fmap (\raw -> either (const (Left raw)) Right . validate . T.encodeUtf8 . T.pack $ raw) rawEmails
      case badEmails of
        (_:_) -> pure $ Left . BUserError $ InvalidEmail_AddUser (fmap T.pack badEmails)
        [] -> do
          results <- forM goodEmails $ \email -> do
            mCompanyId <- withDbEnvE @cfg $ getCompanyId uTypeTbl acctID
            case mCompanyId of
              Nothing -> pure $ Left . BUserError $ NoOrgCode $ T.pack . show $ acctID --"No organization code found"
              Just companyId -> do
                createNewAccountWithSetupEmail @api @db @cfg proxy email (IsGroupUser email companyId) resetRoute mkEmail >>= \case
                  Left bError -> pure $ Left $ AddUser_Signup <$> bError
                  Right a -> pure $ Right a
          pure $ () <$ sequenceA results

-- | Eff-shape sibling of 'addUsersHandler' with the 'Snap.setTimeout'
-- call hoisted OUT of the Eff body. The MTL variant embeds
-- @setTimeout $ length rawEmails + 10@ inside the handler, which
-- forces @MonadSnap (Eff es)@ on the whole action; no instance in the
-- tree satisfies that. Callers of this variant own the timeout —
-- typically at the Snap boundary, before invoking @runEff@, using a
-- conservative fixed budget or a Snap-side parse of the raw @emails@
-- 'T.Text' to compute the same @length rawEmails + 10@ hint.
--
-- MTL original 'addUsersHandler' left in place (no-delete rule).
addUsersHandlerNoTimeout
  :: forall (api :: Type -> Type) db cfg be es n frontendRoute x.
     ( IOE :> es
     , HasJsonNotifyTbl be SendEmailTask n
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     , HasJengaTable Postgres db OrgOwnedUsers
     , HasJengaTable Postgres db SendEmailTask
     )
  => Proxy api
  -> Id Account
  -> T.Text
  -> frontendRoute (Signed PasswordResetToken)
  -> (Link -> MkEmail x)
  -- ^ Email to send user
  -> Eff es (Either (BackendError AddUsersError) ())
addUsersHandlerNoTimeout proxy acctID emails resetRoute mkEmail = do
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  case sepByCommas emails of
    Left _ -> pure $ Left . BUserError $ NoCommas
    Right rawEmails -> do
      liftIO $ print rawEmails
      let (badEmails, goodEmails) = partitionEithers $
            fmap (\raw -> either (const (Left raw)) Right . validate . T.encodeUtf8 . T.pack $ raw) rawEmails
      case badEmails of
        (_:_) -> pure $ Left . BUserError $ InvalidEmail_AddUser (fmap T.pack badEmails)
        [] -> do
          results <- forM goodEmails $ \email -> do
            mCompanyId <- withDbEnvE @cfg $ getCompanyId uTypeTbl acctID
            case mCompanyId of
              Nothing -> pure $ Left . BUserError $ NoOrgCode $ T.pack . show $ acctID
              Just companyId -> do
                createNewAccountWithSetupEmail @api @db @cfg proxy email (IsGroupUser email companyId) resetRoute mkEmail >>= \case
                  Left bError -> pure $ Left $ AddUser_Signup <$> bError
                  Right a -> pure $ Right a
          pure $ () <$ sequenceA results

sepByCommas :: T.Text -> Either ParseError [String]
sepByCommas = parse p ""
  where
    p = sepBy (some $ noneOf [',']) (char ',')
