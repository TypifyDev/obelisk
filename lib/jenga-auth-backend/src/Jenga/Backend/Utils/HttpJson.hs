module Jenga.Backend.Utils.HttpJson where

-- import Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.AuthHandlers
import Jenga.Backend.Utils.Snap
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.Email
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.Schema
import Jenga.Common.BeamExtras


import Rhyolite.Account
import Database.Beam.Postgres
import Database.Beam.Schema
import Web.ClientSession as CS
import Snap (MonadSnap, liftSnap)
import Jenga.Snap.Extras ()

import Control.Monad (void)
import Control.Exception.Lifted (catch, SomeException(..))
import Control.Monad.Catch (MonadCatch)
import Data.Pool
import Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


-- | Rewrite with getRequestBodyJSON
withRequestBodyJSON
  :: forall a m.
     ( Aeson.FromJSON a
     , MonadSnap m
     )
  => (a -> m ())
  -> m ()
withRequestBodyJSON withF = do
  raw <- getRequestBody
  case Aeson.eitherDecode raw :: Either String a of
    Left e -> liftSnap . writeJSON' $ (Left . BInvalidRequest $ T.pack e :: Either (BackendError ()) ())
    Right x -> withF x

withPublicJSONRequestResponse
  :: forall db cfg be a b err es n
  . ( Aeson.FromJSON a
    , Aeson.ToJSON b
    , Aeson.ToJSON err
    , Show err
    , SpecificError (BackendError err)
    , IOE :> es
    , MonadCatch (Eff es)
    , Database Postgres db
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , Reader cfg :> es, HasConfig cfg CS.Key
    , Reader cfg :> es, HasConfig cfg AuthCookieName
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (a -> Eff es (Either (BackendError err) b))
  -> LBS.ByteString
  -> Eff es ()
withPublicJSONRequestResponse f raw = void $ withJSONRequestResponse @db @cfg f raw
    -- \case
    -- Right _ -> pure ()
    -- Left err -> emailGalenWithEnv . LT.fromStrict $ err

  -- x :: Either ApiError b <- withJSONRequestResponse (withF )

  -- when (isLeft x) $  do
  --   emailGalenWithEnv $ LT.fromStrict . T.pack $ show x
  --   -- |^emailGalen emailConfig (LT.fromStrict . T.pack $ show x) -- REIMPLEMENT
  --   pure ()

withConstrainedPrivateJSONRequestResponse
  :: forall db cfg be a b e es n
  . ( FromJSON a
    , ToJSON b
    , ToJSON e
    , Show b
    , Show e
    , SpecificError (BackendError e)
    , IOE :> es
    , MonadCatch (Eff es)
    , MonadSnap (Eff es)
    , Database Postgres db
    , Reader cfg :> es, HasConfig cfg CS.Key
    , Reader cfg :> es, HasConfig cfg AuthCookieName
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => NE.NonEmpty UserType
  -> (Id Account -> a -> Eff es (Either (BackendError e) b))
  -> Eff es ()
withConstrainedPrivateJSONRequestResponse allowedUserTypes withF = do
  csk <- asksM @CS.Key @cfg
  dbConn <- asksM @(Pool Connection) @cfg
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  authCookieName <- asksM @AuthCookieName @cfg
  constrainedPrivateRoute @db uTypeTbl authCookieName dbConn csk allowedUserTypes $ \acctId -> do
    raw <- getRequestBody
    withJSONRequestResponse @db @cfg (withF acctId) raw
    -- when (isLeft x) $ liftIO $ do
    --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
    --   print (LT.fromStrict . T.pack $ show acctId <> show x)

withDependentPrivateJSONRequestResponse
  :: forall db cfg be a b e es n
  . ( FromJSON a
    , ToJSON b
    , ToJSON e
    , Show b
    , Show e
    , SpecificError (BackendError e)
    , IOE :> es
    , MonadCatch (Eff es)
    , MonadSnap (Eff es)
    , Database Postgres db
    , Reader cfg :> es, HasConfig cfg CS.Key
    , Reader cfg :> es, HasConfig cfg AuthCookieName
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , HasJengaTable Postgres db UserTypeTable
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (UserType -> Id Account -> a -> Eff es (Either (BackendError e) b))
  -> Eff es ()
withDependentPrivateJSONRequestResponse withF = do
  csk <- asksM @CS.Key @cfg -- Cfg _clientSessionKey
  dbConn <- asksM @(Pool Connection) @cfg -- Cfg _dbPool
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  authCookieName <- asksM @AuthCookieName @cfg
  dependentPrivateRoute uTypeTbl authCookieName dbConn csk $ \userType acctId -> do
    raw <- getRequestBody
    withJSONRequestResponse @db @cfg (withF userType acctId) raw

-- | This function is meant to be completely independent of Authentication, so that it can be
-- | wrapped by different auth schemes
withJSONRequestResponse
  :: forall db cfg be a b err es n
  . ( FromJSON a
    , ToJSON b
    , ToJSON err
    , IOE :> es
    , Show err
    , MonadCatch (Eff es)
    , SpecificError (BackendError err)
    , Reader cfg :> es, HasConfig cfg (Pool Connection)
    , Reader cfg :> es, HasConfig cfg AdminEmail
    , HasJengaTable Postgres db LogItemRow
    , HasJengaTable Postgres db SendEmailTask
    , HasJsonNotifyTbl be SendEmailTask n
    )
  => (a -> Eff es (Either (BackendError err) b))
  -> LBS.ByteString
  -> Eff es (Either (BackendError err) b)
withJSONRequestResponse withF raw = do
  x :: Either (BackendError e) b <- case Aeson.eitherDecode raw :: Either String a of
    Left e -> pure $ Left . BInvalidRequest $ T.pack e
    Right good -> catch (withF good) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  reportWhenError @db @cfg x
  --pure x

withPrivateRequestResponse
  :: forall db cfg es be a e n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , IOE :> es
     , MonadCatch (Eff es)
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Id Account
  -> (Id Account -> LBS.ByteString -> Eff es (Either (BackendError e) a))
  -> LBS.ByteString
  -> Eff es (Either (BackendError e) a)
withPrivateRequestResponse id_ withF raw = withPrivateErrorHandling @db @cfg id_ withF raw

withPrivateErrorHandling
  :: forall db cfg es be a e n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , IOE :> es
     , MonadCatch (Eff es)
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Id Account
  -> (Id Account -> LBS.ByteString -> Eff es (Either (BackendError e) a))
  -> LBS.ByteString
  -> Eff es (Either (BackendError e) a)
withPrivateErrorHandling acctID withF raw = do
  x <- catch (withF acctID raw) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  reportWhenError @db @cfg x
  -- when (isLeft x) $ liftIO $ do
  --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
  --   print (LT.fromStrict . T.pack $ show x)
  --   logFor acctID (LT.fromStrict . T.pack $ show x)
  -- pure x

-- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
withPublicRequestResponse
  :: forall db cfg es be n e a.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , IOE :> es
     , MonadCatch (Eff es)
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (LBS.ByteString -> Eff es (Either (BackendError e) a))
  -> LBS.ByteString
  -> Eff es (Either (BackendError e) a)
withPublicRequestResponse withF raw = withPublicErrorHandling @db @cfg withF raw

-- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
withPublicErrorHandling
  :: forall db cfg es be a e n.
     ( Show a
     , Show e
     , SpecificError (BackendError e)
     , IOE :> es
     , MonadCatch (Eff es)
     , Reader cfg :> es, HasConfig cfg AdminEmail
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , HasJengaTable Postgres db SendEmailTask
     , HasJengaTable Postgres db LogItemRow
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => (LBS.ByteString -> Eff es (Either (BackendError e) a))
  -> LBS.ByteString
  -> Eff es (Either (BackendError e) a)
withPublicErrorHandling withF raw = do
  x <- catch (withF raw) (\(SomeException e) -> pure $ Left . BException $ T.pack . show $ e)
  reportWhenError @db @cfg x
  -- when (isLeft x) $ liftIO $ do
  --   -- |^ FIX emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
  --   logNoAuth (LT.fromStrict . T.pack $ show x)
  -- pure x

-- TODO(Galen): use requestBodyJSON
theoreticalJSONMonad
  :: forall a b es.
     (FromJSON a, ToJSON b, IOE :> es, MonadCatch (Eff es))
  => (a -> Eff es (Either ApiError b))
  -> LBS.ByteString
  -> Eff es (Either ApiError b)
theoreticalJSONMonad withF raw = do
  x :: Either ApiError b <- case Aeson.eitherDecode raw :: Either String a of
    Left e -> pure $ Left $ T.pack e
    Right good -> catch (withF good) (\(SomeException e) -> pure $ Left $ T.pack . show $ e)
  pure x
