{-# LANGUAGE AllowAmbiguousTypes #-}

module Jenga.Backend.Utils.AuthHandlers where


import Jenga.Backend.Utils.Query
import Jenga.Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Backend.DB.Auth
import Jenga.Backend.Utils.Snap
import Jenga.Common.Errors
import Jenga.Common.Auth
import Jenga.Common.BeamExtras
import Jenga.Common.Schema

import Rhyolite.Account
import Web.ClientSession as CS

import Snap
import qualified Data.Aeson as Aeson
import Database.Beam.Schema
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.Types (SqlSerial(..))
import Data.Signed
import Data.Signed.ClientSession as CSK

import Data.Pool
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS


-- | Accept the quoted token representation used by existing clients.
removeQuotes :: T.Text -> T.Text
removeQuotes = T.dropWhile (== '"') . T.dropWhileEnd (== '"')

removeQuotesBS :: BS.ByteString -> BS.ByteString
removeQuotesBS =
  BS.dropWhile (== 34) . BS.dropWhileEnd (== 34)

-- | Invalid encoding and invalid signatures are both authentication failures.
decodeAccountCookie :: Key -> BS.ByteString -> Maybe AccountId
decodeAccountCookie key raw = do
  bytes <- either (const Nothing) Just $ B64.decode raw
  token <- either (const Nothing) Just $ T.decodeUtf8' bytes
  readSignedWithKey key $ Signed $ removeQuotes token


--- Cookie Authentication Handlers

privateRoute
  :: forall cfg m a.
     ( A.ToJSON a
     , MonadSnap m
     , HasConfig cfg AuthCookieName
     , HasConfig cfg CS.Key
     )
  => (Id Account -> ReaderT cfg m a)
  -> ReaderT cfg m ()
privateRoute fma = do
  let writeJSON'' :: MonadSnap m => Either (BackendError ()) () -> ReaderT cfg m ()
      writeJSON'' = lift . writeJSON

  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- liftSnap $ getCookie $ T.encodeUtf8 authCookieName

  case mCookie of
    Nothing -> writeJSON'' $ (Left NoAuth_NoCookie)
    Just cookie -> do
      key <- asksM
      case decodeAccountCookie key (cookieValue cookie) of
        Nothing -> writeJSON'' $ (Left NoAuth_CantReadKey)
        Just acctId -> do
          _ <- fma acctId -- AcctID $ fromIntegral int64
          pure ()




-- | Authenticate the cookie and load the account's current role from the database.
dependentPrivateRoute
  :: forall db m a.
     ( MonadSnap m
     , A.ToJSON a
     , Database Postgres db
     )
  => PgTable Postgres db UserTypeTable
  -> AuthCookieName
  -> Pool Connection
  -> CSK.Key
  -> (UserType -> Id Account -> m a)
  -> m ()
dependentPrivateRoute uTypeTbl (AuthCookieName authCookieName) dbConn key fma = do
  mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
  case mCookieAID of
    Nothing -> do
      liftIO $ putStrLn "depeendentPrivateRoute: No Cookie Found"
      liftSnap $ writeJSON' $ (Left NoAuth_NoCookie :: Either (BackendError ()) ()) --  :: Either T.Text ())
    Just cookieAID -> do
      case decodeAccountCookie key (cookieValue cookieAID) of
        Nothing -> liftSnap $ writeJSON' $ (Left NoAuth_CantReadKey  :: Either (BackendError ()) ()) -- "couldnt read key" :: Either T.Text ())
        -- We still have this UserType on the frontend so that
        -- we can run conditional logic on what to show the user
        -- but for safety we ask the type ourselves as the user could just enter in a fake one
        Just acctId@(AccountId (SqlSerial _)) -> do
          userType' <- runSerializable dbConn $ getUserType uTypeTbl acctId
          case userType' of
            Nothing -> liftSnap $ writeJSON' $ (Left NoAuth_NoUserTypeCookie :: Either (BackendError ()) ())
            Just userType -> do
              _ <- fma userType acctId
              pure ()

-- | Decode the signed account cookie, preserving typed authentication failures.
getAccountIdFromCookies
  :: ( A.ToJSON e
     , MonadSnap m
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     )
  => ReaderT cfg m (Either (BackendError e) AccountId)
getAccountIdFromCookies = do
  cskey <- asksM -- asksCfg _clientSessionKey
  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  case mCookie of
    Nothing -> pure $ Left NoAuth_NoCookie
    Just cookieSignedId -> pure $
      maybe (Left NoAuth_CantReadKey) Right $
        decodeAccountCookie cskey $ cookieValue cookieSignedId

-- | Authenticate and restrict access to the supplied database roles.
constrainedPrivateRoute
  :: forall db cfg m a.
     ( MonadSnap m
     , Database Postgres db
     , HasConfig cfg CS.Key
     , HasConfig cfg AuthCookieName
     , HasJengaTable Postgres db UserTypeTable
     )
  => Pool Connection
  -> Key
  -> NonEmpty.NonEmpty UserType
  -> (Id Account -> ReaderT cfg m a)
  -> ReaderT cfg m ()
constrainedPrivateRoute dbConn key userTypesAllowed fma = do
  let writeJSON'' :: forall x e. (A.ToJSON x, A.ToJSON e) => Either (BackendError e) x -> Snap ()
      writeJSON'' = writeJSON
  authCookieName <- getAuthCookieName <$> asksM
  mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
  case mCookieAID of
    Nothing -> liftSnap $ do
      writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError ()) ())
    Just cookieAID -> do
      case decodeAccountCookie key (cookieValue cookieAID) of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError ()) ())
        -- We still have this UserType on the frontend so that
        -- we can run conditional logic on what to show the user
        -- but for safety we ask the type ourselves as the user could just enter in a fake one
        Just acctId@(AccountId (SqlSerial _)) -> do
          (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
          userType' <- runSerializable dbConn $ getUserType uTypeTbl acctId
          case userType' of
            Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_NoUserTypeCookie :: Either (BackendError ()) ())
            Just userType -> do
              case elem userType userTypesAllowed of
                False -> liftSnap $ writeJSON'' (Left NoAuth :: Either (BackendError ()) ())
                True -> do
                  _ <- fma acctId
                  pure ()

privateRouteJSONOut
  :: forall cfg m a e.
     ( A.ToJSON a
     , A.ToJSON e
     , MonadSnap m
     , HasConfig cfg AuthCookieName
     )
  => Key
  -> (Id Account -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m ()
privateRouteJSONOut key fma = do
  let writeJSON'' :: forall x1 e1. (A.ToJSON x1, A.ToJSON e1)
        => Either (BackendError e1) x1 -> Snap ()
      writeJSON'' = writeJSON
  authCookieName <- getAuthCookieName <$> asksM
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  case mCookie of
    Nothing -> do
      liftSnap $ writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError e) a)
    Just cookieAID -> do
      case decodeAccountCookie key (cookieValue cookieAID) of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError e) a)
        Just acctId@(AccountId (SqlSerial _)) -> do
          a <- fma acctId
          liftSnap $ writeJSON'' a

type HasAdminReporting be cfg db n =
  ( HasJengaTable Postgres db LogItemRow
  , HasJengaTable Postgres db SendEmailTask
  , HasConfig cfg (Pool Connection)
  , HasConfig cfg AdminEmail
  , HasJsonNotifyTbl be SendEmailTask n
  )

wsRequestAuth
  :: forall db cfg be n e m a.
     ( MonadIO m
     , MonadCatch m
     , Show e
     , SpecificError (BackendError e)
     , HasConfig cfg CS.Key
     , HasAdminReporting be cfg db n
     )
  => Signed (Id Account)
  -> (Id Account -> ReaderT cfg m (Either (BackendError e) a))
  -> ReaderT cfg m (Either (BackendError e) a)
wsRequestAuth authToken k = do
  csk <- asksM
  case readSignedWithKey csk authToken of
    Just user -> withErrorReporting @db $ k user
    Nothing -> pure . Left $ NoAuth

writeJSON :: (Aeson.ToJSON a, MonadSnap m) => a -> m ()
writeJSON x = do
  modifyResponse $ setContentType "application/json"
  writeLBS (Aeson.encode x)
