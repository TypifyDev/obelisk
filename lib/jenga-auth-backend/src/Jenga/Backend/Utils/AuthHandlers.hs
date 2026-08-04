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
import Jenga.Snap.Extras (writeJSON)
import Database.Beam.Schema
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.Types (SqlSerial(..))
import Data.Signed
import Data.Signed.ClientSession as CSK

import Data.Pool
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Either
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static ()


-- | This is currently a toss up as we switch from an old semi-broken system
-- | to a new one ... which may be also just as unknown
-- | this works but ... why does this happen??
removeQuotes :: T.Text -> T.Text
removeQuotes = T.dropWhile (== '"') . T.dropWhileEnd (== '"')
  -- or is it id
  -- or is it: T.init . (T.drop 1)

removeQuotesBS :: BS.ByteString -> BS.ByteString
removeQuotesBS =
  --BS.dropWhile (== '"') . BS.dropWhileEnd (== '"')
  T.encodeUtf8
  . removeQuotes
  . T.decodeUtf8


--- Cookie Authentication Handlers

-- | Authenticate via cookie and run the given action.
-- Takes explicit Key and AuthCookieName since it runs in MonadSnap.
privateRoute
  :: forall m a.
     ( A.ToJSON a
     , MonadSnap m
     )
  => AuthCookieName
  -> CS.Key
  -> (Id Account -> m a)
  -> m ()
privateRoute (AuthCookieName authCookieName) key fma = do
  let writeJSON'' :: MonadSnap m1 => Either (BackendError ()) () -> m1 ()
      writeJSON'' = writeJSON

  mCookie <- liftSnap $ getCookie $ T.encodeUtf8 authCookieName

  case mCookie of
    Nothing -> writeJSON'' $ (Left NoAuth_NoCookie)
    Just cookie -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Rhyolite.Account.Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookie
      case readSignedWithKey key signed of
        Nothing -> writeJSON'' $ (Left NoAuth_CantReadKey)
        Just acctId -> do
          _ <- fma acctId -- AcctID $ fromIntegral int64
          pure ()


-- | Is copy of privateRoute with allowed users
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
      let
        --removeQuotes = id -- T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
      case readSignedWithKey key signed of
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

-- NOTE: could easily generalize this if we ever have other signed data in headers?
getAccountIdFromCookies
  :: ( A.ToJSON e
     , MonadSnap m
     )
  => CS.Key
  -> AuthCookieName
  -> m (Either (BackendError e) AccountId)
getAccountIdFromCookies cskey (AuthCookieName authCookieName) = do
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  case mCookie of
    Nothing -> pure $ Left NoAuth_NoCookie
    Just cookieSignedId -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        eithDecodedB64 = B64.decode . cookieValue $ cookieSignedId
      case eithDecodedB64 of
        Left _decodeErr -> pure $ Left NoAuth_CantReadKey -- "Base64 Decode Error: " <> T.pack decodeErr
        Right decodedB64 -> do
          let
            -- for type annotation
            signed :: Signed (PrimaryKey Account Identity)
            signed = Signed . removeQuotes . T.decodeUtf8 $ decodedB64
          case readSignedWithKey cskey signed of
            Nothing -> pure $ Left NoAuth_CantReadKey
            Just acctId -> pure $ Right acctId

-- | is copy of privateRoute with allowed users
constrainedPrivateRoute
  :: forall db m a.
     ( MonadSnap m
     , Database Postgres db
     )
  => PgTable Postgres db UserTypeTable
  -> AuthCookieName
  -> Pool Connection
  -> Key
  -> NonEmpty.NonEmpty UserType
  -> (Id Account -> m a)
  -> m ()
constrainedPrivateRoute uTypeTbl (AuthCookieName authCookieName) dbConn key userTypesAllowed fma = do
  let writeJSON'' :: forall x e. (A.ToJSON x, A.ToJSON e) => Either (BackendError e) x -> Snap ()
      writeJSON'' = writeJSON
  mCookieAID <- getCookie $ T.encodeUtf8 authCookieName
  case mCookieAID of
    Nothing -> liftSnap $ do
      writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError ()) ())
    Just cookieAID -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen / why do we need this drop?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . removeQuotes . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
      case readSignedWithKey key signed of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError ()) ())
        -- We still have this UserType on the frontend so that
        -- we can run conditional logic on what to show the user
        -- but for safety we ask the type ourselves as the user could just enter in a fake one
        Just acctId@(AccountId (SqlSerial _)) -> do
          userType' <- runSerializable dbConn $ getUserType uTypeTbl acctId
          case userType' of
            Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_NoUserTypeCookie :: Either (BackendError ()) ())
            Just userType -> do
              case elem userType userTypesAllowed of
                False -> pure ()
                True -> do
                  _ <- fma acctId
                  pure ()

privateRouteJSONOut
  :: forall m a e.
     ( A.ToJSON a
     , A.ToJSON e
     , MonadSnap m
     )
  => AuthCookieName
  -> Key
  -> (Id Account -> m (Either (BackendError e) a))
  -> m ()
privateRouteJSONOut (AuthCookieName authCookieName) key fma = do
  let writeJSON'' :: forall x1 e1. (A.ToJSON x1, A.ToJSON e1)
        => Either (BackendError e1) x1 -> Snap ()
      writeJSON'' = writeJSON
  mCookie <- getCookie $ T.encodeUtf8 authCookieName
  case mCookie of
    Nothing -> do
      liftSnap $ writeJSON'' $ (Left NoAuth_NoCookie :: Either (BackendError e) a)
    Just cookieAID -> do
      let
        --removeQuotes = T.init . (T.drop 1) -- TODO: why does this happen ?
        signed :: Signed (PrimaryKey Account Identity)
        signed = Signed . T.decodeUtf8 . (fromRight undefined) . B64.decode . cookieValue $ cookieAID
      case (readSignedWithKey key) signed of
        Nothing -> liftSnap $ writeJSON'' $ (Left NoAuth_CantReadKey :: Either (BackendError e) a)
        Just acctId@(AccountId (SqlSerial _)) -> do
          a <- fma acctId
          liftSnap $ writeJSON'' a

type HasAdminReporting cfg es db n =
  ( HasJengaTable Postgres db LogItemRow
  , HasJengaTable Postgres db SendEmailTask
  , Reader cfg :> es, HasConfig cfg (Pool Connection)
  , Reader cfg :> es, HasConfig cfg AdminEmail
  , IOE :> es
  )

wsRequestAuth
  :: forall db cfg es be n e a.
     ( IOE :> es
     , MonadCatch (Eff es)
     , Show e
     , SpecificError (BackendError e)
     , Reader cfg :> es, HasConfig cfg CS.Key
     , HasAdminReporting cfg es db n
     , HasJsonNotifyTbl be SendEmailTask n
     )
  => Signed (Id Account)
  -> (Id Account -> Eff es (Either (BackendError e) a))
  -> Eff es (Either (BackendError e) a)
wsRequestAuth authToken k = do
  csk <- asksM @CS.Key @cfg
  case readSignedWithKey csk authToken of
    Just user -> withErrorReporting @db @cfg $ k user
    Nothing -> pure . Left $ NoAuth
