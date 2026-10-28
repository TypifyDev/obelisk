module Jenga.Backend.Handlers.Auth.OAuth where

import Jenga.Backend.DB.Auth
-- import Backend.DB

-- import Backend.Utils.Log
import Jenga.Backend.Handlers.Auth.OAuth.Error (OAuthError (..))
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Cookies (addAuthCookieHeader, addUserTypeCookieHeader)
import Jenga.Common.Schema
import Jenga.Common.OAuth
import Jenga.Common.Auth hiding (OAuthError)
import Jenga.Common.BeamExtras (Id)

import Rhyolite.Account
import Control.Monad.Trans.Reader (runReaderT)
import Data.Kind (Type)
import Jenga.Route
import Jenga.Route.Servant (HasRoute)
import Data.Proxy (Proxy)
import Obelisk.OAuth.AccessToken (TokenGrant (..))
import Obelisk.OAuth.Authorization (OAuth (..), RedirectUriParams (..))
import qualified Network.HTTP.Client.MultipartFormData as Http
import Database.Beam.Postgres
import Database.Beam.Schema
import Database.Beam.Backend.SQL.Types (SqlSerial(..))

import Snap
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import Network.HTTP.Client.TLS
import Web.ClientSession as CS
import Data.Pool
import Control.Monad.IO.Class
import Data.Dependent.Sum
import Data.Maybe (isJust, fromMaybe)
import Data.List (find)
import Data.Functor.Identity
import Data.Int (Int64)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Email.Validate as EmailValidate

import Effectful (Eff, (:>), IOE)
import Effectful.Reader.Static (Reader)
import qualified Effectful.Reader.Static as ER

-- | A pre-rendered redirect URI to hand back to an OAuth provider during the
-- token-exchange step. Wrapping in a newtype makes it obvious at the call
-- site that this is the caller's responsibility to construct correctly
-- (via Jenga.Route.Servant, obelisk, or hand-rolled), and stops arbitrary
-- Text from being passed through by mistake.
newtype RedirectUri = RedirectUri { unRedirectUri :: T.Text }
  deriving (Show, Eq)

-- | Build a token-exchange POST request. Inlines the form-encoding that
-- Obelisk.OAuth.AccessToken.getOauthToken did, but takes a fully-rendered
-- RedirectUri instead of an Encoder.
oauthTokenRequest
  :: String            -- ^ Token-exchange URL (provider-specific)
  -> RedirectUri       -- ^ Fully-rendered redirect URI to send back to the provider
  -> T.Text            -- ^ OAuth client ID
  -> T.Text            -- ^ OAuth client secret
  -> TokenGrant        -- ^ Authorization code or refresh token
  -> IO Http.Request
oauthTokenRequest tokenUrl (RedirectUri redirectUri) clientId clientSecret grant = do
  req <- Http.parseRequest tokenUrl
  let grantParts = case grant of
        TokenGrant_AuthorizationCode code ->
          [ Http.partBS "grant_type" "authorization_code"
          , Http.partBS "code" code
          ]
        TokenGrant_RefreshToken refresh ->
          [ Http.partBS "grant_type" "refresh_token"
          , Http.partBS "refresh_token" refresh
          ]
      form =
        [ Http.partBS "client_id" (T.encodeUtf8 clientId)
        , Http.partBS "client_secret" (T.encodeUtf8 clientSecret)
        , Http.partBS "redirect_uri" (T.encodeUtf8 redirectUri)
        ] ++ grantParts
  Http.formDataBody form (req { Http.method = "POST" })

-- | Helper function to handle OAuth login with email-based account lookup
-- Validates email, creates or links account, and sets cookies
handleOAuthLogin
  :: forall (api :: Type -> Type) db cfg es frontendRoute oauthIdRow.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasRoute api (R frontendRoute)
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> String  -- ^ Email from OAuth provider
  -> Maybe oauthIdRow  -- ^ Existing OAuth ID row if user has logged in before
  -> (oauthIdRow -> Int64)  -- ^ Extract account ID from OAuth ID row
  -> (Id Account -> Eff es ())  -- ^ Insert new OAuth ID for account
  -> R frontendRoute  -- ^ Where to redirect after success
  -> Eff es (Either OAuthError (Id Account, Snap ()))
handleOAuthLogin proxy emailStr maybeOAuthID extractAccountId insertOAuthId redirectToRoute = do
  -- Validate email format
  case EmailValidate.validate (T.encodeUtf8 $ T.pack emailStr) of
    Left _err ->
      pure $ Left $ OAuthProviderError $
        "Invalid email format from OAuth provider: " <> T.pack emailStr
    Right _validEmail -> do
      eAccountID <- case maybeOAuthID of
        Nothing -> do
          -- New user signup or link to existing account
          (acctsTbl :: PgTable Postgres db Account) <- asksTableM

          -- Check if account with this email already exists
          maybeExistingAcct <- withDbEnvE @cfg $ getUserByEmail acctsTbl (T.pack emailStr)
          liftIO $ print $ isJust maybeExistingAcct
          case maybeExistingAcct of
            -- Link to existing account
            Just existingAcct -> do
              let existingAccountId = pk existingAcct
              insertOAuthId existingAccountId
              pure $ Right existingAccountId
            -- Create new account
            Nothing -> do
              accountID <- withDbEnvE @cfg $ newAccount acctsTbl (Email $ T.pack emailStr) Nothing
              case accountID of
                Nothing ->
                  pure $ Left $ OAuthAccountNotFound "Insert new account failed"
                Just aid -> do
                  insertOAuthId aid
                  -- Create user type for new OAuth user
                  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
                  withDbEnvE @cfg $ putNewUserType uTypeTbl aid Nothing
                  pure $ Right aid

        Just oauthIdRow -> do
          -- Existing user login
          pure $ Right $ AccountId $ SqlSerial $ extractAccountId oauthIdRow

      case eAccountID of
        Left err -> pure (Left err)
        Right accountID' -> do
          snapAction <- setCookiesAndRedirect @api @db @cfg proxy accountID' redirectToRoute
          pure $ Right (accountID', snapAction)

-- | Helper function to set both auth and user type cookies after OAuth login
setCookiesAndRedirect
  :: forall (api :: Type -> Type) db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasRoute api (R frontendRoute)
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> Id Account
  -> R frontendRoute
  -> Eff es (Snap ())
setCookiesAndRedirect _proxy accountID' redirectToRoute = do
  liftIO $ putStrLn $ "setCookiesAndRedirect called for account: " <> show accountID'
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  userType <- withDbEnvE @cfg $ getUserType uTypeTbl accountID'

  -- If user doesn't have a type, create one
  userType' <- case userType of
    Just uType -> pure uType
    Nothing -> do
      liftIO $ putStrLn $ "Creating missing user type for existing OAuth user: " <> show accountID'
      withDbEnvE @cfg $ putNewUserType uTypeTbl accountID' Nothing
      pure Self

  liftIO $ putStrLn $ "About to set cookies for user type: " <> show userType'
  userTypeCookieName <- getUserTypeCookieName <$> ER.asks (fromCfg :: cfg -> UserTypeCookieName)
  liftIO $ putStrLn $ "User type cookie name: " <> show userTypeCookieName
  liftIO $ putStrLn "User type cookie set, now setting auth cookie"
  authCookieName <- getAuthCookieName <$> ER.asks (fromCfg :: cfg -> AuthCookieName)
  liftIO $ putStrLn $ "Auth cookie name: " <> show authCookieName
  liftIO $ putStrLn "Both cookies set, checking response headers before redirect"

  link_ <- renderFullRouteFEE @api @frontendRoute @cfg redirectToRoute
  cfgVal <- ER.ask @cfg

  pure $ do
    flip runReaderT cfgVal $ addUserTypeCookieHeader userTypeCookieName userType'
    flip runReaderT cfgVal $ addAuthCookieHeader authCookieName accountID'
    -- Debug: inspect response headers before redirect
    resp <- Snap.getResponse
    let headers_ = Snap.listHeaders resp
    liftIO $ putStrLn $ "Response headers before redirect: " <> show headers_
    Snap.redirect $ T.encodeUtf8 (getLink link_)

-- TODO: they dont need to reset password if they have signed up with github
-- | TODO: not actually in use, should actually work no problem with github
oauthHandler
  :: forall (api :: Type -> Type) db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg GithubOAuthClientSecret
     , Reader cfg :> es, HasConfig cfg GithubOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db GithubID
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> DSum OAuth Identity
  -> RedirectUri                  -- ^ Fully-rendered redirect URI to hand back to the OAuth provider
  -> R frontendRoute
  -> R frontendRoute
  -> Eff es (Either OAuthError (Maybe (Id Account, T.Text), Snap ()))
oauthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing ->
      pure $ Left $ OAuthProviderError
        "github: authorization callback did not include an authorization code"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGithubOAuthClientID <$> ER.asks (fromCfg :: cfg -> GithubOAuthClientID)
      clientSecret <- getGithubOAuthClientSecret <$> ER.asks (fromCfg :: cfg -> GithubOAuthClientSecret)
      let oAuthUrl = "https://github.com/login/oauth/access_token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      req <- liftIO $ oauthTokenRequest oAuthUrl redirectUri clientId clientSecret
                        (TokenGrant_AuthorizationCode $ T.encodeUtf8 code)
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          noAuthLink <- renderFullRouteFEE @api @frontendRoute @cfg redirectNoAuth
          pure $ Right (Nothing, Snap.redirect (T.encodeUtf8 (getLink noAuthLink)))
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://api.github.com/user"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   , (Http.hUserAgent, "Ace Interview Prep Haskell Server")
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userGithub :: Maybe GitHubUser = Aeson.decode . Http.responseBody $ res
          case userGithub of
            Nothing ->
              pure $ Left $ OAuthDecodeError
                "github /user: could not decode GitHubUser (possibly no linked github account)"
            Just userGH -> do
              ghEmail <- case _githubUser_github_email userGH of
                Just email -> pure email
                Nothing -> do
                  reqEmails <- liftIO $ Http.parseRequest "https://api.github.com/user/emails"
                  let reqEmails' = reqEmails { Http.requestHeaders = Http.requestHeaders reqEmails <>
                                              [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                              , (Http.hUserAgent, "Ace Interview Prep Haskell Server")
                                              ]
                                            }
                  resEmails <- liftIO $ flip Http.httpLbs tlsMgr reqEmails'
                  let emails = Aeson.decode . Http.responseBody $ resEmails :: Maybe [GitHubEmail]
                  case emails of
                    Just emailList -> do
                      let primaryEmail = find (\e -> _githubEmail_primary e && _githubEmail_verified e) emailList
                      case primaryEmail of
                        Just e -> pure $ _githubEmail_email e
                        Nothing -> pure $ _githubUser_login userGH
                    Nothing -> pure $ _githubUser_login userGH

              (ghTbl :: PgTable Postgres db GithubID) <- asksTableM
              maybeGitID <- withDbEnvE @cfg $ getGithubUserIfTheyExist ghTbl userGH

              liftIO $ putStrLn ghEmail
              let ghDisplayName = T.pack $ fromMaybe (_githubUser_login userGH) (_githubUser_name userGH)
              -- handleOAuthLogin now bubbles OAuthError; forward Left, wrap Right.
              eLogin <- handleOAuthLogin @api @db @cfg
                proxy
                ghEmail
                maybeGitID
                (\(GithubID _ghid uid) -> uid)
                (\aid -> withDbEnvE @cfg $ insertNewGithubID ghTbl userGH aid)
                redirectToRoute
              case eLogin of
                Left err -> pure $ Left err
                Right (acctId, snapAction) ->
                  pure $ Right (Just (acctId, ghDisplayName), snapAction)

-- | Google OAuth handler
googleOAuthHandler
  :: forall (api :: Type -> Type) db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg GoogleOAuthClientSecret
     , Reader cfg :> es, HasConfig cfg GoogleOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db GoogleID
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> DSum OAuth Identity
  -> RedirectUri                  -- ^ Fully-rendered redirect URI to hand back to the OAuth provider
  -> R frontendRoute
  -> R frontendRoute
  -> Eff es (Either OAuthError (Maybe (Id Account, T.Text), Snap ()))
googleOAuthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing ->
      pure $ Left $ OAuthProviderError
        "google: authorization callback did not include an authorization code"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGoogleOAuthClientID <$> ER.asks (fromCfg :: cfg -> GoogleOAuthClientID)
      clientSecret <- getGoogleOAuthClientSecret <$> ER.asks (fromCfg :: cfg -> GoogleOAuthClientSecret)
      let oAuthUrl = "https://oauth2.googleapis.com/token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      req <- liftIO $ oauthTokenRequest oAuthUrl redirectUri clientId clientSecret
                        (TokenGrant_AuthorizationCode $ T.encodeUtf8 code)
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          noAuthLink <- renderFullRouteFEE @api @frontendRoute @cfg redirectNoAuth
          pure $ Right (Nothing, Snap.redirect (T.encodeUtf8 (getLink noAuthLink)))
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://www.googleapis.com/oauth2/v2/userinfo"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userGoogle :: Maybe GoogleUser = Aeson.decode . Http.responseBody $ res
          case userGoogle of
            Nothing ->
              pure $ Left $ OAuthDecodeError
                "google /oauth2/v2/userinfo: could not decode GoogleUser"
            Just userG -> do
              (googleTbl :: PgTable Postgres db GoogleID) <- asksTableM
              maybeGoogleID <- withDbEnvE @cfg $ getGoogleUserIfTheyExist googleTbl userG

              let googleEmail = _googleUser_email userG
                  googleDisplayName = T.pack $ fromMaybe (takeWhile (/= '@') googleEmail) (_googleUser_name userG)
              eLogin <- handleOAuthLogin @api @db @cfg
                proxy
                googleEmail
                maybeGoogleID
                (\(GoogleID _gid uid) -> uid)
                (\aid -> withDbEnvE @cfg $ insertNewGoogleID googleTbl userG aid)
                redirectToRoute
              case eLogin of
                Left err -> pure $ Left err
                Right (acctId, snapAction) ->
                  pure $ Right (Just (acctId, googleDisplayName), snapAction)

-- | Discord OAuth handler
discordOAuthHandler
  :: forall (api :: Type -> Type) db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , HasConfig cfg (FullRouteEncoder api frontendRoute)
     , HasRoute api (R frontendRoute)
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg DiscordOAuthClientSecret
     , Reader cfg :> es, HasConfig cfg DiscordOAuthClientID
     , HasJengaTable Postgres db Account
     , HasJengaTable Postgres db DiscordID
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> DSum OAuth Identity
  -> RedirectUri                  -- ^ Fully-rendered redirect URI to hand back to the OAuth provider
  -> R frontendRoute
  -> R frontendRoute
  -> Eff es (Either OAuthError (Maybe (Id Account, T.Text), Snap ()))
discordOAuthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing ->
      pure $ Left $ OAuthProviderError
        "discord: authorization callback did not include an authorization code"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getDiscordOAuthClientID <$> ER.asks (fromCfg :: cfg -> DiscordOAuthClientID)
      clientSecret <- getDiscordOAuthClientSecret <$> ER.asks (fromCfg :: cfg -> DiscordOAuthClientSecret)
      let oAuthUrl = "https://discord.com/api/oauth2/token"
      tlsMgr <- liftIO $ Http.newManager tlsManagerSettings

      req <- liftIO $ oauthTokenRequest oAuthUrl redirectUri clientId clientSecret
                        (TokenGrant_AuthorizationCode $ T.encodeUtf8 code)
      rsp <- liftIO $ flip Http.httpLbs tlsMgr (req { Http.requestHeaders = Http.requestHeaders req
                                                      <> [(Http.hAccept, "application/json")] }
                                               )
      let accessToken = fmap access_token . Aeson.decode . Http.responseBody $ rsp
      case accessToken of
        Nothing -> do
          noAuthLink <- renderFullRouteFEE @api @frontendRoute @cfg redirectNoAuth
          pure $ Right (Nothing, Snap.redirect (T.encodeUtf8 (getLink noAuthLink)))
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://discord.com/api/users/@me"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userDiscord :: Maybe DiscordUser = Aeson.decode . Http.responseBody $ res
          case userDiscord of
            Nothing ->
              pure $ Left $ OAuthDecodeError
                "discord /users/@me: could not decode DiscordUser"
            Just userD -> do
              (discordTbl :: PgTable Postgres db DiscordID) <- asksTableM
              maybeDiscordID <- withDbEnvE @cfg $ getDiscordUserIfTheyExist discordTbl userD

              let discordEmail = fromMaybe (_discordUser_username userD) $ _discordUser_email userD
                  discordDisplayName = T.pack $ _discordUser_username userD
              eLogin <- handleOAuthLogin @api @db @cfg
                proxy
                discordEmail
                maybeDiscordID
                (\(DiscordID _did uid) -> uid)
                (\aid -> withDbEnvE @cfg $ insertNewDiscordID discordTbl userD aid)
                redirectToRoute
              case eLogin of
                Left err -> pure $ Left err
                Right (acctId, snapAction) ->
                  pure $ Right (Just (acctId, discordDisplayName), snapAction)
