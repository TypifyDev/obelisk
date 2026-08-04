module Jenga.Backend.Handlers.Auth.OAuth where

import Jenga.Backend.DB.Auth
-- import Backend.DB

-- import Backend.Utils.Log
import Jenga.Backend.Utils.Snap
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Cookies (addAuthCookieHeader, addUserTypeCookieHeader)
import Jenga.Common.Schema
import Jenga.Common.OAuth
import Jenga.Common.Auth
import Jenga.Common.BeamExtras (Id)

import Rhyolite.Account
import Jenga.Route
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
import Effectful.Reader.Static ()

-- | A pre-rendered redirect URI to hand back to an OAuth provider during the
-- token-exchange step. Wrapping in a newtype makes it obvious at the call
-- site that this is the caller's responsibility to construct correctly
-- (via jenga-route-servant, obelisk, or hand-rolled), and stops arbitrary
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
  :: forall api db cfg es frontendRoute oauthIdRow.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
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
  -> Eff es (Id Account, Snap ())
handleOAuthLogin proxy emailStr maybeOAuthID extractAccountId insertOAuthId redirectToRoute = do
  -- Validate email format
  case EmailValidate.validate (T.encodeUtf8 $ T.pack emailStr) of
    Left _err -> error $ "Invalid email format from OAuth provider: " <> emailStr
    Right _validEmail -> do
      accountID' <- case maybeOAuthID of
        Nothing -> do
          -- New user signup or link to existing account
          (acctsTbl :: PgTable Postgres db Account) <- asksTableM

          -- Check if account with this email already exists
          maybeExistingAcct <- withDbEnv @cfg $ getUserByEmail acctsTbl (T.pack emailStr)
          liftIO $ print $ isJust maybeExistingAcct
          case maybeExistingAcct of
            -- Link to existing account
            Just existingAcct -> do
              let existingAccountId = pk existingAcct
              insertOAuthId existingAccountId
              pure existingAccountId
            -- Create new account
            Nothing -> do
              accountID <- withDbEnv @cfg $ newAccount acctsTbl (Email $ T.pack emailStr) Nothing
              case accountID of
                Nothing -> error "Insert new account failed"
                Just aid -> do
                  insertOAuthId aid
                  -- Create user type for new OAuth user
                  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
                  withDbEnv @cfg $ putNewUserType uTypeTbl aid Nothing
                  pure aid

        Just oauthIdRow -> do
          -- Existing user login
          pure $ AccountId $ SqlSerial $ extractAccountId oauthIdRow

      snapAction <- setCookiesAndRedirect @api @db @cfg proxy accountID' redirectToRoute
      pure (accountID', snapAction)

-- | Helper function to set both auth and user type cookies after OAuth login
setCookiesAndRedirect
  :: forall api db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
     , Reader cfg :> es, HasConfig cfg AuthCookieName
     , Reader cfg :> es, HasConfig cfg UserTypeCookieName
     , Reader cfg :> es, HasConfig cfg DomainOption
     , Reader cfg :> es, HasConfig cfg CS.Key
     , Reader cfg :> es, HasConfig cfg (Pool Connection)
     , Reader cfg :> es, HasConfig cfg BaseURL
     , HasRoute api (R frontendRoute)
     , HasJengaTable Postgres db UserTypeTable
     )
  => Proxy api
  -> Id Account
  -> R frontendRoute
  -> Eff es (Snap ())
setCookiesAndRedirect proxy accountID' redirectToRoute = do
  liftIO $ putStrLn $ "setCookiesAndRedirect called for account: " <> show accountID'
  (uTypeTbl :: PgTable Postgres db UserTypeTable) <- asksTableM
  userType <- withDbEnv @cfg $ getUserType uTypeTbl accountID'

  -- If user doesn't have a type, create one
  userType' <- case userType of
    Just uType -> pure uType
    Nothing -> do
      liftIO $ putStrLn $ "Creating missing user type for existing OAuth user: " <> show accountID'
      withDbEnv @cfg $ putNewUserType uTypeTbl accountID' Nothing
      pure Self

  liftIO $ putStrLn $ "About to set cookies for user type: " <> show userType'
  userTypeCookieName <- getUserTypeCookieName <$> asksM @UserTypeCookieName @cfg
  liftIO $ putStrLn $ "User type cookie name: " <> show userTypeCookieName
  domainOpts <- asksM @DomainOption @cfg
  liftIO $ putStrLn "User type cookie set, now setting auth cookie"
  authCookieName <- getAuthCookieName <$> asksM @AuthCookieName @cfg
  liftIO $ putStrLn $ "Auth cookie name: " <> show authCookieName
  csk <- asksM @CS.Key @cfg
  liftIO $ putStrLn "Both cookies set, checking response headers before redirect"

  link_ <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy redirectToRoute

  pure $ do
    addUserTypeCookieHeader domainOpts userTypeCookieName userType'
    addAuthCookieHeader csk domainOpts authCookieName accountID'
    -- Debug: inspect response headers before redirect
    resp <- Snap.getResponse
    let headers_ = Snap.listHeaders resp
    liftIO $ putStrLn $ "Response headers before redirect: " <> show headers_
    frontendRedirect link_

-- TODO: they dont need to reset password if they have signed up with github
-- | TODO: not actually in use, should actually work no problem with github
oauthHandler
  :: forall api db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
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
  -> Eff es (Maybe (Id Account, T.Text), Snap ())
oauthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGithubOAuthClientID <$> asksM @GithubOAuthClientID @cfg
      clientSecret <- getGithubOAuthClientSecret <$> asksM @GithubOAuthClientSecret @cfg
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
          noAuthLink <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy redirectNoAuth
          pure (Nothing, frontendRedirect noAuthLink)
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
            Nothing -> error "this is likely a bug or you do not have a github account"
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
              maybeGitID <- withDbEnv @cfg $ getGithubUserIfTheyExist ghTbl userGH

              liftIO $ putStrLn ghEmail
              let ghDisplayName = T.pack $ fromMaybe (_githubUser_login userGH) (_githubUser_name userGH)
              (acctId, snapAction) <- handleOAuthLogin @api @db @cfg
                proxy
                ghEmail
                maybeGitID
                (\(GithubID _ghid uid) -> uid)
                (\aid -> withDbEnv @cfg $ insertNewGithubID ghTbl userGH aid)
                redirectToRoute
              pure (Just (acctId, ghDisplayName), snapAction)

-- | Google OAuth handler
googleOAuthHandler
  :: forall api db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
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
  -> Eff es (Maybe (Id Account, T.Text), Snap ())
googleOAuthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getGoogleOAuthClientID <$> asksM @GoogleOAuthClientID @cfg
      clientSecret <- getGoogleOAuthClientSecret <$> asksM @GoogleOAuthClientSecret @cfg
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
          noAuthLink <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy redirectNoAuth
          pure (Nothing, frontendRedirect noAuthLink)
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://www.googleapis.com/oauth2/v2/userinfo"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userGoogle :: Maybe GoogleUser = Aeson.decode . Http.responseBody $ res
          case userGoogle of
            Nothing -> error "Failed to get Google user info"
            Just userG -> do
              (googleTbl :: PgTable Postgres db GoogleID) <- asksTableM
              maybeGoogleID <- withDbEnv @cfg $ getGoogleUserIfTheyExist googleTbl userG

              let googleEmail = _googleUser_email userG
                  googleDisplayName = T.pack $ fromMaybe (takeWhile (/= '@') googleEmail) (_googleUser_name userG)
              (acctId, snapAction) <- handleOAuthLogin @api @db @cfg
                proxy
                googleEmail
                maybeGoogleID
                (\(GoogleID _gid uid) -> uid)
                (\aid -> withDbEnv @cfg $ insertNewGoogleID googleTbl userG aid)
                redirectToRoute
              pure (Just (acctId, googleDisplayName), snapAction)

-- | Discord OAuth handler
discordOAuthHandler
  :: forall api db cfg es frontendRoute.
     ( IOE :> es
     , Database Postgres db
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
  -> Eff es (Maybe (Id Account, T.Text), Snap ())
discordOAuthHandler proxy oauthRoute redirectUri redirectToRoute redirectNoAuth = case oauthRoute of
  OAuth_RedirectUri :/ redirectParams -> case redirectParams of
    Nothing -> liftIO $ error "Expected to receive the authorization code here"
    Just (RedirectUriParams code _mstate) -> do
      clientId <- getDiscordOAuthClientID <$> asksM @DiscordOAuthClientID @cfg
      clientSecret <- getDiscordOAuthClientSecret <$> asksM @DiscordOAuthClientSecret @cfg
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
          noAuthLink <- renderFullRouteFE @api @(R frontendRoute) @cfg @es proxy redirectNoAuth
          pure (Nothing, frontendRedirect noAuthLink)
        Just aToken -> do
          reqUser <- liftIO $ Http.parseRequest "https://discord.com/api/users/@me"
          let reqUser' = reqUser { Http.requestHeaders = Http.requestHeaders reqUser <>
                                   [ (Http.hAuthorization, "Bearer " <> (T.encodeUtf8 aToken))
                                   ]
                                 }
          res <- liftIO $ flip Http.httpLbs tlsMgr reqUser'
          let userDiscord :: Maybe DiscordUser = Aeson.decode . Http.responseBody $ res
          case userDiscord of
            Nothing -> error "Failed to get Discord user info"
            Just userD -> do
              (discordTbl :: PgTable Postgres db DiscordID) <- asksTableM
              maybeDiscordID <- withDbEnv @cfg $ getDiscordUserIfTheyExist discordTbl userD

              let discordEmail = fromMaybe (_discordUser_username userD) $ _discordUser_email userD
                  discordDisplayName = T.pack $ _discordUser_username userD
              (acctId, snapAction) <- handleOAuthLogin @api @db @cfg
                proxy
                discordEmail
                maybeDiscordID
                (\(DiscordID _did uid) -> uid)
                (\aid -> withDbEnv @cfg $ insertNewDiscordID discordTbl userD aid)
                redirectToRoute
              pure (Just (acctId, discordDisplayName), snapAction)
