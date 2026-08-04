module Jenga.Common.HasJengaConfig
  ( -- * Effectful config access (replaces HasConfig + ReaderT)
    askConfig

    -- * Config newtypes
  , BaseURL(..)
  , DomainOption(..)
  , AuthCookieName(..)
  , UserTypeCookieName(..)
  , StripePlan(..)
  , CompanySignupCode(..)
  , SubscribeHash(..)
  , GithubOAuthClientID(..)
  , GithubOAuthClientSecret(..)
  , GoogleOAuthClientID(..)
  , GoogleOAuthClientSecret(..)
  , DiscordOAuthClientID(..)
  , DiscordOAuthClientSecret(..)
  , FreeTrialInfo(..)
  , StripeCode(..)
  , Plans(..)

    -- * Config utilities
  , getJsonConfigBase
  , getJsonConfig

    -- * Route rendering
  , renderFullRouteBE
  , renderFullRouteFE
  , Link(..)

    -- * Env checks
  , isLocalHostEnv
  , lookupSubscriptionCodeEnv
  , matchesCompanyCodeEnv

    -- * Legacy compatibility
  , HasConfig(..)
  , asksM
  , MkRoute

    -- * Re-exports
  , Reader
  )
where

import Data.Time.Clock
import qualified Control.Monad.Fail as Fail
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Network.URI
import Control.Applicative
import GHC.Generics

import Effectful (Eff, (:>))
import Effectful.Reader.Static (Reader, ask)

import Jenga.Route (HasRoute(..), renderRoute)
import Data.Proxy (Proxy)

-- | Constraint synonym for handlers/renderers that need base URL access
-- when constructing links back to the application. The @br@ / @fr@ type
-- parameters are phantom in the servant-based routing world (jenga-route
-- doesn't expose an Encoder type here) but are retained for signature
-- compatibility with pre-servant consumers.
type MkRoute cfg br fr = HasConfig cfg BaseURL

-- ─── Effectful config access ─────────────────────────────────

-- ─── HasConfig class ─────────────────────────────────────────

-- | Extract a capability from a config record.
--
-- Used with @Reader cfg :> es@ to get typed config values:
--
-- @
-- handler :: (Reader cfg :> es, HasConfig cfg BaseURL, HasConfig cfg (Pool Connection))
--         => Eff es ()
-- handler = do
--   BaseURL url <- asksM
--   pool <- asksM
-- @
class HasConfig a b where
  fromCfg :: a -> b

-- | Ask for a config value by extracting it from the Reader environment.
--
-- Drop-in replacement for the old @ReaderT cfg m@ version.
asksM :: forall x cfg es. (Reader cfg :> es, HasConfig cfg x) => Eff es x
asksM = fromCfg @cfg @x <$> ask @cfg

-- | Synonym for 'asksM'.
askConfig :: forall x cfg es. (Reader cfg :> es, HasConfig cfg x) => Eff es x
askConfig = asksM @x @cfg

-- ─── Config newtypes ─────────────────────────────────────────

newtype BaseURL = BaseURL { getBaseURL :: URI }
newtype AuthCookieName = AuthCookieName { getAuthCookieName :: T.Text }
newtype UserTypeCookieName = UserTypeCookieName { getUserTypeCookieName :: T.Text }
newtype StripePlan = StripePlan { getStripePlan :: T.Text }
newtype CompanySignupCode = CompanySignupCode { getCompanySignupCode :: T.Text }
newtype SubscribeHash = SubscribeHash { getSubscribeHash :: T.Text } deriving (Eq, Ord)
newtype GithubOAuthClientID = GithubOAuthClientID { getGithubOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype GithubOAuthClientSecret = GithubOAuthClientSecret { getGithubOAuthClientSecret :: T.Text } deriving (Eq, Ord)
newtype GoogleOAuthClientID = GoogleOAuthClientID { getGoogleOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype GoogleOAuthClientSecret = GoogleOAuthClientSecret { getGoogleOAuthClientSecret :: T.Text } deriving (Eq, Ord)
newtype DiscordOAuthClientID = DiscordOAuthClientID { getDiscordOAuthClientID :: T.Text } deriving (Eq, Ord)
newtype DiscordOAuthClientSecret = DiscordOAuthClientSecret { getDiscordOAuthClientSecret :: T.Text } deriving (Eq, Ord)
data FreeTrialInfo = FreeTrialInfo
  { getFreeTrialCode :: T.Text
  , getFreeTrialLength :: NominalDiffTime
  } deriving (Eq, Ord)

newtype StripeCode = StripeCode T.Text

data Plans = Plans
  { defaultPlan :: Maybe StripePlan
  , getPlans :: Map.Map SubscribeHash StripePlan
  }

-- ─── JSON config parsing ─────────────────────────────────────

getJsonConfigBase :: FromJSON a => T.Text -> Map.Map T.Text BS.ByteString -> (Maybe (Either String a))
getJsonConfigBase key cfgs = fmap eitherDecodeStrict' $ cfgs Map.!? key

getJsonConfig :: (FromJSON a, Fail.MonadFail m) => T.Text -> Map.Map T.Text BS.ByteString -> m a
getJsonConfig k cfgs = case getJsonConfigBase k cfgs of
  Nothing -> Fail.fail $ "getJsonConfig missing key: " <> T.unpack k
  Just (Left err) -> Fail.fail $ "getJsonConfig invalid for key " <> T.unpack k <> " : " <> err
  Just (Right val) -> pure val

-- ─── Route rendering (using servant routing) ─────────────────

-- | Render a backend route as a full URL.
renderFullRouteBE
  :: forall api r cfg es.
     ( HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     )
  => Proxy api
  -> r
  -> Eff es Link
renderFullRouteBE proxy route = do
  BaseURL baseUrl <- asksM @BaseURL @cfg
  pure . Link $ (T.pack $ show baseUrl) <> renderRoute proxy route

-- | Render a frontend route as a full URL.
renderFullRouteFE
  :: forall api r cfg es.
     ( HasRoute api r
     , Reader cfg :> es, HasConfig cfg BaseURL
     )
  => Proxy api
  -> r
  -> Eff es Link
renderFullRouteFE proxy route = renderFullRouteBE @api @r @cfg proxy route

-- | Strong witness to the contained text being a valid link.
newtype Link = Link { getLink :: T.Text } deriving Generic
instance ToJSON Link
instance FromJSON Link

-- ─── Environment checks ──────────────────────────────────────

isLocalHostEnv
  :: forall cfg es. (Reader cfg :> es, HasConfig cfg BaseURL)
  => Eff es Bool
isLocalHostEnv = T.isPrefixOf "http://localhost:" . T.pack . show . getBaseURL <$> asksM @BaseURL @cfg

lookupSubscriptionCodeEnv
  :: forall cfg es. (Reader cfg :> es, HasConfig cfg Plans)
  => Maybe T.Text
  -> Eff es (Maybe StripePlan)
lookupSubscriptionCodeEnv codeAsked = do
  plans <- asksM @Plans @cfg
  pure $ (codeAsked >>= flip Map.lookup (getPlans plans) . SubscribeHash) <|> defaultPlan plans

matchesCompanyCodeEnv
  :: forall cfg es. (Reader cfg :> es, HasConfig cfg CompanySignupCode)
  => T.Text
  -> Eff es Bool
matchesCompanyCodeEnv c = (==c) . getCompanySignupCode <$> asksM @CompanySignupCode @cfg

data DomainOption = ProxiedDomain T.Text T.Text | DirectDomain T.Text
