{-|
Description:
  Typed errors for the OAuth login handlers.

  Introduced so callers can pattern-match on OAuth failure modes instead of
  the previous behaviour of calling 'error' and taking the whole Snap thread
  down on missing config / undecodable provider responses / invalid emails.
  See 'Jenga.Backend.Handlers.Auth.OAuth' for the sites that now return
  these instead.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Jenga.Backend.Handlers.Auth.OAuth.Error
  ( OAuthError (..)
  , handleOAuthError
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Failure modes an OAuth handler can surface to its caller.
--
-- Each constructor documents WHEN it fires. Callers may render, log, or
-- redirect based on the constructor; the library refuses to decide for them.
data OAuthError
  = -- | A required config field was missing at the point we needed it
    -- (client id, client secret, cookie name, etc). Payload is a
    -- human-readable label for the missing item.
    OAuthConfigMissing Text
  | -- | The provider's JSON response could not be decoded into the shape we
    -- expected (missing user info, missing access token, malformed body).
    -- Payload names the endpoint / field that failed.
    OAuthDecodeError Text
  | -- | Lookup for an OAuth-linked account row failed unexpectedly.
    OAuthAccountNotFound Text
  | -- | Catch-all for provider-side errors that don't fit the categories
    -- above (e.g. missing authorization code on the redirect callback,
    -- provider returned an email we couldn't validate).
    OAuthProviderError Text
  deriving (Show, Eq, Generic)

-- | Convenience wrapper for the common "log-or-redirect" caller pattern.
-- The library never assumes which of those the caller wants, so we just
-- offer the boring shape as a helper instead of forcing every call site to
-- rewrite the same 'either'.
handleOAuthError
  :: Applicative m
  => (OAuthError -> m a)
  -> (b -> m a)
  -> Either OAuthError b
  -> m a
handleOAuthError onErr onOk = either onErr onOk
