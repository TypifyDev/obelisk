{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful \"Sign in with X\" widget helpers for the three OAuth
-- providers jenga's backend currently knows how to accept
-- (Google, GitHub, Discord).
--
-- ==== The flow this widget participates in
--
-- All three providers use the OAuth 2.0 Authorization Code grant with a
-- full-page redirect (matching @deps\/jenga\/deps\/obelisk-oauth\/example\/frontend\/src\/Frontend.hs@):
--
--   1. Frontend renders an @\<a\ href=…\>Sign in with X\</a\>@ pointing at
--      the provider's authorization endpoint (built here via
--      'buildAuthorizeUri' + provider constants).
--   2. User clicks; the browser navigates away to the provider.
--   3. Provider redirects the browser back to the app's backend callback
--      endpoint (@BackendRoute_GoogleOAuth :\/ …@ etc.).
--   4. The backend's 'Jenga.Backend.Handlers.Auth.OAuth' handler
--      completes the token exchange, sets the auth cookie via
--      'addAuthCookieHeader', and redirects the browser back into the
--      app.
--   5. On the resulting page load, 'getSessionTokenE' in
--      "Jenga.Frontend.Auth.Session" sees the fresh cookie and the app
--      is authenticated.
--
-- Because step (2) is a full-page navigation, the widget itself CANNOT
-- observe the authentication result — the JS context it lives in is
-- destroyed before the token comes back. That is why these widgets
-- return @'Eff' es ('Event' t ())@ (a click event, useful for pre-nav
-- logging \/ pre-nav route stashing) rather than @'Eff' es ('Event' t
-- ('Either' err 'AuthToken'))@. The token surfaces through
-- 'Jenga.Frontend.Auth.Session.getSessionTokenE' after the round-trip,
-- not through the widget.
--
-- A JSaddle-popup variant (@window.open(…)@ + @postMessage@ callback
-- inside the same tab) would justify a signature that carries the
-- authenticated token back to the widget, but that infrastructure does
-- not exist in jenga today. Adding it would need a matching backend
-- handler that renders a post-message-and-close bridge page instead of
-- the current 'frontendRedirect'. When that lands, this module gains a
-- popup-variant widget; the anchor widgets here stay as-is because
-- direct redirect is still the correct choice on mobile (where popups
-- are unreliable) and in cases where the caller wants the browser
-- history to reflect the auth navigation.
--
-- ==== Why these three constructors
--
-- These are exactly the three providers wired up by the corresponding
-- backend handlers:
--
--   * 'Jenga.Backend.Handlers.Auth.OAuth.googleOAuthHandler'
--   * 'Jenga.Backend.Handlers.Auth.OAuth.discordOAuthHandler'
--   * The GitHub handler in the same module (invoked via the general
--     @BackendRoute_OAuth@ path).
--
-- Adding a fourth provider means adding a widget here AND a handler in
-- @jenga-auth-backend@; the two must agree on scope, redirect URI, and
-- authorization endpoint.
module Jenga.Frontend.Auth.OAuth
  ( -- * Widgets
    googleOAuthE
  , githubOAuthE
  , discordOAuthE

    -- * Configuration
  , OAuthWidgetConfig (..)
  , defaultGoogleScopes
  , defaultGithubScopes
  , defaultDiscordScopes

    -- * URL construction (exposed for testing \/ custom widgets)
  , buildAuthorizeUri
  , googleAuthorizeEndpoint
  , githubAuthorizeEndpoint
  , discordAuthorizeEndpoint
  ) where
import Reflex.Host.HeadlessEffectful (NotReady)
import Reflex.Host.HeadlessEffectful (Adjustable)

import           Control.Lens             ((&), (.~))
import           Data.Default             (def)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Effectful                (Eff, (:>))

import           Reflex.Class   (Event, Reflex)
import           Reflex.Dom.Core
                    ( AttributeName (..)
                    , EventName (Click)
                    , EventResult
                    , ElementConfig
                    , domEvent
                    , elementConfig_initialAttributes
                    )
import           Reflex.Dom.Core (DomSpace, element)
import Reflex.Effectful.DomHost (DomBuilderSpaceE)
import Reflex.Effectful.DomHost (DomBuilderE)
import Reflex.Effectful.DomHost (DomBuilderTimelineE)
import           Reflex.Dom.Core (text)

import           Jenga.Common.HasJengaConfig
                    ( DiscordOAuthClientID (..)
                    , GithubOAuthClientID (..)
                    , GoogleOAuthClientID (..)
                    , HasConfig
                    )
import qualified Effectful.Reader.Static as ER
import Effectful.Reader.Static (Reader)
import Jenga.Common.HasJengaConfig (fromCfg)

-- ─── Widget config ──────────────────────────────────────────────────

-- | Per-render OAuth widget config. Kept as a record so callers can
-- override individual fields without listing every argument at each call
-- site.
--
-- The 'oauthWidgetLabel' field defaults to \"Sign in with X\" text
-- rendered inside the anchor; supply a different label to swap in an
-- SVG icon or localized string. The 'oauthWidgetAttrs' let a call site
-- attach ClasshSS-driven styling; the widget deliberately writes zero
-- default classes so the app owns the look.
--
-- 'oauthWidgetState' is passed through as the @state@ query parameter
-- (RFC 6749 §10.12) — set it to a random nonce and validate it on the
-- callback if you want CSRF protection. The default is 'Nothing'
-- because the value is meaningful only when the backend actually checks
-- it, and jenga's current handlers do not.
data OAuthWidgetConfig = OAuthWidgetConfig
  { oauthWidgetLabel :: Text
    -- ^ Text rendered inside the anchor.
  , oauthWidgetAttrs :: Map Text Text
    -- ^ Extra HTML attributes (class, id, aria-*, …) merged with the
    -- @href@ that the widget itself controls.
  , oauthWidgetScopes :: [Text]
    -- ^ OAuth scopes to request. Defaults are per-provider — see
    -- 'defaultGoogleScopes' \/ 'defaultGithubScopes' \/
    -- 'defaultDiscordScopes'.
  , oauthWidgetState :: Maybe Text
    -- ^ Optional @state@ nonce for CSRF protection.
  , oauthWidgetRedirectUri :: Text
    -- ^ Fully-rendered URL the OAuth provider should redirect the
    -- browser to after granting. MUST match a URI registered with the
    -- provider AND agree with the backend's callback route (see
    -- @BackendRoute_GoogleOAuth@ etc. in the app's common routes).
    -- Callers construct this from their app's base URL — the widget
    -- doesn't attempt to guess.
  }

-- | Canonical minimum-viable Google scopes: email + profile so the
-- backend has enough to populate the account row.
defaultGoogleScopes :: [Text]
defaultGoogleScopes =
  [ "https://www.googleapis.com/auth/userinfo.email"
  , "https://www.googleapis.com/auth/userinfo.profile"
  ]

-- | Canonical minimum-viable GitHub scopes. @user:email@ is required
-- because the /user endpoint won't reveal email without it.
defaultGithubScopes :: [Text]
defaultGithubScopes = [ "read:user", "user:email" ]

-- | Canonical minimum-viable Discord scopes.
defaultDiscordScopes :: [Text]
defaultDiscordScopes = [ "identify", "email" ]

-- ─── Endpoint constants ────────────────────────────────────────────

-- | Google OAuth 2.0 authorization endpoint. Constant per RFC and
-- Google's documented flow.
googleAuthorizeEndpoint :: Text
googleAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"

-- | GitHub OAuth authorization endpoint.
githubAuthorizeEndpoint :: Text
githubAuthorizeEndpoint = "https://github.com/login/oauth/authorize"

-- | Discord OAuth 2.0 authorization endpoint.
discordAuthorizeEndpoint :: Text
discordAuthorizeEndpoint = "https://discord.com/api/oauth2/authorize"

-- ─── URL construction ──────────────────────────────────────────────

-- | Build the full \"begin authorization\" URL that the anchor points at.
--
-- The shape is @endpoint?client_id=…&redirect_uri=…&response_type=code&scope=…[&state=…]@
-- which is what all three currently-supported providers accept for the
-- authorization-code flow. Values are URL-encoded via 'urlEncodeParam';
-- separators are literal.
--
-- Exposed at module top level so custom widgets (e.g. a shared \"pick
-- your provider\" grid) can build the URL themselves without going
-- through the anchor renderer.
buildAuthorizeUri
  :: Text        -- ^ Authorization endpoint (see the three constants above)
  -> Text        -- ^ Client ID
  -> Text        -- ^ Redirect URI (must match what's registered)
  -> [Text]      -- ^ Scopes; joined with @' '@ per RFC 6749 §3.3
  -> Maybe Text  -- ^ Optional @state@ nonce
  -> Text
buildAuthorizeUri endpoint clientId redirectUri scopes mState =
  endpoint <> "?" <> T.intercalate "&" params
  where
    params = mconcat
      [ [ "client_id=" <> urlEncodeParam clientId
        , "redirect_uri=" <> urlEncodeParam redirectUri
        , "response_type=code"
        ]
      , case scopes of
          [] -> []
          _  -> [ "scope=" <> urlEncodeParam (T.intercalate " " scopes) ]
      , case mState of
          Nothing -> []
          Just s  -> [ "state=" <> urlEncodeParam s ]
      ]

-- | Percent-encode a query parameter value. Intentionally minimal — we
-- only need to cover the characters that show up in OAuth params
-- (scopes contain @:@ and @\/@ and spaces; state nonces are usually
-- URL-safe base64). Anything not in the RFC 3986 unreserved set gets
-- percent-encoded.
urlEncodeParam :: Text -> Text
urlEncodeParam = T.concatMap encodeChar
  where
    encodeChar c
      | isUnreserved c = T.singleton c
      | c == ' '       = "%20"
      | otherwise      = T.pack (percent c)
    isUnreserved c =
      (c >= 'A' && c <= 'Z') ||
      (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') ||
      c == '-' || c == '_' || c == '.' || c == '~'
    percent c =
      let n = fromEnum c
      in ['%', hex (n `div` 16), hex (n `mod` 16)]
    hex n
      | n < 10    = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)

-- ─── Widgets ───────────────────────────────────────────────────────

-- | Google \"Sign in with Google\" anchor.
--
-- Renders @\<a\ href=…\>label\</a\>@. Click fires the returned 'Event t
-- ()' AND lets the browser navigate — no 'preventDefault' is attached,
-- on purpose: the whole point is to hand control to Google. The event
-- is surfaced so consumers can log \/ record pre-nav state (e.g. stash
-- the current route so post-auth landing can return the user to it).
googleOAuthE
  :: forall cfg t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , DomSpace (DomBuilderSpaceE es)
     , t ~ DomBuilderTimelineE es
     , Reader cfg    :> es
     , HasConfig cfg GoogleOAuthClientID
     
     , Reflex t
     )
  => OAuthWidgetConfig
  -> Eff es (Event t ())
googleOAuthE widgetCfg = do
  GoogleOAuthClientID clientId <- ER.asks (fromCfg :: cfg -> GoogleOAuthClientID)
  oauthAnchor
    googleAuthorizeEndpoint
    clientId
    widgetCfg

-- | GitHub \"Sign in with GitHub\" anchor. Structurally identical to
-- 'googleOAuthE'; the only difference is which client-id newtype it
-- reads from the config.
githubOAuthE
  :: forall cfg t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , DomSpace (DomBuilderSpaceE es)
     , t ~ DomBuilderTimelineE es
     , Reader cfg    :> es
     , HasConfig cfg GithubOAuthClientID
     
     , Reflex t
     )
  => OAuthWidgetConfig
  -> Eff es (Event t ())
githubOAuthE widgetCfg = do
  GithubOAuthClientID clientId <- ER.asks (fromCfg :: cfg -> GithubOAuthClientID)
  oauthAnchor
    githubAuthorizeEndpoint
    clientId
    widgetCfg

-- | Discord \"Sign in with Discord\" anchor. Same shape as the other
-- two.
discordOAuthE
  :: forall cfg t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , DomSpace (DomBuilderSpaceE es)
     , t ~ DomBuilderTimelineE es
     , Reader cfg    :> es
     , HasConfig cfg DiscordOAuthClientID
     
     , Reflex t
     )
  => OAuthWidgetConfig
  -> Eff es (Event t ())
discordOAuthE widgetCfg = do
  DiscordOAuthClientID clientId <- ER.asks (fromCfg :: cfg -> DiscordOAuthClientID)
  oauthAnchor
    discordAuthorizeEndpoint
    clientId
    widgetCfg

-- ─── Internal ──────────────────────────────────────────────────────

-- | Shared anchor-rendering machinery. Not exported: the three widget
-- functions above are the public surface and this internal helper
-- exists purely to deduplicate the anchor-rendering boilerplate.
--
-- Deliberately does NOT attach any 'elementConfig_eventSpec' modifier:
-- 'domEvent' 'Click' fires by default on any 'Element' produced by
-- 'element', and we do NOT want 'preventDefault' — the browser
-- following the @href@ IS the desired behavior for OAuth redirect.
oauthAnchor
  :: forall t es.
     ( DomBuilderE t (DomBuilderSpaceE es) :> es, NotReady t :> es, Adjustable t :> es
     , DomSpace (DomBuilderSpaceE es)
     , t ~ DomBuilderTimelineE es
     
     , Reflex t
     )
  => Text              -- ^ Authorization endpoint
  -> Text              -- ^ Client ID
  -> OAuthWidgetConfig
  -> Eff es (Event t ())
oauthAnchor endpoint clientId OAuthWidgetConfig{..} = do
  let href = buildAuthorizeUri
               endpoint
               clientId
               oauthWidgetRedirectUri
               oauthWidgetScopes
               oauthWidgetState
      attrs = Map.insert "href" href oauthWidgetAttrs
      elCfg :: ElementConfig EventResult t (DomBuilderSpaceE es)
      elCfg = def
        & elementConfig_initialAttributes
            .~ Map.mapKeys (AttributeName Nothing) attrs
  (e, _) <- element "a" elCfg (text oauthWidgetLabel)
  pure (domEvent Click e)
