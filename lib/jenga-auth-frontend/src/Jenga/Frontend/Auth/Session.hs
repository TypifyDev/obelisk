{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Effectful helpers for reading the authenticated-session cookie on the
-- frontend.
--
-- Companion of 'Jenga.Frontend.Cookie' (MTL) — same job (surface the auth
-- token that the backend deposited into the browser via 'addAuthCookieHeader')
-- but expressed via the 'CookiesEff' effect used by every other jenga
-- effectful frontend widget.
--
-- The authoritative session identity is 'AuthToken' — a
-- @'Signed' ('Rhyolite.Account.Id' 'Rhyolite.Account.Account')@ — and the
-- cookie is named according to the runtime config value 'AuthCookieName'
-- (deliberately not hard-coded here; different jenga-based apps pick
-- different names, e.g. @"ace_ip"@ vs @"cookie_ip"@ — see
-- @thunks/common-constants/src/Common/Constants.hs@ for the typify
-- project's choice).
--
-- Two shapes are exposed:
--
-- * 'getSessionTokenE' — one-shot 'Eff' read of the cookie map. Suitable
--   for postBuild-scoped decisions (\"do I already have a session at page
--   load\").
--
-- * 'sessionCookieE' — same read wrapped in a 'Dynamic' so consumers can
--   react to session-token changes without re-plumbing through
--   'Reflex.Dynamic'. The 'Dynamic' is presently constant because
--   'CookiesEff' only exposes the snapshot at widget-construction time;
--   consumers that need updates on login\/logout should merge them via
--   their own login \/ logout Events (this matches how the MTL side does
--   it via 'manageAuthCookies' — the read and the writes are decoupled by
--   design).
--
-- Both bubble decode failures as 'Nothing' rather than an explicit
-- 'Either', matching how the corresponding MTL code in
-- @typify-ws\/frontend\/src\/Frontend.hs@ treats a malformed cookie:
-- absence of the entry and presence of an undecodable entry are both
-- \"no session\" from the widget's point of view. Callers that need to
-- distinguish \"missing\" from \"present-but-corrupt\" can call
-- 'lookupSessionCookieRaw' + 'keylessFrom' themselves.
module Jenga.Frontend.Auth.Session
  ( -- * One-shot reads
    getSessionTokenE
  , lookupSessionCookieRaw

    -- * Dynamic wrappers
  , sessionCookieE
  ) where

import qualified Data.ByteString          as BS
import qualified Data.List                as L
import qualified Data.Text.Encoding       as T

import           Effectful                (Eff, (:>))

import qualified Reflex.Class   as R
import           Reflex.Class   (Dynamic, Reflex)
import           Reflex.Class (holdDyn)
import Reflex.Host.HeadlessEffectful (Hold)
import Reflex.Host.HeadlessEffectful (Sample)

import           Jenga.Common.Auth        (AuthToken)
import           Jenga.Common.Cookie      (keylessFrom)
import           Jenga.Common.HasJengaConfig
                    (AuthCookieName (..), HasConfig (..))
import qualified Effectful.Reader.Static as ER
import           Effectful.Reader.Static (Reader)
import           Jenga.Frontend.Effectful.Cookies (CookiesEff, askCookies)

-- | Look up the raw bytes of the auth cookie (whatever value the browser
-- currently has) without decoding. Returns 'Nothing' if the cookie is
-- absent. Useful when the caller wants full control over how a
-- present-but-corrupt cookie is treated (log, retry, force-logout, …).
lookupSessionCookieRaw
  :: forall cfg es.
     ( CookiesEff       :> es
     , Reader cfg       :> es
     , HasConfig cfg AuthCookieName
     )
  => Eff es (Maybe BS.ByteString)
lookupSessionCookieRaw = do
  AuthCookieName name <- ER.asks (fromCfg :: cfg -> AuthCookieName)
  cookies <- askCookies
  pure $ L.lookup (T.encodeUtf8 name) cookies

-- | Read the current session token from the browser's cookies. 'Nothing'
-- when the cookie is missing OR when the cookie value fails to decode as
-- a 'Signed (Id Account)' — the widget-level view treats both as
-- \"unauthenticated\".
--
-- Mirrors this MTL snippet from typify-ws @Frontend.hs@:
--
-- > cookies <- askCookies
-- > let authCookieEntry = L.lookup (T.encodeUtf8 authCookieName) cookies
-- >     baseAuth        = keylessFrom =<< authCookieEntry
getSessionTokenE
  :: forall cfg es.
     ( CookiesEff       :> es
     , Reader cfg       :> es
     , HasConfig cfg AuthCookieName
     )
  => Eff es (Maybe AuthToken)
getSessionTokenE = do
  mBytes <- lookupSessionCookieRaw @cfg
  pure $ keylessFrom =<< mBytes

-- | 'Dynamic' wrapper around 'getSessionTokenE'.
--
-- Present because most consumers want a 'Dynamic t (Maybe AuthToken)' to
-- feed into request-issuing pipelines rather than a bare snapshot. The
-- 'Dynamic' is currently constant at the value observed at widget build:
-- 'CookiesEff' returns the cookie snapshot captured when the runtime
-- interpreter was created, and the browser cannot push cookie mutations
-- back through it after the fact.
--
-- Consumers that need live updates on login \/ logout should compose:
--
-- > initial <- getSessionTokenE
-- > sessionD <- holdDyn initial (leftmost [Just <$> loginE, Nothing <$ logoutE])
--
-- which is the same shape typify-ws uses on the MTL side (see
-- @manageAuthCookies@ in the typify frontend), just spelled with 'Eff'.
sessionCookieE
  :: forall t cfg es.
     ( CookiesEff       :> es
     , Reader cfg       :> es
     , HasConfig cfg AuthCookieName
     , Hold t           :> es
     , Sample t         :> es
     , Reflex t
     )
  => Eff es (Dynamic t (Maybe AuthToken))
sessionCookieE = do
  initial <- getSessionTokenE @cfg
  holdDyn initial (R.never :: R.Event t (Maybe AuthToken))
