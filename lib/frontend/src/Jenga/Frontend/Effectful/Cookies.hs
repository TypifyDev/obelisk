{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

-- | Jenga's browser-cookie effect (previously
-- 'Reflex.Class'). Provides read access to the
-- cookies attached to the current request/session.
module Jenga.Frontend.Effectful.Cookies
  ( CookiesEff(..)
  , Cookies
  , askCookies
  , runCookies
  ) where

import           Data.ByteString          (ByteString)
import qualified Effectful                as Eff
import           Effectful                ((:>))
import qualified Effectful.Dispatch.Dynamic as Eff

-- | Cookie type (list of name-value pairs).
-- Re-defined here to avoid depending on cookie package directly.
type Cookies = [(ByteString, ByteString)]

data CookiesEff :: Eff.Effect where
  AskCookies :: CookiesEff m Cookies

type instance Eff.DispatchOf CookiesEff = 'Eff.Dynamic

askCookies :: CookiesEff :> es => Eff.Eff es Cookies
askCookies = Eff.send AskCookies

-- | Run the 'CookiesEff' effect with a pre-loaded cookie snapshot.
--
-- The 'Cookies' list is captured by the interpreter closure, so every
-- 'askCookies' call inside the inner action returns this same snapshot.
-- Mirrors 'runConfigs' in 'Jenga.Frontend.Effectful.Configs': the
-- one-shot cookie read happens once at boot (typically via
-- 'GHCJS.DOM.Document.getCookie' + 'Web.Cookie.parseCookies' on the
-- client) and every downstream widget sees the same value.
--
-- 'Jenga.Frontend.runFrontend' (@jenga-route-servant@) installs this
-- interpreter itself: it performs the one-shot @getCookie =<<
-- askDocument@ read inside the widget stack and wraps the body with
-- 'runCookies', so every downstream widget can 'askCookies' without
-- having to bridge the JSM boundary. Call this directly only in
-- alternative entrypoints (SSR, tests) that don't go through
-- 'runFrontend'.
runCookies
  :: Cookies
  -> Eff.Eff (CookiesEff : es) a
  -> Eff.Eff              es   a
runCookies cookies = Eff.interpret $ \_ -> \case
  AskCookies -> pure cookies
