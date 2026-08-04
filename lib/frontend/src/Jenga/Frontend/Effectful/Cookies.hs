-- | Jenga's browser-cookie effect (previously
-- 'Reflex.Effectful.Effect.Jenga.Cookies'). Provides read access to the
-- cookies attached to the current request/session.
module Jenga.Frontend.Effectful.Cookies
  ( CookiesEff(..)
  , askCookies
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
