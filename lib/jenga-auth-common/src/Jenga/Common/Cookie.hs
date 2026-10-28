module Jenga.Common.Cookie where

import Jenga.Common.BeamExtras
import Data.String
import Data.Signed
import Rhyolite.Account
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

clientTypeHeader :: IsString s => s
clientTypeHeader = "X-ClientType"

-- | Encode an account token using the authentication-cookie wire format.
keylessTo :: Signed (Id Account) -> BS.ByteString
keylessTo = keylessTo'

-- | Assert phantom type
keylessFrom :: BS.ByteString -> Maybe (Signed (Id Account))
keylessFrom = keylessFrom'

keylessTo' :: Signed a -> BS.ByteString
keylessTo' signedAcctID =
  B64.encode .
  T.encodeUtf8 $
  unSigned signedAcctID

keylessFrom' :: BS.ByteString -> Maybe (Signed a)
keylessFrom' raw = do
  decoded <- either (const Nothing) Just $ B64.decode raw
  Signed <$> either (const Nothing) Just (T.decodeUtf8' decoded)

-- | Signed is a phantom type. We dont actually
-- | touch its inner text
signedToJSON :: Signed a -> LBS.ByteString
signedToJSON = A.encode

-- | Signed is a phantom type. We dont actually
-- | touch its inner text
signedFromJSON :: LBS.ByteString -> Maybe (Signed a)
signedFromJSON = A.decode
