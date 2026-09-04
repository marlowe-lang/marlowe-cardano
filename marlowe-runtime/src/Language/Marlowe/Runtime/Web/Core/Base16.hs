module Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..), fromText) where

import Control.DeepSeq (NFData)
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSON (toJSON),
  ToJSONKey (toJSONKey),
  Value (String),
  withText,
 )
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16Untyped, encodeBase16)
import Data.OpenApi (
  NamedSchema (..),
  ToSchema,
  declareSchema,
 )
import Data.OpenApi.Schema (ToSchema (..))
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseUrlPiece),
  Proxy (..),
  ToHttpApiData (toUrlPiece),
 )
import Servant.API.Generic (Generic)
import Data.Base16.Types (extractBase16)
import Control.Error (hush)

-- | A newtype for Base16 decoding and encoding ByteStrings
newtype Base16 = Base16 {unBase16 :: ByteString}
  deriving (Eq, Ord, Generic)

fromText :: T.Text -> Maybe Base16
fromText = fmap Base16 . hush . decodeBase16Untyped . encodeUtf8

instance NFData Base16

instance Show Base16 where
  show = T.unpack . extractBase16 . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16Untyped . encodeUtf8 . T.pack

instance ToJSON Base16 where
  toJSON = String . toUrlPiece

instance ToJSONKey Base16 where
  toJSONKey = toJSONKeyText toUrlPiece

instance FromJSON Base16 where
  parseJSON =
    withText "Base16" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToHttpApiData Base16 where
  toUrlPiece = extractBase16 . encodeBase16 . unBase16

instance FromHttpApiData Base16 where
  parseUrlPiece = fmap Base16 . decodeBase16Untyped . encodeUtf8

instance ToSchema Base16 where
  declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy @String)
