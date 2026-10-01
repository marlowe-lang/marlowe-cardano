{-# LANGUAGE FlexibleInstances #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams where

import Cardano.Api (SystemStart (..))
import qualified Cardano.Api as C
import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Core as L
import qualified Data.ByteString as BS
import Data.Binary (Put, Get)
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Word (Word8)

-- | Tag byte for the 'NetworkId' binary encoding.
networkIdTagMainnet :: Word8
networkIdTagMainnet = 0

-- | Tag byte for a testnet 'NetworkId'.
networkIdTagTestnet :: Word8
networkIdTagTestnet = 1

putNetworkId :: C.NetworkId -> Put
putNetworkId = \case
  C.Mainnet -> Binary.putWord8 networkIdTagMainnet
  C.Testnet (C.NetworkMagic m) -> do
    Binary.putWord8 networkIdTagTestnet
    Binary.putWord32be m

getNetworkId :: Get C.NetworkId
getNetworkId = do
  tag <- Binary.getWord8
  case tag of
    _ | tag == networkIdTagMainnet -> pure C.Mainnet
      | tag == networkIdTagTestnet -> C.Testnet . C.NetworkMagic <$> Binary.getWord32be
      | otherwise -> fail $ "NetworkParams.getNetworkId: unknown tag " <> show tag

-- | 'SystemStart' is encoded as a CBOR value via the underlying
-- 'Cardano.Slotting.Time.SystemStart' 'ToCBOR'/'FromCBOR' instances.
putSystemStart :: C.SystemStart -> Put
putSystemStart = Binary.putByteString . CBOR.serialize' . systemStartToUTCTime
  where
    systemStartToUTCTime (SystemStart ss) = ss

-- | Decode a 'SystemStart' from a strict bytestring using CBOR.
decodeSystemStart :: BS.ByteString -> Either CBOR.DecoderError C.SystemStart
decodeSystemStart = CBOR.decodeFull'

-- | 'PParams' for the Conway era is encoded as a CBOR value via the
-- ledger 'ToCBOR'/'FromCBOR' instances.
putProtocolParameters :: L.PParams (C.ShelleyLedgerEra C.ConwayEra) -> Put
putProtocolParameters = Binary.putByteString . CBOR.serialize'

-- | Decode Conway-era 'PParams' from a strict bytestring using CBOR.
decodeProtocolParameters :: BS.ByteString -> Either CBOR.DecoderError (L.PParams (C.ShelleyLedgerEra C.ConwayEra))
decodeProtocolParameters = CBOR.decodeFull'
