{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitProtocolParameters where

import qualified Cardano.Api as C
import qualified Cardano.Ledger.Core as L
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (putProtocolParameters)

commitProtocolParameters :: L.PParams (C.ShelleyLedgerEra C.ConwayEra) -> H.Transaction ()
commitProtocolParameters pparams =
  H.statement
    (prepareParams pparams)
    [resultlessStatement|
    INSERT INTO marlowe.node_status (attr, value)
    VALUES ('protocolParameters', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

prepareParams :: L.PParams (C.ShelleyLedgerEra C.ConwayEra) -> BS.ByteString
prepareParams = toStrict . runPut . putProtocolParameters
