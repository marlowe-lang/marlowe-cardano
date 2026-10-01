{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitNetworkId where

import qualified Cardano.Api as C
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (putNetworkId)

commitNetworkId :: C.NetworkId -> H.Transaction ()
commitNetworkId networkId =
  H.statement
    (prepareParams networkId)
    [resultlessStatement|
    INSERT INTO marlowe.node_status (attr, value)
    VALUES ('networkId', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

prepareParams :: C.NetworkId -> BS.ByteString
prepareParams = toStrict . runPut . putNetworkId
