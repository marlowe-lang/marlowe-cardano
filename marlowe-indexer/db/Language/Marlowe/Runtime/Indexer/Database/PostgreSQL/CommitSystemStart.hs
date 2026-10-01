{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitSystemStart where

import qualified Cardano.Api as C
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (putSystemStart)

commitSystemStart :: C.SystemStart -> H.Transaction ()
commitSystemStart systemStart =
  H.statement
    (prepareParams systemStart)
    [resultlessStatement|
    INSERT INTO marlowe.node_status (attr, value)
    VALUES ('systemStart', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

prepareParams :: C.SystemStart -> BS.ByteString
prepareParams = toStrict . runPut . putSystemStart
