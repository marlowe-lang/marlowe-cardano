{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetProtocolParameters where

import qualified Cardano.Api as C
import qualified Cardano.Ledger.Core as L
import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (decodeProtocolParameters)
import Language.Marlowe.Runtime.Query.Database (GetProtocolParametersError (InvalidProtocolParameters, MissingProtocolParameters))

type GetProtocolParameters m = m (Either GetProtocolParametersError (L.PParams (C.ShelleyLedgerEra C.ConwayEra)))

getProtocolParametersQuery :: GetProtocolParameters T.Transaction
getProtocolParametersQuery = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT node_status.value :: bytea
          FROM marlowe.node_status
          WHERE node_status.attr = 'protocolParameters'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleBytes -> do
    bytes <- note MissingProtocolParameters possibleBytes
    bimap (const $ InvalidProtocolParameters bytes) id $ decodeProtocolParameters bytes
