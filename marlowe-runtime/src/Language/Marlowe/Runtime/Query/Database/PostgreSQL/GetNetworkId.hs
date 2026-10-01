{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetNetworkId where

import qualified Cardano.Api as C
import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString, fromStrict)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (getNetworkId)
import Language.Marlowe.Runtime.Query.Database (GetNetworkIdError (InvalidNetworkId, MissingNetworkId))

type GetNetworkId m = m (Either GetNetworkIdError C.NetworkId)

getNetworkIdQuery :: C.NetworkId -> GetNetworkId T.Transaction
getNetworkIdQuery _expected = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT node_status.value :: bytea
          FROM marlowe.node_status
          WHERE node_status.attr = 'networkId'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleBytes -> do
    bytes <- note MissingNetworkId possibleBytes
    bimap (const $ InvalidNetworkId bytes) (\(_, _, nid) -> nid) $
      runGetOrFail getNetworkId (fromStrict bytes)
