{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetSystemStart where

import qualified Cardano.Api as C
import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.NetworkParams (decodeSystemStart)
import Language.Marlowe.Runtime.Query.Database (GetSystemStartError (InvalidSystemStart, MissingSystemStart))

type GetSystemStart m = m (Either GetSystemStartError C.SystemStart)

getSystemStartQuery :: GetSystemStart T.Transaction
getSystemStartQuery = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT node_status.value :: bytea
          FROM marlowe.node_status
          WHERE node_status.attr = 'systemStart'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleBytes -> do
    bytes <- note MissingSystemStart possibleBytes
    bimap (const $ InvalidSystemStart bytes) id $ decodeSystemStart bytes
