{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Indexer.Database where

import qualified Cardano.Api as C
import qualified Cardano.Ledger.Core as L
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, SlotNo, IndexerTip, NodeTip)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweBlock, MarloweUTxO)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints as GetIntersectionPoints
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetStatus (DecodeError, Status)

data DatabaseQueries m = DatabaseQueries
  { commitBlocks :: [MarloweBlock] -> m ()
  , commitEraHistory :: C.EraHistory -> m ()
  , commitIndexerTip :: IndexerTip -> m ()
  , commitNetworkId :: C.NetworkId -> m ()
  , commitNodeTip :: NodeTip -> m ()
  , commitProtocolParameters :: L.PParams (C.ShelleyLedgerEra C.ConwayEra) -> m ()
  , commitRollback :: ChainPoint -> m ()
  , commitSystemStart :: C.SystemStart -> m ()
  , getIntersectionPoints :: GetIntersectionPoints.SecurityParameter -> m [BlockHeader]
  , getMarloweUTxO :: SlotNo -> m MarloweUTxO
  , getLatestMarloweUTxO :: m MarloweUTxO
  , getStatus :: m (Either DecodeError Status)
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} =
  DatabaseQueries
    { commitBlocks = f <$> commitBlocks
    , commitEraHistory = f <$> commitEraHistory
    , commitIndexerTip = f <$> commitIndexerTip
    , commitNetworkId = f <$> commitNetworkId
    , commitNodeTip = f <$> commitNodeTip
    , commitProtocolParameters = f <$> commitProtocolParameters
    , commitRollback = f <$> commitRollback
    , commitSystemStart = f <$> commitSystemStart
    , getIntersectionPoints = f <$> getIntersectionPoints
    , getMarloweUTxO = f <$> getMarloweUTxO
    , getLatestMarloweUTxO = f getLatestMarloweUTxO
    , getStatus = f getStatus
    }
