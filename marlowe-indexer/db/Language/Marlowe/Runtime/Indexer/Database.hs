{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Indexer.Database where

import qualified Cardano.Api as C
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, SlotNo, IndexerTip, NodeTip)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweBlock, MarloweUTxO)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints as GetIntersectionPoints
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetStatus (DecodeError, Status)

data DatabaseQueries m = DatabaseQueries
  { commitBlocks :: [MarloweBlock] -> m ()
  , commitEraHistory :: C.EraHistory -> m ()
  , commitIndexerTip :: IndexerTip -> m ()
  , commitNodeTip :: NodeTip -> m ()
  , commitRollback :: ChainPoint -> m ()
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
    , commitNodeTip = f <$> commitNodeTip
    , commitRollback = f <$> commitRollback
    , getIntersectionPoints = f <$> getIntersectionPoints
    , getMarloweUTxO = f <$> getMarloweUTxO
    , getLatestMarloweUTxO = f getLatestMarloweUTxO
    , getStatus = f getStatus
    }
