{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Marlowe.Indexer (
  IndexerDependencies (..),
  StartPoint (..),
  mkIndexer,
) where

import Control.Concurrent.Component (Component (..), mkComponent, mkComponent_, runComponent)
import Language.Marlowe.Runtime.Core.ScriptRegistry (ScriptRegistry)
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Log (MonadLog, logInfo, logInfo_)
import Marlowe.Indexer.MarloweChainFollower (MarloweChainFollower (..), MarloweChainFollowerDependencies (..), mkMarloweChainFollower)
import Marlowe.Indexer.NodeFollower (MemoryCostConfig, NodeFollower (..), NodeFollowerDependencies (..), mkNodeFollower)
import Marlowe.Indexer.NodeQuerier (NodeQuerier (..), NodeQuerierDependencies (..), Query (QueryChainTip, QueryParams, QueryStartup), mkNodeQuerier)
import Marlowe.Indexer.Store (StoreDependencies (..), mkStore)
import UnliftIO (MonadUnliftIO)
import qualified Cardano.Api as C
import Ouroboros.Network.Point (WithOrigin(..))
import Data.Maybe (mapMaybe)
import Language.Marlowe.Runtime.Cardano.Api (toCardanoBlockHeader)
import Language.Marlowe.Runtime.History.Api (MarloweScriptHashes)

-- | Where the indexer should start its initial chain sync from.
data StartPoint
  = -- | Start from the genesis block. The chain sync will intersect at
    -- genesis and replay the chain from there.
    StartFromGenesis
  | -- | Start from a specific block on the chain identified by its slot
    -- number and header hash. The chain sync will intersect at this block
    -- and begin processing from there.
    StartFromBlock
      !C.SlotNo
      !(C.Hash C.BlockHeader)
  | -- | Default behaviour: if the database already has indexed blocks, start
    -- from the latest one. Otherwise, start from the local node's current
    -- chain tip so the indexer catches up only what is produced after this
    -- run is started.
    StartFromDbOrTip
  deriving stock (Eq, Show)

data IndexerDependencies m = IndexerDependencies
  { localNodeConnectInfo :: !C.LocalNodeConnectInfo
  , databaseQueries :: !(DatabaseQueries m)
  , memoryCostConfig :: !MemoryCostConfig
  , marloweScriptHashes :: !MarloweScriptHashes
  , scriptRegistry :: !ScriptRegistry
  , startPoint :: !StartPoint
  }

-- | Fetch the static network parameters from the local node once and persist
-- them to the database. These values are stable across the lifetime of the
-- node (system start and network id are fixed by the configuration,
-- protocol parameters change only on epoch boundaries) so a single warm-up
-- fetch is sufficient. The runtime server later reads them from the database
-- instead of having to talk to the cardano-node itself.
seedNetworkParameters
  :: forall m
   . MonadLog m
  => NodeQuerier m
  -> DatabaseQueries m
  -> C.NetworkId
  -> m ()
seedNetworkParameters nodeQuerier dbQueries networkId = do
  logInfo_ "Seeding network parameters from the local cardano-node..."
  (systemStart, _genesis, _eraHistory) <- runQuery nodeQuerier QueryStartup
  (C.LedgerProtocolParameters pparams) <- runQuery nodeQuerier QueryParams
  logInfo "Fetched system start" systemStart
  dbQueries.commitSystemStart systemStart
  dbQueries.commitProtocolParameters pparams
  dbQueries.commitNetworkId networkId

mkIndexer
  :: forall m
   . (MonadUnliftIO m, MonadLog m)
  => IndexerDependencies m
  -> Component m ()
mkIndexer IndexerDependencies{..} =
  mkComponent "marlowe-indexer" $ do
    let
        nodeQuerierComponent :: Component m (NodeQuerier m)
        nodeQuerierComponent = mkNodeQuerier NodeQuerierDependencies{localNodeConnectInfo}

        nodeFollowerComponent :: NodeQuerier m -> Component m NodeFollower
        nodeFollowerComponent nodeQuerier = mkNodeFollower NodeFollowerDependencies
          { localNodeConnectInfo
          , getIntersectionPoints = \securityParameter -> case startPoint of
              StartFromGenesis -> pure [Origin]
              StartFromBlock slot hash -> pure [At (C.BlockHeader slot hash 0)]
              StartFromDbOrTip ->
                databaseQueries.getIntersectionPoints securityParameter >>= \case
                  [] -> do
                    logInfo_ "Database is empty; falling back to the local node's chain tip as the starting point."
                    tip <- runQuery nodeQuerier QueryChainTip
                    case tip of
                      C.ChainPointAtGenesis -> pure [Origin]
                      C.ChainPoint tipSlot tipHash -> pure [At (C.BlockHeader tipSlot tipHash (C.BlockNo 0))]
                  points -> pure $ map At . mapMaybe toCardanoBlockHeader $ points
          , memoryCostConfig
          , nodeQuerier
          }

        marloweChainFollowerComponent :: NodeQuerier m -> NodeFollower -> Component m MarloweChainFollower
        marloweChainFollowerComponent nodeQuerier (NodeFollower changes _) = mkMarloweChainFollower MarloweChainFollowerDependencies
          { changes
          , getLatestMarloweUTxO = databaseQueries.getLatestMarloweUTxO
           , marloweScriptHashes
           , scriptRegistry
           , nodeQuerier

          }

        storeComponent :: MarloweChainFollower -> Component m ()
        storeComponent (MarloweChainFollower pullEvent) = mkStore StoreDependencies
          { databaseQueries
          , pullEvent
          }

        warmupComponent :: NodeQuerier m -> Component m ()
        warmupComponent nodeQuerier =
          mkComponent_ "marlowe-indexer-network-params-warmup" $
            seedNetworkParameters nodeQuerier databaseQueries localNodeConnectInfo.localNodeNetworkId

        indexedComponent :: Component m ()
        indexedComponent = do
          nodeQuerier <- nodeQuerierComponent
          nodeFollower <- nodeFollowerComponent nodeQuerier
          warmupComponent nodeQuerier
          nodeQuerier' <- nodeQuerierComponent
          marloweChainFollower <- marloweChainFollowerComponent nodeQuerier' nodeFollower
          storeComponent marloweChainFollower

    (run, ()) <- runComponent indexedComponent
    pure (run, ())