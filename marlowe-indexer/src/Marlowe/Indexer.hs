{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Marlowe.Indexer (
  IndexerDependencies (..),
  mkIndexer,
) where

import Control.Concurrent.Component (Component (..), mkComponent, runComponent)
import Language.Marlowe.Runtime.Core.ScriptRegistry (ScriptRegistry)
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Log (MonadLog)
import Marlowe.Indexer.MarloweChainFollower (MarloweChainFollower (..), MarloweChainFollowerDependencies (..), mkMarloweChainFollower)
import Marlowe.Indexer.NodeFollower (MemoryCostConfig, NodeFollower (..), NodeFollowerDependencies (..), mkNodeFollower)
import Marlowe.Indexer.NodeQuerier (NodeQuerier (..), NodeQuerierDependencies (..), mkNodeQuerier)
import Marlowe.Indexer.Store (StoreDependencies (..), mkStore)
import UnliftIO (MonadUnliftIO)
import qualified Cardano.Api as C
import Ouroboros.Network.Point (WithOrigin(..))
import Data.Maybe (mapMaybe)
import Language.Marlowe.Runtime.Cardano.Api (toCardanoBlockHeader)
import Language.Marlowe.Runtime.History.Api (MarloweScriptHashes)

data IndexerDependencies m = IndexerDependencies
  { localNodeConnectInfo :: !C.LocalNodeConnectInfo
  , databaseQueries :: !(DatabaseQueries m)
  , memoryCostConfig :: !MemoryCostConfig
  , marloweScriptHashes :: !MarloweScriptHashes
  , scriptRegistry :: !ScriptRegistry
  }

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
          , getIntersectionPoints = \securityParameter -> do
              databaseQueries.getIntersectionPoints securityParameter >>= \case
                [] -> pure [Origin]
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

        indexedComponent :: Component m ()
        indexedComponent = do
          nodeQuerier <- nodeQuerierComponent
          nodeFollower <- nodeFollowerComponent nodeQuerier
          nodeQuerier' <- nodeQuerierComponent
          marloweChainFollower <- marloweChainFollowerComponent nodeQuerier' nodeFollower
          storeComponent marloweChainFollower

    (run, ()) <- runComponent indexedComponent
    pure (run, ())
