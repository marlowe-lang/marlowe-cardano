module Marlowe.Indexer.NodeFollower (
  Changes (..),
  MemoryCostConfig (..),
  ChangesMemoryCostModel (..),
  IndexerTip (..),
  NodeTip (..),
  NodeFollower (..),
  NodeFollowerDependencies (..),
  RollbackToBlock (..),
  areChangesEmpty,
  blockHeaderToPoint,
  mkIndexerTipFromBlockHeader,
  mkNodeFollower,
  toEmptyChanges,
) where

import Cardano.Api (
  BlockInMode (..),
  BlockNo (..),
  ChainPoint (..),
  ChainSyncClientPipelined (..),
  ChainTip (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  SlotNo (..),
  getBlockHeader,
  serialiseToRawBytes, BlockHeader (..),
 )
import Network.TypedProtocol (N(..), pattern Succ, Nat (..))
import Control.Arrow ((&&&))
import Control.Concurrent.Component (Component, mkComponent)
import Control.Concurrent.STM (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Base16 (encodeBase16)
import Data.IORef (IORef)
import Debug.Trace (traceM)
-- FIXME: Why we use a lazy map here?
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.List (sortOn)
import Data.Maybe (mapMaybe, isNothing)
import Data.Ord (Down (..))
import Data.String (fromString)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ouroboros.Network.Point (WithOrigin (..))
import Text.Printf (printf)
import UnliftIO (MonadIO, MonadUnliftIO, atomicModifyIORef, atomically, newIORef, withRunInIO, writeIORef, readTVarIO)
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ClientPipelinedStIdle (..), ClientStNext (..), ClientPipelinedStIntersect (..), mapChainSyncClientPipelined)
import Log (MonadLog)
import Log qualified
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (MkPipelineDecision, PipelineDecision (..), pipelineDecisionLowHighMark, runPipelineDecision)
import Data.Base16.Types (extractBase16)
import qualified Marlowe.Indexer.NodeFollower.Block as Block
import Control.Monad.Reader (ReaderT (..), MonadReader, ask)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints (SecurityParameter(..))
import Marlowe.Indexer.NodeQuerier (NodeQuerier, runQuery, Query (QueryStartup))
import Data.Functor ((<&>))
import Cardano.Ledger.BaseTypes (NonZero(..))
import Data.Text (Text)

logInfo_ :: MonadLog m => Text -> m ()
logInfo_ msg = Log.logInfo_ $ "[NodeFollower] " <> msg

type NumberedCardanoBlock = (BlockNo, BlockInMode)
type NumberedChainTip = (WithOrigin BlockNo, ChainTip)

-- | Parameters for estimating the cost of writing a batch of changes.
data ChangesMemoryCostModel = ChangesMemoryCostModel
  { blockMemoryCost :: Int
  , txMemoryCost :: Int
  }
  deriving (Show, Eq)

-- | Computes the cost of a change set. The value is a unitless heuristic.
--   Prevents large numbers of transactions and blocks being held in memory.
memoryCost :: ChangesMemoryCostModel -> Changes -> Int
memoryCost ChangesMemoryCostModel{..} Changes{..} =
  changesBlockCount * blockMemoryCost + changesTxCount * txMemoryCost

newtype IndexerTip = IndexerTip ChainTip
  deriving newtype (Eq, Show)

mkIndexerTipFromBlockHeader :: BlockHeader -> IndexerTip
mkIndexerTipFromBlockHeader (BlockHeader slot hash blockNo) =
  IndexerTip $ ChainTip slot hash blockNo

instance ToJSON IndexerTip where
  toJSON (IndexerTip tip) = A.object
    [ "IndexerTip" .= toJSON tip
    ]

newtype NodeTip = NodeTip ChainTip
  deriving newtype (Eq, Show)

instance ToJSON NodeTip where
  toJSON (NodeTip tip) = A.object
    [ "NodeTip" .= toJSON tip
    ]

-- Missing block header implies rollback to genesis.
newtype RollbackToBlock = RollbackToBlock (Maybe BlockHeader)

minRollbackPoint :: RollbackToBlock -> RollbackToBlock -> RollbackToBlock
minRollbackPoint (RollbackToBlock Nothing) _ = RollbackToBlock Nothing
minRollbackPoint _ (RollbackToBlock Nothing) = RollbackToBlock Nothing
minRollbackPoint (RollbackToBlock (Just (BlockHeader s1 h1 n1))) (RollbackToBlock (Just (BlockHeader s2 h2 n2)))
  | s1 < s2 = RollbackToBlock (Just (BlockHeader s1 h1 n1))
  | otherwise = RollbackToBlock (Just (BlockHeader s2 h2 n2))

-- | Describes a batch of chain data changes to write.
-- | IMPORTANT: Blocks are stored in reverse order (newest first).
data Changes = Changes
  { changesRollback :: !(Maybe RollbackToBlock)
  -- ^ Point to rollback to before writing any blocks.
  , changesBlocks :: ![BlockInMode]
  -- ^ New blocks to write. Ordered from the most recent to the oldest.
  , changesTip :: !NodeTip
  -- ^ Most recently observed tip of the local node.
  , changesIndexerTip :: !IndexerTip
  -- ^ Chain tip the changes will advance the local state to.
  , changesBlockCount :: !Int
  -- ^ Number of blocks in the change set.
  , changesTxCount :: !Int
  -- ^ Number of transactions in the change set.
  }

-- | An empty Changes collection.
emptyChanges :: Changes
emptyChanges = Changes Nothing [] (NodeTip ChainTipAtGenesis) (IndexerTip ChainTipAtGenesis) 0 0

-- | Make a set of changes into an empty set (preserves the tip and point fields).
toEmptyChanges :: Changes -> Changes
toEmptyChanges changes =
  changes
    { changesRollback = Nothing
    , changesBlocks = []
    , changesBlockCount = 0
    , changesTxCount = 0
    }

areChangesEmpty :: Changes -> Bool
areChangesEmpty Changes{..} =
  isNothing changesRollback && null changesBlocks && changesBlockCount == 0 && changesTxCount == 0

data MemoryCostConfig = MemoryCostConfig
  { maxMemoryCost :: Int
  , changesMemoryCostModel :: ChangesMemoryCostModel
  }

-- | The set of dependencies needed by the NodeFollower component.
data NodeFollowerDependencies m = NodeFollowerDependencies
  { localNodeConnectInfo :: !C.LocalNodeConnectInfo
  -- ^ Connect to the local node.
  , getIntersectionPoints :: SecurityParameter -> m [WithOrigin BlockHeader]
  -- ^ How to load the set of initial intersection points for the chain sync client.
  -- | The maximum cost a set of changes is allowed to incur before the
  -- NodeFollower blocks.
  , memoryCostConfig :: MemoryCostConfig
  , nodeQuerier :: NodeQuerier m
  }

-- | The public API of the NodeFollower component.
data NodeFollower = NodeFollower
  { changes :: STM Changes
  -- ^ An STM action that atomically reads and clears the current change set.
  , connected :: STM Bool
  }

type FollowerT m = ReaderT MemoryCostConfig m

-- | Create a new NodeFollower component.
mkNodeFollower
  :: forall m
   . MonadUnliftIO m
  => MonadLog m
  => NodeFollowerDependencies m
  -> Component m NodeFollower
mkNodeFollower NodeFollowerDependencies{..}  = mkComponent "indexer-node-client" do
  changesVar <- newTVar emptyChanges
  connectedVar <- newTVar False
  securityParamVar <- newTVar Nothing
  let
      changes :: STM Changes
      changes = do
        -- Read the changes and reset the state
        currentChanges <- readTVar changesVar
        modifyTVar changesVar toEmptyChanges
        pure currentChanges

      pipelinedClient' :: ChainSyncClientPipelined BlockInMode ChainPoint ChainTip (FollowerT m) ()
      pipelinedClient' =
        mapChainSyncClientPipelined id id (blockToBlockNo &&& id) (chainTipToBlockNo &&& id) $
          mkPipelinedClient
            changesVar
            do
              securityParam <- liftIO (readTVarIO securityParamVar) >>= \case
                Just s -> pure s
                Nothing -> do
                  logInfo_ "Querying system start from node..."
                  s <- lift $ runQuery nodeQuerier QueryStartup <&> \(_, C.GenesisParameters{protocolParamSecurity}, _) ->
                    SecurityParameter . toInteger . unNonZero $ protocolParamSecurity

                  liftIO $ atomically $ writeTVar securityParamVar (Just s)
                  pure s
              lift $ getIntersectionPoints securityParam

      runNodeFollower :: m ()
      runNodeFollower = withRunInIO \runInIO ->
        runInIO $
          C.connectToLocalNode localNodeConnectInfo $
            LocalNodeClientProtocols
              { localChainSyncClient = do
                  let ChainSyncClientPipelined client = pipelinedClient'
                  LocalChainSyncClientPipelined
                    $ hoistClient (runInIO . flip runReaderT memoryCostConfig) $ ChainSyncClientPipelined do
                        atomically $ writeTVar connectedVar True
                        client
              , localTxSubmissionClient = Nothing
              , localTxMonitoringClient = Nothing
              , localStateQueryClient = Nothing
              }

      connected = readTVar connectedVar

  pure (runNodeFollower, NodeFollower{changes, connected})

hoistClient
  :: forall block point tip m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> ChainSyncClientPipelined block point tip m a
  -> ChainSyncClientPipelined block point tip n a
hoistClient f (ChainSyncClientPipelined m) = ChainSyncClientPipelined $ f $ hoistIdle <$> m
  where
    hoistIdle :: ClientPipelinedStIdle i block point tip m a -> ClientPipelinedStIdle i block point tip n a
    hoistIdle = \case
      SendMsgRequestNext stAwait stNext -> SendMsgRequestNext (f stAwait) (hoistNext stNext)
      SendMsgRequestNextPipelined await idle -> SendMsgRequestNextPipelined (f await) (hoistIdle idle)
      SendMsgFindIntersect points intersect -> SendMsgFindIntersect points $ hoistIntersect intersect
      SendMsgDone a -> SendMsgDone a
      CollectResponse mIdle next -> CollectResponse (fmap (f . fmap hoistIdle) mIdle) (hoistNext next)

    hoistNext :: ClientStNext i block point tip m a -> ClientStNext i block point tip n a
    hoistNext ClientStNext{..} =
      ClientStNext
        { recvMsgRollForward = fmap (f . fmap hoistIdle) . recvMsgRollForward
        , recvMsgRollBackward = fmap (f . fmap hoistIdle) . recvMsgRollBackward
        }

    hoistIntersect :: ClientPipelinedStIntersect block point tip m a -> ClientPipelinedStIntersect block point tip n a
    hoistIntersect ClientPipelinedStIntersect{..} =
      ClientPipelinedStIntersect
        { recvMsgIntersectFound = fmap (f . fmap hoistIdle) . recvMsgIntersectFound
        , recvMsgIntersectNotFound = f . fmap hoistIdle . recvMsgIntersectNotFound
        }

blockHeaderToBlockNo :: BlockHeader -> BlockNo
blockHeaderToBlockNo (BlockHeader _ _ blockNo) = blockNo

blockToBlockNo :: BlockInMode -> BlockNo
blockToBlockNo (BlockInMode _ block) = blockHeaderToBlockNo $ getBlockHeader block

blockHeaderToPoint :: WithOrigin BlockHeader -> ChainPoint
blockHeaderToPoint Origin = ChainPointAtGenesis
blockHeaderToPoint (At (BlockHeader s h _)) = ChainPoint s h

chainTipToBlockNo :: ChainTip -> WithOrigin BlockNo
chainTipToBlockNo = \case
  ChainTipAtGenesis -> Origin
  ChainTip _ _ blockNo -> At blockNo

mkPipelinedClient
  :: forall m
   . MonadIO m
  => MonadLog m
  => MonadReader MemoryCostConfig m
  => TVar Changes
  -> m [WithOrigin BlockHeader]
  -> ChainSyncClientPipelined NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkPipelinedClient changesVar getIntersectionPoints =
  ChainSyncClientPipelined do
    headers <- sortOn (Down . fmap blockHeaderToBlockNo) <$> getIntersectionPoints
    case headers of
      (At (BlockHeader _ hash no) : _) ->
        logInfo_ $ do
          let
            msg = printf
              "Tip of local chain: %s (#%d)"
              (extractBase16 . encodeBase16 $ serialiseToRawBytes hash)
              (unBlockNo no)
          fromString msg
      _ -> logInfo_ "Local chain empty"
    pure $
      SendMsgFindIntersect
        (blockHeaderToPoint <$> headers)
        ClientPipelinedStIntersect
          { recvMsgIntersectFound = \point tip -> do
              logInfo_ "Intersected with node chain"
              let
                getSlotAndBlock :: WithOrigin BlockHeader -> Maybe (Int, BlockNo)
                getSlotAndBlock = case point of
                    ChainPointAtGenesis -> const Nothing
                    ChainPoint pointSlot _ -> \case
                      Origin -> Nothing
                      At (BlockHeader (SlotNo s) _ b)
                        | SlotNo s <= pointSlot -> Just (fromIntegral s, b)
                        | otherwise -> Nothing
                slotNoToBlockNo = IntMap.fromList $ mapMaybe getSlotAndBlock headers
              clientStIdle slotNoToBlockNo point tip
          , recvMsgIntersectNotFound = \tip -> do
              logInfo_ "No intersection with node chain"
              clientStIdle mempty ChainPointAtGenesis tip
          }
  where
    clientStIdle
      :: IntMap BlockNo
      -> ChainPoint
      -> NumberedChainTip
      -> m (ClientPipelinedStIdle 'Z NumberedCardanoBlock ChainPoint NumberedChainTip m ())
    clientStIdle slotNoToBlockNo point nodeTip = do
      lastLog <- newIORef $ posixSecondsToUTCTime 0
      let clientTip = case point of
            ChainPointAtGenesis -> Origin
            ChainPoint (SlotNo s) _ -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
              Nothing -> error $ "Unable to find block number for chain point " <> show point
              Just b -> At b
      pure $ mkClientStIdle lastLog changesVar slotNoToBlockNo pipelinePolicy Zero clientTip nodeTip

    -- How to pipeline. If we have fewer than 50 requests in flight, send
    -- another request. When we hit 50, start collecting responses until we
    -- have 1 request in flight, then repeat. If we are caught up to tip,
    -- requests will not be pipelined.
    pipelinePolicy :: MkPipelineDecision
    pipelinePolicy = pipelineDecisionLowHighMark 1 50

mkClientStIdle
  :: forall m n
   . MonadIO m
  => MonadLog m
  => MonadReader MemoryCostConfig m
  => IORef UTCTime
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> WithOrigin BlockNo
  -> NumberedChainTip
  -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStIdle lastLog changesVar slotNoToBlockNo pipelineDecision n clientTip nodeTip =
  case (n, runPipelineDecision pipelineDecision n clientTip (fst nodeTip)) of
    (_, (Request, pipelineDecision')) ->
      SendMsgRequestNext (pure ()) (collect pipelineDecision' n)
    (_, (Pipeline, pipelineDecision')) ->
      nextPipelineRequest pipelineDecision'
    (Succ n', (CollectOrPipeline, pipelineDecision')) ->
      CollectResponse
        (Just $ pure $ nextPipelineRequest pipelineDecision')
        (collect pipelineDecision' n')
    (Succ n', (Collect, pipelineDecision')) ->
      CollectResponse Nothing (collect pipelineDecision' n')
  where
    nextPipelineRequest
      :: MkPipelineDecision
      -> ClientPipelinedStIdle n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    nextPipelineRequest pipelineDecision' =
      SendMsgRequestNextPipelined (pure ()) $
        mkClientStIdle lastLog changesVar slotNoToBlockNo pipelineDecision' (Succ n) clientTip nodeTip

    collect
      :: forall n'
       . MkPipelineDecision
      -> Nat n'
      -> ClientStNext n' NumberedCardanoBlock ChainPoint NumberedChainTip m ()
    collect = mkClientStNext lastLog changesVar slotNoToBlockNo

mkClientStNext
  :: MonadIO m
  => MonadLog m
  => MonadReader MemoryCostConfig m
  => IORef UTCTime
  -> TVar Changes
  -> IntMap BlockNo
  -> MkPipelineDecision
  -> Nat n
  -> ClientStNext n NumberedCardanoBlock ChainPoint NumberedChainTip m ()
mkClientStNext lastLog changesVar slotNoToBlockNo pipelineDecision n =
  ClientStNext
    { recvMsgRollForward = \(blockNo, blockInMode@(BlockInMode _ block)) tip -> do
        let header@(BlockHeader slotNo hash _) = getBlockHeader block
        traceM ("[NODE-FOLLOWER] RollForward received: blockNo=" ++ show (unBlockNo blockNo) ++ " slotNo=" ++ show slotNo ++ " txCount=" ++ show (Block.txCount blockInMode))
        now <- liftIO getCurrentTime
        canLog <- atomicModifyIORef lastLog \lastLogValue ->
          if diffUTCTime now lastLogValue >= minLogPeriod
            then (now, True)
            else (lastLogValue, False)
        when canLog do
          let localBlockNo = unBlockNo blockNo
          let remoteBlockNo = case fst tip of
                Origin -> 0
                At remote -> unBlockNo remote
          logInfo_ $
            fromString $
              printf
                "Rolled forward to block %s (#%d of %d) (%d%%)"
                (extractBase16 . encodeBase16 $ serialiseToRawBytes hash)
                localBlockNo
                remoteBlockNo
                ((localBlockNo * 100) `div` remoteBlockNo)
        memoryCostConfig <- ask
        atomically do
          changes <- readTVar changesVar
          let nextChanges =
                changes
                  { changesBlocks = blockInMode : changesBlocks changes
                  , changesTip = (NodeTip . snd) tip
                  , changesIndexerTip = mkIndexerTipFromBlockHeader header
                  , changesBlockCount = changesBlockCount changes + 1
                  , changesTxCount = changesTxCount changes + Block.txCount blockInMode
                  }
          -- Retry unless the next change set would not be too expensive.
          guard $ memoryCost memoryCostConfig.changesMemoryCostModel nextChanges <= memoryCostConfig.maxMemoryCost
          writeTVar changesVar nextChanges
        let clientTip = At blockNo
        let slotNoToInt (SlotNo s) = fromIntegral s
        -- FIXME: Can we leak memory here? I can not find any garbage collection for this map which should be
        -- done for non volatile blocks I believe (based on my intuition that it is used only during rollbacks).
        let slotNoToBlockNo' = IntMap.insert (slotNoToInt slotNo) blockNo slotNoToBlockNo
        pure $ mkClientStIdle lastLog changesVar slotNoToBlockNo' pipelineDecision n clientTip tip
    , recvMsgRollBackward = \point tip -> do
        traceM ("[NODE-FOLLOWER] RollBackward")
        now <- liftIO getCurrentTime
        writeIORef lastLog now
        let clientTip = case point of
              ChainPointAtGenesis -> Origin
              ChainPoint (SlotNo s) hash -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
                Nothing -> error $ "Unable to find block number for chain point " <> show point
                Just b -> At (hash, b)
        logInfo_ case snd tip of
          ChainTipAtGenesis -> "Local node rolled back to genesis."
          ChainTip _ hash no ->
            fromString $
              printf
                "Local node switched to new tip %s (#%d)"
                (extractBase16 . encodeBase16 $ serialiseToRawBytes hash)
                (unBlockNo no)
        let remote = case fst tip of
              Origin -> 0
              At r -> unBlockNo r
        logInfo_ case clientTip of
          Origin -> "Rolled back to genesis"
          At (hash, BlockNo local) ->
            fromString $
              printf
                "Rolled back to block %s (#%d of %d) (%d%%)"
                (extractBase16 . encodeBase16 $ serialiseToRawBytes hash)
                local
                remote
                ((local * 100) `div` remote)
        atomically do
          Changes{..} <- readTVar changesVar
          let changesBlocks' = case point of
                ChainPointAtGenesis -> []
                ChainPoint slot _ -> dropWhile ((> slot) . blockSlot) changesBlocks
              rollback = case point of
                ChainPointAtGenesis -> RollbackToBlock Nothing
                ChainPoint (SlotNo s) hash -> case IntMap.lookup (fromIntegral s) slotNoToBlockNo of
                  Nothing -> error $ "Unable to find block number for chain point " <> show point
                  Just b -> RollbackToBlock (Just $ BlockHeader (SlotNo s) hash b)
              newChanges =
                Changes
                  { changesBlocks = changesBlocks'
                  , changesRollback = case changesRollback of
                      -- If there was no previous rollback, and we still have blocks
                      -- in the batch after the rollback, we don't need to actually
                      -- process the rollback.
                      Nothing -> rollback <$ guard (null changesBlocks')
                      -- Otherwise, we need to process whichever rollback was to an
                      -- earlier point: the previous one, or this new one.
                      Just prevRollback -> Just $ minRollbackPoint rollback prevRollback
                  , changesTip = NodeTip . snd $ tip
                  , changesIndexerTip = case (point, clientTip) of
                      (ChainPointAtGenesis, _) -> IndexerTip ChainTipAtGenesis
                      (_, Origin) -> IndexerTip ChainTipAtGenesis
                      (ChainPoint slotNo hash, At (_, blockNo)) -> IndexerTip $ ChainTip slotNo hash blockNo
                  , changesBlockCount = length changesBlocks'
                  , changesTxCount = sum $ Block.txCount <$> changesBlocks
                  }
          writeTVar changesVar newChanges
        let slotNoToBlockNo' = case point of
              ChainPointAtGenesis -> mempty
              ChainPoint (SlotNo s) _ ->
                IntMap.fromDistinctAscList $
                  reverse $
                    dropWhile ((s <) . fromIntegral . fst) $
                      IntMap.toDescList slotNoToBlockNo
        pure $ mkClientStIdle lastLog changesVar slotNoToBlockNo' pipelineDecision n (snd <$> clientTip) tip
    }
  where
    minLogPeriod = secondsToNominalDiffTime 1

blockSlot :: BlockInMode -> SlotNo
blockSlot (BlockInMode _ (getBlockHeader -> (BlockHeader slot _ _))) = slot


