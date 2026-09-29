module Marlowe.Indexer.NodeQuerier (
  NodeQuerierDependencies (..),
  NodeQuerier (..),
  QueryRequest (..),
  Query (..),
  TimeoutSeconds,
  TimeBoundedNodeQuerier(..),
  hoistNodeQuerier,
  mkNodeQuerier,
  mkTimeoutSeconds,
  mkTimeBoundNodeFollower,
  tenSecondsTimeout,
  stateQueryClient,
) where

import Cardano.Api (
  Address,
  AnyCardanoEra (..),
  AnyShelleyBasedEra (..),
  BlockInMode (..),
  ChainPoint (..),
  ConwayEra,
  EraHistory (..),
  GenesisParameters (..),
  LedgerProtocolParameters (LedgerProtocolParameters),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  QueryUTxOFilter (QueryUTxOByAddress, QueryUTxOByTxIn),
  ShelleyAddr,
  ShelleyBasedEra (ShelleyBasedEraConway),
  ShelleyEra,
  SystemStart,
  TxIn,
  UTxO (..),
  caseByronOrShelleyBasedEra,
  toAddressAny, LocalStateQueryClient (..), LocalNodeClientProtocols (..),
 )
import Cardano.Api.Network (Target (..))
import Control.Exception (
  throwIO,
 )
import Control.Monad.IO.Class (MonadIO (..), liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Log (
  MonadLog,
  logAttention_,
 )
import Log qualified
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Q
import Control.Concurrent.STM (STM)
import UnliftIO (MonadUnliftIO, withRunInIO, atomically, writeTVar, newTVar, readTMVar, newEmptyTMVar, newEmptyTMVarIO, TMVar, registerDelay, readTVar, orElse, takeTMVar, putTMVar)
import Control.Monad.Reader.Class ( MonadReader, ask )
import Control.Concurrent.Component (Component, mkComponent)
import qualified Cardano.Api as C
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as C
import Control.Monad (void)

logTrace_ :: MonadLog m => Text -> m ()
logTrace_ msg = Log.logTrace_ $ "[NodeQuerier] " <> msg

data Query a where
  QueryStartup :: Query (SystemStart, GenesisParameters ShelleyEra, EraHistory)
  QueryHistory :: Query EraHistory
  QueryParams :: Query (LedgerProtocolParameters ConwayEra)
  QueryTxIns :: [TxIn] -> Query (UTxO ConwayEra)
  QueryAddress :: Address ShelleyAddr -> Query (UTxO ConwayEra)
  QueryChainTip :: Query ChainPoint

deriving instance Show (Query a)

data QueryRequest where
  QueryRequest :: Query a -> TMVar a -> QueryRequest

newtype TimeoutSeconds = TimeoutSeconds Int

mkTimeoutSeconds :: Int -> Maybe TimeoutSeconds
mkTimeoutSeconds seconds
  | seconds <= 0 || seconds > (maxBound `div` 1000000) = Nothing
  | otherwise = Just $ TimeoutSeconds seconds

tenSecondsTimeout :: TimeoutSeconds
tenSecondsTimeout = case mkTimeoutSeconds 10 of
  Just timeout -> timeout
  Nothing -> error "Impossible: Invalid timeout value"

newtype NodeQuerierDependencies = NodeQuerierDependencies
  { localNodeConnectInfo :: C.LocalNodeConnectInfo }

newtype TimeBoundedNodeQuerier m = TimeBoundedNodeQuerier { query :: forall a. Query a -> m (Maybe a) }

mkTimeBoundNodeFollower
  :: forall m
   . MonadUnliftIO m
  => MonadLog m
  => NodeQuerierDependencies
  -> TimeoutSeconds
  -> Component m (TimeBoundedNodeQuerier m)
mkTimeBoundNodeFollower NodeQuerierDependencies{..} (TimeoutSeconds _timeoutSeconds) = mkComponent "time-bounded-node-querier" $ do
  connectedVar <- newTVar False
  requestVar <- newEmptyTMVar
  let
    readRequest :: STM QueryRequest
    readRequest = takeTMVar requestVar

    clearRequest :: STM ()
    clearRequest = void $ takeTMVar requestVar

    runNodeQuerier :: m ()
    runNodeQuerier = withRunInIO \runInIO -> do
      let
        stateQueryClient' :: LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
        stateQueryClient' = do
          let
            LocalStateQueryClient client = stateQueryClient
          hoistQueryClient (runInIO . flip runReaderT (readRequest, clearRequest)) $ LocalStateQueryClient $ do
            atomically $ writeTVar connectedVar True
            client

      runInIO $
        C.connectToLocalNode localNodeConnectInfo $
          C.LocalNodeClientProtocols
            { localChainSyncClient = C.NoLocalChainSyncClient
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            , localStateQueryClient = Just stateQueryClient'
            }

    doRequest :: forall a. Query a -> m (Maybe a)
    doRequest query = liftIO do
      resultVar <- newEmptyTMVarIO
      let
        req = QueryRequest query resultVar
      atomically $ putTMVar requestVar req
      delayVar <- registerDelay (_timeoutSeconds * 1000000)
      let
        readResult = do
          val <- readTMVar resultVar
          pure $ Just val
        readTimeout = do
          timeout <- readTVar delayVar
          if timeout
            then pure Nothing
            else readResult
      atomically $ readResult `orElse` readTimeout
  pure (runNodeQuerier, TimeBoundedNodeQuerier doRequest)

newtype NodeQuerier m = NodeQuerier { runQuery :: forall a. Query a -> m a }

hoistNodeQuerier :: (forall x. m x -> n x) -> NodeQuerier m -> NodeQuerier n
hoistNodeQuerier f (NodeQuerier query) = NodeQuerier $ \query' -> f $ query query'

mkNodeQuerier
  :: forall m
   . MonadUnliftIO m
  => MonadLog m
  => NodeQuerierDependencies
  -> Component m (NodeQuerier m)
mkNodeQuerier NodeQuerierDependencies{..} = mkComponent "node-querier" $ do
  connectedVar <- newTVar False
  requestVar <- newEmptyTMVar
  let
    readRequest :: STM QueryRequest
    readRequest = readTMVar requestVar

    clearRequest :: STM ()
    clearRequest = void $ takeTMVar requestVar

    runNodeQuerier :: m ()
    runNodeQuerier = withRunInIO \runInIO -> do
      let
        stateQueryClient' :: LocalStateQueryClient BlockInMode ChainPoint QueryInMode IO ()
        stateQueryClient' = do
          let
            LocalStateQueryClient client = stateQueryClient
          hoistQueryClient (runInIO . flip runReaderT (readRequest, clearRequest)) $ LocalStateQueryClient $ do
            atomically $ writeTVar connectedVar True
            client

      runInIO $
        C.connectToLocalNode localNodeConnectInfo $
          C.LocalNodeClientProtocols
            { localChainSyncClient = C.NoLocalChainSyncClient
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            , localStateQueryClient = Just stateQueryClient'
            }

    doRequest :: forall a. Query a -> m a
    doRequest query = do
      logTrace_ $ "Submitting query: " <> T.pack (show query)
      result <- liftIO do
        resultVar <- newEmptyTMVarIO
        let
          req = QueryRequest query resultVar
        atomically $ putTMVar requestVar req
        atomically $ readTMVar resultVar
      logTrace_ $ "Received query result for: " <> T.pack (show query)
      pure result
  pure (runNodeQuerier, NodeQuerier doRequest)

stateQueryClient
  :: forall m
   . MonadIO m
  => MonadLog m
  => MonadReader (STM QueryRequest, STM ()) m
  => Q.LocalStateQueryClient BlockInMode ChainPoint QueryInMode m ()
stateQueryClient = Q.LocalStateQueryClient go
  where
    go = do
      (readRequest, clearRequest) <- ask
      QueryRequest query res <- liftIO $ atomically readRequest
      pure $
        Q.SendMsgAcquire
          VolatileTip
          Q.ClientStAcquiring
            { recvMsgAcquired = handleRequest query \a -> do
                liftIO $ atomically do
                  clearRequest
                  putTMVar res a
                pure $ Q.SendMsgRelease go
            , recvMsgFailure = \acquireFailure ->
                logThrow $
                  "Failed to acquire immutable tip, as per documentation this should not happen. "
                    <> T.pack (show acquireFailure)
            }

handleRequest
  :: forall m a
   . MonadIO m
  => MonadLog m
  => Query a
  -> (a -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ()))
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
handleRequest = \case
  QueryStartup -> queryStartup
  QueryHistory -> queryHistory
  QueryParams -> queryParams
  QueryTxIns inputs -> queryTxIns inputs
  QueryAddress addr -> queryAddress addr
  QueryChainTip -> queryChainTip

queryStartup
  :: forall m
   . MonadIO m
  => MonadLog m
  => ( (SystemStart, GenesisParameters ShelleyEra, EraHistory)
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryStartup k = do
  pure $ Q.SendMsgQuery QueryCurrentEra $ Q.ClientStQuerying \case
    AnyCardanoEra era -> do
      AnyShelleyBasedEra era' <-
        caseByronOrShelleyBasedEra
          (logThrow "Cannot operate with a node in Byron era")
          (pure . AnyShelleyBasedEra)
          era
      let genesisParamsQ = QueryInEra $ QueryInShelleyBasedEra era' QueryGenesisParameters
      pure $ Q.SendMsgQuery genesisParamsQ $ Q.ClientStQuerying \case
        Left err -> logThrow $ "Failed to query genesis params " <> T.pack (show err)
        Right genesisParams ->
          pure $ Q.SendMsgQuery QuerySystemStart $ Q.ClientStQuerying \systemStart ->
            pure $ Q.SendMsgQuery QueryEraHistory $ Q.ClientStQuerying \eraHistory ->
              k (systemStart, genesisParams, eraHistory)

queryParams
  :: forall m
   . MonadIO m
  => MonadLog m
  => ( LedgerProtocolParameters ConwayEra
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryParams k = do
  let pparamsQ =
        QueryInEra $
          QueryInShelleyBasedEra ShelleyBasedEraConway QueryProtocolParameters
  pure $ Q.SendMsgQuery pparamsQ $ Q.ClientStQuerying \case
    Left err -> logThrow $ "Failed to query protocol params " <> T.pack (show err)
    Right pparams -> k $ LedgerProtocolParameters pparams

queryHistory
  :: forall m
   . MonadIO m
  => ( EraHistory
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryHistory = pure . Q.SendMsgQuery QueryEraHistory . Q.ClientStQuerying

queryTxIns
  :: forall m
   . MonadIO m
  => MonadLog m
  => [TxIn]
  -> ( UTxO ConwayEra
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryTxIns inputs k = do
  let utxoQ =
        QueryInEra
          . QueryInShelleyBasedEra ShelleyBasedEraConway
          . QueryUTxO
          . QueryUTxOByTxIn
          . Set.fromList
          $ inputs
  pure $ Q.SendMsgQuery utxoQ $ Q.ClientStQuerying \case
    Left err -> logThrow $ "Failed to query utxo" <> T.pack (show err)
    Right utxo -> k utxo

queryAddress
  :: forall m
   . MonadIO m
  => MonadLog m
  => Address ShelleyAddr
  -> ( UTxO ConwayEra
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryAddress addr k = do
  let utxoQ =
        QueryInEra
          . QueryInShelleyBasedEra ShelleyBasedEraConway
          . QueryUTxO
          . QueryUTxOByAddress
          . Set.singleton
          . toAddressAny
          $ addr
  pure $ Q.SendMsgQuery utxoQ $ Q.ClientStQuerying \case
    Left err -> logThrow $ "Failed to query utxo" <> T.pack (show err)
    Right utxo -> k utxo

queryChainTip
  :: forall m
   . MonadIO m
  => ( ChainPoint
       -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
     )
  -> m (Q.ClientStAcquired BlockInMode ChainPoint QueryInMode m ())
queryChainTip k =
  pure $ Q.SendMsgQuery QueryChainPoint $ Q.ClientStQuerying \tip -> k tip

logThrow :: (MonadLog m, MonadIO m) => Text -> m a
logThrow msg = do
  logAttention_ msg
  liftIO $ throwIO $ userError $ T.unpack msg

hoistQueryClient
  :: forall m n
   . (Functor m)
  => (forall x. m x -> n x)
  -> C.LocalStateQueryClient BlockInMode ChainPoint C.QueryInMode m ()
  -> C.LocalStateQueryClient BlockInMode ChainPoint C.QueryInMode n ()
hoistQueryClient f (C.LocalStateQueryClient m) = C.LocalStateQueryClient $ f $ hoistIdle <$> m
  where
    hoistIdle :: C.ClientStIdle block point query m a -> C.ClientStIdle block point query n a
    hoistIdle = \case
      C.SendMsgAcquire target stAcquiring -> C.SendMsgAcquire target $ hoistAcquiring stAcquiring
      C.SendMsgDone a -> C.SendMsgDone a

    hoistAcquiring :: C.ClientStAcquiring block point query m a -> C.ClientStAcquiring block point query n a
    hoistAcquiring C.ClientStAcquiring{..} =
      C.ClientStAcquiring
        { C.recvMsgAcquired = f $ hoistAcquired <$> recvMsgAcquired
        , C.recvMsgFailure = \acquireFailure -> f $ hoistIdle <$> recvMsgFailure acquireFailure
        }

    hoistAcquired :: C.ClientStAcquired block point query m a -> C.ClientStAcquired block point query n a
    hoistAcquired = \case
      C.SendMsgQuery query stQuerying -> C.SendMsgQuery query $ hoistQuerying stQuerying
      C.SendMsgReAcquire target stAcquiring -> C.SendMsgReAcquire target $ hoistAcquiring stAcquiring
      C.SendMsgRelease mIdle -> C.SendMsgRelease $ f $ hoistIdle <$> mIdle

    hoistQuerying :: C.ClientStQuerying block point query m a result -> C.ClientStQuerying block point query n a result
    hoistQuerying C.ClientStQuerying{..} =
      C.ClientStQuerying
        { C.recvMsgResult = \result -> f $ hoistAcquired <$> recvMsgResult result
        }

