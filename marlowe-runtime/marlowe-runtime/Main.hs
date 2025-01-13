{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Cardano.Api (
  ConsensusModeParams (..),
  EpochSlots (..),
  File (..),
  GenesisParameters (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  ShelleyBasedEra (..),
  queryNodeLocalState,
 )
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Exception (bracket)
import Control.Monad (unless, when, (<=<))
import Control.Monad.Event.Class
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Control.Monad.Reader (MonadReader (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Maybe (fromMaybe)
import qualified Data.Set.NonEmpty as NESet
import Data.String (fromString)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Version (showVersion)
import Data.Word (Word64)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Protocol.Server (marloweRuntimeServerDirectPeer)
import Language.Marlowe.Runtime (MarloweRuntime (..), MarloweRuntimeDependencies (..), marloweRuntime)
import Language.Marlowe.Runtime.ChainIndexer.Database (
  CommitGenesisBlock (..),
  DatabaseQueries (..),
  runGetGenesisBlock,
 )
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as ChainIndexerPostgres
import Language.Marlowe.Runtime.ChainIndexer.Genesis (computeGenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel (..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockNo (..))
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as ChainSyncPostgres
import Language.Marlowe.Runtime.Contract.Store (traceContractStore)
import Language.Marlowe.Runtime.Contract.Store.File (
  ContractStoreOptions (..),
  createContractStore,
  defaultContractStoreOptions,
 )
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as IndexerPostgreSQL
import qualified Language.Marlowe.Runtime.Indexer.Party as Party
import Language.Marlowe.Runtime.Logging (RootSelector (..), renderRootSelectorOTel)
import qualified Language.Marlowe.Runtime.Sync.Database as Sync
import qualified Language.Marlowe.Runtime.Sync.Database.PostgreSQL as SyncPostgres
import Language.Marlowe.Runtime.Transaction (mkCommandLineRoleTokenMintingPolicy)
import Network.Protocol.Driver (TcpServerDependencies (..), tcpServer)
import Network.Protocol.Driver.Trace (tcpServerTraced)
import Network.Socket (HostName, PortNumber)
import Network.TypedProtocol (unsafeIntToNat)
import Observe.Event.Backend (injectSelector)
import OpenTelemetry.Trace (
  InstrumentationLibrary (
    InstrumentationLibrary,
    libraryName,
    libraryVersion
  ),
 )
import Options.Applicative (
  Mod,
  OptionFields,
  ReadM,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  value,
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))
import Paths_marlowe_runtime (version)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO (..), liftIO, throwIO)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  run =<< getOptions

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
  (dbName, dbUser, dbHost, dbPort) <-
    either throwIO pure =<< Pool.use pool do
      connection <- ask
      liftIO $ withLibPQConnection connection \conn ->
        (,,,)
          <$> PQ.db conn
          <*> PQ.user conn
          <*> PQ.host conn
          <*> PQ.port conn
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,)
      <$> withExceptT
        (mappend "failed to read byron genesis file: " . T.pack . show)
        (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  shelleyGenesis <- either error id <$> eitherDecodeFileStrict shelleyGenesisFile
  let localNodeConnectInfo :: LocalNodeConnectInfo
      localNodeConnectInfo =
        LocalNodeConnectInfo
          { -- FIXME read from config - what is the appropriate value?
            localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
          , localNodeNetworkId = networkId
          , localNodeSocketPath = File nodeSocket
          }

      genesisBlock = computeGenesisBlock (abstractHashToBytes hash) genesisConfig shelleyGenesis

  scripts <- case ScriptRegistry.getScripts MarloweV1 of
    NESet.IsEmpty -> fail "No known marlowe scripts"
    NESet.IsNonEmpty scripts -> pure scripts

  runAppMTraced instrumentationLibrary (renderRootSelectorOTel dbName dbUser dbHost dbPort) do
    let chainIndexerDatabaseQueries = ChainIndexerPostgres.databaseQueries pool

    runGetGenesisBlock (getGenesisBlock chainIndexerDatabaseQueries) >>= \case
      Just dbGenesisBlock -> unless (dbGenesisBlock == genesisBlock) do
        liftIO $ fail "Existing genesis block does not match computed genesis block"
      Nothing -> runCommitGenesisBlock (commitGenesisBlock chainIndexerDatabaseQueries) genesisBlock

    contractStore <- traceContractStore inject <$> createContractStore ContractStoreOptions{..}

    securityParameter <-
      liftIO $
        (either (fail . show) (either (fail . show) $ pure . protocolParamSecurity) =<<) $
          runExceptT $
            queryNodeLocalState localNodeConnectInfo VolatileTip $
              QueryInEra $
                QueryInShelleyBasedEra ShelleyBasedEraConway QueryGenesisParameters

    flip runComponent_ () proc _ -> do
      MarloweRuntime{..} <-
        marloweRuntime
          -<
            MarloweRuntimeDependencies
              { marloweSyncDatabaseQueries =
                  Sync.logDatabaseQueries $
                    Sync.hoistDatabaseQueries
                      (either throwIO pure <=< liftIO . Pool.use pool)
                      SyncPostgres.databaseQueries
              , connectToLocalNode = liftIO . Cardano.connectToLocalNode localNodeConnectInfo
              , batchSize = unsafeIntToNat bufferSize
              , chainIndexerDatabaseQueries
              , chainSyncDatabaseQueries = ChainSyncPostgres.databaseQueries pool networkId
              , contractStore
              , costModel
              , genesisBlock
              , marloweIndexerDatabaseQueries = IndexerPostgreSQL.databaseQueries pool securityParameter
              , maxCost
              , marloweScriptHashes = NESet.map ScriptRegistry.marloweScript scripts
              , payoutScriptHashes = NESet.map ScriptRegistry.payoutScript scripts
              , persistRateLimit = 1
              , pollingInterval = 1
              , confirmationTimeout = 3600
              , getCurrentScripts = ScriptRegistry.getCurrentScripts
              , getScripts = ScriptRegistry.getScripts
              , submitConfirmationBlocks
              , networkId
              , runtimeVersion = version
              , indexParties = withRunInIO \runInIO ->
                  either throwIO pure =<< Pool.use pool do
                    connection <- ask
                    liftIO $ runInIO $ Party.indexParties connection
              , mkRoleTokenMintingPolicy = mkCommandLineRoleTokenMintingPolicy mintingPolicyCmd
              }

      tcpServer "marlowe-runtime"
        -<
          TcpServerDependencies
            { toPeer = marloweRuntimeServerDirectPeer
            , ..
            }

      tcpServerTraced "marlowe-runtime-traced" (injectSelector ProxyServer)
        -<
          TcpServerDependencies
            { port = portTraced
            , toPeer = marloweRuntimeServerDirectPeer
            , ..
            }

      probeServer -< ProbeServerDependencies{port = fromIntegral httpPort, ..}
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-runtime"
        , libraryVersion = T.pack $ showVersion version
        }

data Options = Options
  { databaseUri :: String
  , host :: HostName
  , port :: PortNumber
  , portTraced :: PortNumber
  , nodeSocket :: FilePath
  , networkId :: NetworkId
  , genesisConfigHash :: Text
  , genesisConfigFile :: FilePath
  , shelleyGenesisFile :: FilePath
  , costModel :: CostModel
  , maxCost :: Int
  , bufferSize :: Int
  , contractStoreDirectory :: FilePath
  , contractStoreStagingDirectory :: FilePath
  , lockingMicrosecondsBetweenRetries :: Word64
  , submitConfirmationBlocks :: BlockNo
  , httpPort :: PortNumber
  , mintingPolicyCmd :: FilePath
  , minContractAge :: NominalDiffTime
  , maxStoreSize :: Integer
  }

getOptions :: IO Options
getOptions = do
  defaultNetworkId <- value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty value <$> readSocketPath
  defaultStoreOptions <- defaultContractStoreOptions
  execParser $ info (helper <*> parser defaultNetworkId defaultSocketPath defaultStoreOptions) infoMod
  where
    parser defaultNetworkId defaultSocketPath ContractStoreOptions{..} =
      Options
        <$> databaseUriParser
        <*> hostParser
        <*> portParser
        <*> portTracedParser
        <*> socketPathParser defaultSocketPath
        <*> networkIdParser defaultNetworkId
        <*> genesisConfigHashParser
        <*> genesisConfigFileParser
        <*> shelleyGenesisFileParser
        <*> costModelParser
        <*> maxCostParser
        <*> bufferSizeParser
        <*> contractStoreDirectoryParser contractStoreDirectory
        <*> contractStoreStagingDirectoryParser contractStoreStagingDirectory
        <*> lockingMicrosecondsBetweenRetriesParser lockingMicrosecondsBetweenRetries
        <*> submitConfirmationBlocksParser
        <*> httpPortParser
        <*> mintingPolicyCmdParser
        <*> minContractAgeParser minContractAge
        <*> maxStoreSizeParser maxStoreSize

    databaseUriParser =
      strOption $
        mconcat
          [ long "database-uri"
          , short 'd'
          , metavar "DATABASE_URI"
          , help "URI of the database where the chain and contract information is saved."
          ]

    hostParser =
      strOption $
        mconcat
          [ long "host"
          , short 'h'
          , value "127.0.0.1"
          , metavar "HOST_NAME"
          , help "The host name to run the server on."
          , showDefault
          ]

    portParser =
      option auto $
        mconcat
          [ long "port"
          , short 'p'
          , value 3700
          , metavar "PORT_NUMBER"
          , help "The port number to run the server on."
          , showDefault
          ]

    portTracedParser =
      option auto $
        mconcat
          [ long "port-traced"
          , value 3701
          , metavar "PORT_NUMBER"
          , help "The port number to run the server with tracing on."
          , showDefault
          ]

    socketPathParser defaultSocketPath = strOption options
      where
        options :: Mod OptionFields FilePath
        options =
          mconcat
            [ long "socket-path"
            , short 's'
            , metavar "SOCKET_FILE"
            , defaultSocketPath
            , help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable."
            ]

    shelleyGenesisFileParser = strOption options
      where
        options :: Mod OptionFields FilePath
        options =
          mconcat
            [ long "shelley-genesis-config-file"
            , metavar "CONFIG_FILE"
            , help "Path to the Shelley Genesis Config JSON File."
            ]

    genesisConfigFileParser = strOption options
      where
        options :: Mod OptionFields FilePath
        options =
          mconcat
            [ long "genesis-config-file"
            , metavar "CONFIG_FILE"
            , help "Path to the Byron Genesis Config JSON File."
            ]

    genesisConfigHashParser = strOption options
      where
        options :: Mod OptionFields Text
        options =
          mconcat
            [ long "genesis-config-file-hash"
            , metavar "CONFIG_HASH"
            , help "Hash of the Byron Genesis Config JSON file."
            ]

    networkIdParser defaultNetworkId = option parse options
      where
        parse :: ReadM NetworkId
        parse = Testnet . NetworkMagic . toEnum <$> auto

        options :: Mod OptionFields NetworkId
        options =
          mconcat
            [ long "testnet-magic"
            , short 'm'
            , metavar "INTEGER"
            , defaultNetworkId
            , help "Testnet network ID magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable."
            ]

    costModelParser = CostModel <$> blockCostParser <*> txCostParser

    blockCostParser =
      option auto $
        mconcat
          [ long "block-cost"
          , value 1
          , metavar "COST_UNITS"
          , help "The number of cost units to associate with persisting a block when computing the cost model."
          , showDefault
          ]

    txCostParser =
      option auto $
        mconcat
          [ long "tx-cost"
          , value 10
          , metavar "COST_UNITS"
          , help "The number of cost units to associate with persisting a transaction when computing the cost model."
          , showDefault
          ]

    maxCostParser =
      option auto $
        mconcat
          [ long "max-cost"
          , value 100_000
          , metavar "COST_UNITS"
          , help
              "The maximum number of cost units that can be batched when persisting blocks. If the cost of the current batch would exceed this value, the chain sync client will wait until the current batch is persisted before requesting another block."
          , showDefault
          ]

    bufferSizeParser =
      option readOption $
        mconcat
          [ long "contract-buffer-size"
          , short 'b'
          , value 512
          , metavar "INTEGER"
          , help "The number of contracts to accept from the client before flushing to disk."
          , showDefault
          ]
      where
        readOption = do
          i <- auto
          when (i <= 0) do
            fail "Positive batch size required"
          pure i

    contractStoreDirectoryParser defaultValue =
      strOption $
        mconcat
          [ long "store-dir"
          , short 's'
          , value defaultValue
          , metavar "DIR"
          , help "The root directory of the contract store"
          , showDefault
          ]

    contractStoreStagingDirectoryParser defaultValue =
      strOption $
        mconcat
          [ long "store-staging-dir"
          , value defaultValue
          , metavar "DIR"
          , help "The root directory of the contract store staging areas"
          , showDefault
          ]

    lockingMicrosecondsBetweenRetriesParser defaultValue =
      option auto $
        mconcat
          [ long "store-lock-microseconds-between-retries"
          , value defaultValue
          , metavar "MICRO_SECONDS"
          , help "The number of microseconds to wait between retries when acquiring the store lock"
          , showDefault
          ]

    submitConfirmationBlocksParser =
      option (BlockNo <$> auto) $
        mconcat
          [ long "submit-confirmation-blocks"
          , value 0
          , metavar "INTEGER"
          , help
              "The number of blocks after a transaction has been confirmed to wait before displaying the block in which was confirmed."
          , showDefault
          ]

    httpPortParser =
      option auto $
        mconcat
          [ long "http-port"
          , metavar "PORT_NUMBER"
          , help "Port number to serve the http healthcheck API on"
          , value 8080
          , showDefault
          ]

    mintingPolicyCmdParser =
      strOption $
        mconcat
          [ long "minting-policy-cmd"
          , metavar "CMD"
          , help
              "A command which creates the role token minting policy for a contract. It should read the arguments via the command line and output the serialized script binary to stdout."
          ]

    minContractAgeParser def =
      option auto $
        mconcat
          [ long "min-contract-age"
          , metavar "MINUTES"
          , help "The minimum age contracts in the store must reach before they can be garbage collected."
          , value def
          , showDefault
          ]

    maxStoreSizeParser def =
      option auto $
        mconcat
          [ long "max-store-size"
          , metavar "BYTES"
          , help "The maximum allowed size of the contract store, in bytes."
          , value def
          , showDefault
          ]

    infoMod =
      mconcat
        [ fullDesc
        , progDesc "Contract synchronization and query service for Marlowe Runtime"
        , header "marlowe-sync : a contract synchronization and query service for the Marlowe Runtime."
        ]

    readNetworkId = do
      magicString <- lookupEnv "CARDANO_TESTNET_MAGIC"
      pure $ Testnet . NetworkMagic <$> (readMaybe =<< magicString)

    readSocketPath = do
      pathString <- lookupEnv "CARDANO_NODE_SOCKET_PATH"
      pure case pathString of
        Just "" -> Nothing
        _ -> pathString
