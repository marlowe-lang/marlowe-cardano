module Main where

import Log (LogLevel (..), runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Backend.StandardOutputInlined (withStdOutInlinedLogger)
import Control.Concurrent.Component (runComponent_)
import Control.Monad (join, when)
import Data.Version (showVersion)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Marlowe.Indexer (IndexerDependencies (..), StartPoint (..), mkIndexer)
import Marlowe.Indexer.NodeFollower (MemoryCostConfig (..), ChangesMemoryCostModel (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  strOption, ReadM, asum, flag', eitherReader, value, (<|>), showDefault,
 )
import Paths_marlowe_indexer (version)
import Cardano.Api qualified as C
import Language.Marlowe.Runtime.History.Api (getRegistryMarloweScriptHashes)
import Language.Marlowe.Runtime.Core.ScriptRegistry (ScriptRegistry, loadDefaultScriptRegistry, loadScriptRegistry)
import System.Environment.Blank (getEnv)
import qualified Hasql.Connection.Settings as Hasql
import qualified Hasql.Pool.Config as Hasql
import qualified Data.Text as T
import qualified Text.Read as T
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable (Foldable(..))
import Log.Class (logInfo_, logInfo)
import System.Exit (die)

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
  , nodeSocketPath :: FilePath
  , networkId :: C.NetworkId
  , scriptRegistry :: Maybe FilePath
  , startPoint :: StartPoint
  }
longOption :: ReadM a -> String -> String -> String -> Parser a
longOption reader longText helpText metavarText =
  option reader (long longText <> help helpText <> metavar metavarText)

databaseUriParser :: Parser Hasql.Settings
databaseUriParser = do
  let
    readSettings = eitherReader \s -> do
      let
        settings = Hasql.connectionString (T.pack s)
      when (settings == mempty) $ Left ("Invalid database URI" :: String)
      pure settings

  longOption
    readSettings
    "database-uri"
    "URI of the claim processing database"
    "DATABASE_URI"

logLevelParser :: Parser LogLevel
logLevelParser =
  asum
    [ flag' LogTrace $
        fold
          [ long "verbose"
          , help "Enable trace-level logging (sets the backend log level to Trace)"
          ]
    , flag' LogAttention $
        fold
          [ long "quiet"
          , help "Only emit attention-level logs"
          ]
    , pure LogInfo
    ]

startPointParser :: Parser StartPoint
startPointParser =
  asum
    [ flag' StartFromGenesis $
        fold
          [ long "start-from-genesis"
          , help "Start indexing from the genesis block."
          ]
    , StartFromBlock <$> startFromSlotParser <*> startFromBlockParser
    , pure StartFromDbOrTip
    ]

startFromSlotParser :: Parser C.SlotNo
startFromSlotParser =
  option (C.SlotNo <$> auto) $
    fold
      [ long "start-from-slot"
      , metavar "SLOT"
      , help "Slot number of the block the indexer should start from. Must be used together with --start-from-block."
      ]

startFromBlockParser :: Parser (C.Hash C.BlockHeader)
startFromBlockParser =
  option readBlockHeaderHash $
    fold
      [ long "start-from-block"
      , metavar "HEX_HASH"
      , help "Hex-encoded block header hash of the block the indexer should start from. Must be used together with --start-from-slot."
      ]

readBlockHeaderHash :: ReadM (C.Hash C.BlockHeader)
readBlockHeaderHash =
  eitherReader \raw -> do
    case C.deserialiseFromRawBytesHex @(C.Hash C.BlockHeader) (BSC.pack raw) of
      Left err -> Left $ show err
      Right hash -> Right hash

getEnvNetworkId :: IO (Maybe C.NetworkId)
getEnvNetworkId =
  fmap (C.Testnet . C.NetworkMagic)
    . (T.readMaybe =<<)
    <$> getEnv "CARDANO_NODE_NETWORK_ID"

mkNetworkIdParser :: IO (Parser C.NetworkId)
mkNetworkIdParser = do
  possibleNodeNetworkId <- getEnvNetworkId
  pure do
    let
      networkMod = maybe mempty value possibleNodeNetworkId
      mainnetParser = flag' C.Mainnet (long "mainnet" <> help "Execute on mainnet.")
      testnetParser = option
          (C.Testnet . C.NetworkMagic . toEnum <$> auto)
          ( long "testnet-magic"
              <> metavar "INTEGER"
              <> networkMod
              <> help "Network magic. Defaults to the CARDANO_NODE_NETWORK_ID environment variable's value."
          )
    mainnetParser <|> testnetParser

mkNodeSocketParser :: IO (Parser FilePath)
mkNodeSocketParser = do
  possibleNodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
  pure $ strOption do
    let
      opt = long "socket-path"
        <> short 's'
        <> showDefault
        <> help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
    case possibleNodeSocketPath of
      Just path -> opt <> value path
      Nothing -> opt

loadRegistry :: Maybe FilePath -> IO ScriptRegistry
loadRegistry file = do
  result <- maybe loadDefaultScriptRegistry loadScriptRegistry file
  either (die . show) pure result

runIndexer :: Options -> IO ()
runIndexer Options{..} = do
  pool <- do
    let
      cfg = Hasql.settings [ Hasql.staticConnectionSettings databaseUri ]
    Pool.acquire cfg
  registry <- loadRegistry scriptRegistry
  let
    localNodeConnectInfo = C.LocalNodeConnectInfo
      { C.localConsensusModeParams = C.CardanoModeParams $ C.EpochSlots 21_600
      , C.localNodeNetworkId = networkId
      , C.localNodeSocketPath = C.File nodeSocketPath
      }
    dbQueries = PostgreSQL.databaseQueries pool
    marloweScriptHashes = getRegistryMarloweScriptHashes registry
    memoryCostConfig = MemoryCostConfig
      { maxMemoryCost = 100_000
      , changesMemoryCostModel = ChangesMemoryCostModel
          { blockMemoryCost = 100
          , txMemoryCost = 1
          }
      }
    prettyLog = True
    withLogger = if prettyLog then withStdOutLogger else withStdOutInlinedLogger
  withLogger \logger ->
    runLogT "marlowe-indexer" logger logLevel do

    logInfo_ "Starting Marlowe Indexer:"
    logInfo "Script hashes:" marloweScriptHashes
    logInfo_ $ "Start point: " <> T.pack (show startPoint)

    runComponent_ $ mkIndexer IndexerDependencies
      { localNodeConnectInfo
      , databaseQueries = dbQueries
      , memoryCostConfig = memoryCostConfig
      , marloweScriptHashes = marloweScriptHashes
      , scriptRegistry = registry
      , startPoint
      }

mkParser :: IO (Parser (IO ()))
mkParser = do
  nodeSocketPathParser <- mkNodeSocketParser
  networkIdParser <- mkNetworkIdParser
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser
      <*> nodeSocketPathParser
      <*> networkIdParser
      <*> optional (strOption (long "script-registry" <> metavar "FILE" <> help "Script registry JSON file."))
      <*> startPointParser

    versionOption =
      infoOption ("marlowe-indexer " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

  pure $ helper <*> versionOption <*> (runIndexer <$> parserOptions)

main :: IO ()
main = do
  parser <- mkParser
  let
    parserInfo :: ParserInfo (IO ())
    parserInfo = do
      let
        description =
          fullDesc
            <> progDesc "Marlowe Runtime Indexer - indexes Marlowe contracts from the blockchain."
            <> header "marlowe-indexer"
      info parser description
  join $ execParser parserInfo
