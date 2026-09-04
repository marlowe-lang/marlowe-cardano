module Commands.Init where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Yaml qualified as Yaml
import qualified Marlowe.Plutus.Binaries.Devel as Devel
import qualified Marlowe.Plutus.Binaries.Production as Production
import Options.Applicative (
  Parser,
  ParserInfo,
  ReadM,
  eitherReader,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  switch,
  value, flag', Alternative ((<|>)), auto,
 )
import qualified Text.Read as T
import qualified Cardano.Api as C
import System.Environment.Blank (getEnv)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1), emptyMarloweTransactionMetadata, MarloweVersionTag(V1))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Marlowe.Runtime.ChainSync.Api
    ( TokenName(TokenName),
      fromCardanoShelleyAddress,
      UTxOs(utxosMap) )
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.V3 as PV3
import Language.Marlowe.Runtime.Plutus.V3.Api (toPlutusTxOutRef)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext(WalletContext), MintingSeed(MintingSeed))
import Control.Monad (when)
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Marlowe.Plutus.RoleTokens (mkRoleTokens)
import Data.Bifunctor (first)
import Data.Foldable (Foldable(fold))
import Data.String (IsString(fromString))
import qualified Data.Set as Set
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import System.Exit (die)
import Language.Marlowe.Runtime.Transaction.Api (LoadHelpersContextError(LoadHelpersContextErrorNotFound), RoleTokensConfig(RoleTokensNone), ContractInitialized(ContractInitialized), ContractInitializedInEra(txBody, contractId))
import Language.Marlowe.Runtime.Transaction.Builders (execInit)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoUTxO, fromPlutusSerialisedScript)
import Log (LogLevel(LogTrace), runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)

data MessageFormat = MessageFormatText | MessageFormatJson | MessageFormatYaml
  deriving (Eq)

instance Show MessageFormat where
  show = \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

instance A.ToJSON MessageFormat where
  toJSON = A.String . \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

messageFormatParser :: Parser MessageFormat
messageFormatParser = do
  let
    readMessageFormat :: ReadM MessageFormat
    readMessageFormat = eitherReader $ \case
      "text" -> Right MessageFormatText
      "json" -> Right MessageFormatJson
      "yaml" -> Right MessageFormatYaml
      other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

readAddress :: ReadM (C.Address C.ShelleyAddr)
readAddress =
  eitherReader $
    first show . C.deserialiseFromBech32 . fromString

fundingAddressParser :: Parser (C.Address C.ShelleyAddr)
fundingAddressParser =
  option readAddress $
    fold
      [ long "funding-wallet-address"
      , help "The address which pays for claim transactions."
      , metavar "BECH32"
      ]

data InitCommand = InitCommand
  { contractFile :: FilePath
  , develScripts :: Bool
  , fundingAddress :: C.Address C.ShelleyAddr
  , messageFormat :: MessageFormat
  , networkId :: C.NetworkId
  , nodeSocketPath :: FilePath
  , outputDir :: FilePath
  }

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

getEnvNetworkId :: IO (Maybe C.NetworkId)
getEnvNetworkId =
  fmap (C.Testnet . C.NetworkMagic)
    . (T.readMaybe =<<)
    <$> getEnv "CARDANO_NODE_NETWORK_ID"

-- | Read the CARDANO_NODE_SOCKET_PATH environment variable for the default node socket path.
getEnvNodeSocketPath :: IO (Maybe FilePath)
getEnvNodeSocketPath = getEnv "CARDANO_NODE_SOCKET_PATH"

decodeFileStrict :: (A.FromJSON a, MonadIO m) => MessageFormat -> FilePath -> m a
decodeFileStrict msgFormat filePath = do
  result <- liftIO $ Yaml.decodeFileEither filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the contract file. Details: " <> Yaml.prettyPrintParseException err
    Right v -> pure v

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

mkInitCommandParser :: IO (ParserInfo InitCommand)
mkInitCommandParser = do
  networkIdParser <- mkNetworkIdParser
  socketPathParser <- mkNodeSocketParser
  let
    desc = progDesc "Build initial marlowe transaction"
    cmd = InitCommand
      <$> strOption do
        long "contract-file"
          <> metavar "CONTRACT_FILE"
          <> help "JSON input file for the contract."
      <*> switch do
        long "devel-scripts"
          <> help "Compile the devel script variants with tracing preserved."
      <*> fundingAddressParser
      <*> messageFormatParser
      <*> networkIdParser
      <*> socketPathParser
      <*> strOption do
        long "output-dir"
          <> short 'o'
          <> value "out"
          <> showDefault
          <> help "Directory where transaction file will be written."
  pure $ info cmd desc

runInitCommand :: InitCommand -> IO ()
runInitCommand cmd = do
  case cmd.messageFormat of
    MessageFormatText -> putStrLn $ "Creating initial Marlowe transaction to: " <> show cmd.outputDir <> "."
    _ -> pure ()

  let
    mkRoleTokensPolicy (MintingSeed txOutRef) tokens = do
      let
        mkPolicy = if cmd.develScripts then Devel.mkRoleTokensPolicyBytes else Production.mkRoleTokensPolicyBytes
        roleTokens = mkRoleTokens $ Map.toList tokens <&> \(TokenName bs, amount) -> do
          (PV3.TokenName . PV3.toBuiltin $ bs, amount)
      pure . fromPlutusSerialisedScript C.PlutusScriptV3 . mkPolicy roleTokens $ toPlutusTxOutRef txOutRef
    connectInfo =
      C.LocalNodeConnectInfo
        (C.CardanoModeParams $ C.EpochSlots 21600)
        cmd.networkId
        (C.File cmd.nodeSocketPath)
  rawQueryResult <- C.executeLocalStateQueryExpr connectInfo C.VolatileTip $
      (,,,)
        <$> C.querySystemStart
        <*> C.queryEraHistory
        <*> C.queryProtocolParameters C.ShelleyBasedEraConway
        <*> C.queryUtxo C.ShelleyBasedEraConway (C.QueryUTxOByAddress $ Set.fromList [C.AddressShelley cmd.fundingAddress])

  (systemStart, eraHistory, protocolParams, walletUtxos) <- case rawQueryResult of
    Right (Right systemStart, Right eraHistory, Right (Right protocolParams), Right (Right walletUtxos@(C.UTxO walletUtxosMap))) -> do
      when (null walletUtxosMap) $
        emitError cmd.messageFormat $ "The funding address " <> show cmd.fundingAddress <> " has no UTXOs. Please provide an address with at least one UTXO."
      walletUtxos' <- case fromCardanoUTxO walletUtxos of
        Nothing ->
          emitError
            cmd.messageFormat
            "Failed to convert UTXOs from the cardano-node into the format expected by the Marlowe Runtime. Please ensure the provided address is valid and has at least one UTXO."
        Just ws -> pure ws
      pure (systemStart, eraHistory, protocolParams, walletUtxos')
    _ -> do
      emitError cmd.messageFormat "Failed to query the cardano-node for necessary information."

  let
    solveConstraints = Constraints.solveConstraints systemStart (C.toLedgerEpochInfo eraHistory)
    walletContext = WalletContext
      walletUtxos
      (Set.fromList . Map.keys $ walletUtxos.utxosMap)
      (fromCardanoShelleyAddress cmd.fundingAddress)

    -- FIXME: Accept the below options on the CLI level:
    loadHelpersContext _ _ = pure $ Left LoadHelpersContextErrorNotFound
    stakeCredential = Nothing
    threadTokenName = Nothing
    roleTokensConfig = RoleTokensNone
    optMinAda = Nothing
    accounts = mempty
    analysisTimeout = 60
    transactionMetadata = emptyMarloweTransactionMetadata

  contract <- decodeFileStrict cmd.messageFormat cmd.contractFile
  result <- withStdOutLogger \logger -> do
    runLogT "marlowe-transactions-cli" logger LogTrace $
      execInit
        mkRoleTokensPolicy
        C.ConwayEra
        (\_ -> pure Nothing)
        ScriptRegistry.getCurrentScripts
        solveConstraints
        protocolParams
        walletContext
        loadHelpersContext
        cmd.networkId
        stakeCredential
        MarloweV1
        threadTokenName
        roleTokensConfig
        transactionMetadata
        optMinAda
        accounts
        (Left contract)
        analysisTimeout
  case result of
    Left err -> emitError cmd.messageFormat $ "Failed to create initial Marlowe transaction. Details: " <> show err
    Right (ContractInitialized era initializedInEra) -> C.babbageEraOnwardsConstraints era do
      -- Write the transaction to the output directory
      let
        txBodyFile = cmd.outputDir <> "/init.tx.json"
      C.writeFileTextEnvelope
        (C.File txBodyFile) Nothing
        initializedInEra.txBody >>= \case
          Left err -> emitError cmd.messageFormat $ "Failed to write the transaction body to a file. Details: " <> show err
          Right () -> emitSummary cmd.messageFormat txBodyFile initializedInEra

emitError :: MessageFormat -> String -> IO a
emitError messageFormat err =
  case messageFormat of
    MessageFormatText -> die err
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "compile command failed"
    MessageFormatYaml -> BS8.putStrLn (Yaml.encode err) >> die "compile command failed"

emitSummary :: MessageFormat -> FilePath -> ContractInitializedInEra era 'V1 -> IO ()
emitSummary messageFormat txBodyFile initialized = do
  let
    json =
      A.object
        [ "contractId" A..= initialized.contractId
        , "transactionBodyFile" A..= txBodyFile
        ]
  case messageFormat of
    MessageFormatText -> do
      let
        summary =
          "Contract initialized successfully.\n"
            <> "Contract ID: " <> show initialized.contractId <> "\n"
            <> "Transaction body written to: " <> show txBodyFile
      putStrLn summary
    MessageFormatJson ->
      LBS8.putStrLn $ A.encodePretty json
    MessageFormatYaml ->
      BS8.putStrLn $ Yaml.encode json

