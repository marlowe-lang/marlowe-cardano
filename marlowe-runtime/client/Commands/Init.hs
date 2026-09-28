module Commands.Init where

import Cardano.Api qualified as C
import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.Address qualified as Addr
import Commands.Options.CardanoNode (mkNetworkIdParser, mkNodeSocketParser)
import Commands.Options.MessageFormat (MessageFormat (MessageFormatText), messageFormatParser, emitError, emitJSONError, emitResponseWith)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Client (postContract)
import Language.Marlowe.Runtime.Web.Contract.API (PostContractsRequest(PostContractsRequest, accounts, contract, metadata, minUTxODeposit, roles, tags, threadTokenName, version), ContractOrSourceId(ContractOrSourceId))
import Language.Marlowe.Runtime.Web.Contract.Source.API (ContractSourceId)
import Language.Marlowe.Runtime.Web.Core.Address qualified as Web
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (V1))
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Language.Marlowe.Runtime.Web.Tx.API (CreateTxEnvelope, CardanoTx)
import Options.Applicative ( Parser, ParserInfo, help, info, long, metavar, optional, progDesc, short, showDefault, strOption, switch, value)
import Servant.Client (ClientError)
import System.FilePath ((</>))
import Language.Marlowe.Runtime.Web.Contract.Source.API (contractSourceIdFromText)

fundingAddressParser :: Parser (C.Address C.ShelleyAddr)
fundingAddressParser = Addr.mkAddressParser $ Addr.AddressParserConfig
  { Addr.long = "funding-wallet-address"
  , Addr.short = 'w'
  , Addr.help = "The address which pays for the initial contract deployment transaction."
  }

data InitCommand = InitCommand
  { contractFile :: Maybe FilePath
  , contractSourceId :: Maybe T.Text
  , develScripts :: Bool
  , fundingAddress :: C.Address C.ShelleyAddr
  , messageFormat :: MessageFormat
  , networkId :: C.NetworkId
  , nodeSocketPath :: FilePath
  , outputDir :: FilePath
  , servantClientRunner :: ServantClientRunner
  }

decodeFileStrict :: (A.FromJSON a, MonadIO m) => MessageFormat -> FilePath -> m a
decodeFileStrict msgFormat filePath = do
  result <- liftIO $ Yaml.decodeFileEither filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the contract file. Details: " <> Yaml.prettyPrintParseException err
    Right v -> pure v

mkInitCommandParser :: IO (ParserInfo InitCommand)
mkInitCommandParser = do
  networkIdParser <- mkNetworkIdParser
  socketPathParser <- mkNodeSocketParser
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Build initial marlowe transaction"
    cmd = InitCommand
      <$> optional (strOption do
        long "contract-file"
          <> metavar "CONTRACT_FILE"
          <> help "JSON input file for the contract.")
      <*> optional (strOption do
        long "contract-source-id"
          <> metavar "CONTRACT_SOURCE_ID"
          <> help "Hex-encoded identifier of a previously uploaded Marlowe contract source. The runtime will deploy the contract from the store by id, not from an inline file.")
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
      <*> servantClientRunnerParser
  pure $ info cmd desc

runInitCommand :: InitCommand -> IO ()
runInitCommand cmd = do
  case cmd.messageFormat of
    MessageFormatText -> putStrLn $ "Creating initial Marlowe transaction to: " <> show cmd.outputDir <> "."
    _ -> pure ()

  let
    connectInfo =
      C.LocalNodeConnectInfo
        (C.CardanoModeParams $ C.EpochSlots 21600)
        cmd.networkId
        (C.File cmd.nodeSocketPath)
  rawQueryResult <- do
    let
      query = C.queryUtxo C.ShelleyBasedEraConway (C.QueryUTxOByAddress $ Set.fromList [C.AddressShelley cmd.fundingAddress])
    C.executeLocalStateQueryExpr
      connectInfo
      C.VolatileTip
      query

  walletUtxos <- case rawQueryResult of
    Right (Right (Right walletUtxos@(C.UTxO walletUtxosMap))) -> do
      when (null walletUtxosMap) $
        emitError cmd.messageFormat $ "The funding address " <> show cmd.fundingAddress <> " has no UTXOs. Please provide an address with at least one UTXO."
      pure walletUtxos
    _ -> do
      emitError cmd.messageFormat ("Failed to query the cardano-node for necessary information." :: String)

  let
    errBoth :: String -> IO (Either Semantics.Contract ContractSourceId)
    errBoth msg = do
      emitError cmd.messageFormat msg
      pure (Left (error "unreachable" :: Semantics.Contract))
  contractRef <- case (cmd.contractFile :: Maybe FilePath, cmd.contractSourceId :: Maybe T.Text) of
    (Just _, Just _) -> errBoth "Please specify exactly one of --contract-file or --contract-source-id."
    (Just f, Nothing) -> Left <$> decodeFileStrict cmd.messageFormat f
    (Nothing, Just sidStr) -> case contractSourceIdFromText sidStr of
      Just cid -> pure $ Right cid
      Nothing -> errBoth $ "Invalid contract source id: " <> T.unpack sidStr <> ". It must be a 32-byte hex-encoded string."
    (Nothing, Nothing) -> errBoth "Please specify either --contract-file or --contract-source-id."
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
    stakeCredential = Nothing
    request = PostContractsRequest
      { tags = mempty
      , metadata = mempty
      , version = V1
      , roles = Nothing
      , threadTokenName = Nothing
      , contract = ContractOrSourceId contractRef
      , accounts = mempty
      , minUTxODeposit = Nothing
      }
    changeAddress = Web.Address $ C.serialiseToBech32 cmd.fundingAddress
    availableUTxOs = Web.fromCardanoUTxO walletUtxos
  (result :: Either ClientError (CreateTxEnvelope CardanoTx)) <- runWebClient $ postContract stakeCredential changeAddress availableUTxOs request
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right createTxEnvelope -> do
      let
        outputFile = cmd.outputDir </> "init.tx.json"
      LBS8.writeFile outputFile (A.encodePretty $ A.toJSON createTxEnvelope)
      emitResponseWith
        cmd.messageFormat
        createTxEnvelope
        \_ -> do
          let
            outputFile' = LBS.fromStrict . T.encodeUtf8 . T.pack $ outputFile
          "Initial transaction created successfully and written to " <> outputFile' <> "."

