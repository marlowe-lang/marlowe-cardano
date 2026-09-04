{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- We "return" lambdas for clarity in some helpers.
{- HLINT ignore "Redundant lambda" -}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.Monad.Error (except)
import Cardano.Ledger.Core qualified as L
import Control.Applicative (Alternative((<|>)))
import Control.Error (throwE, note)
import Control.Exception (throwIO, Exception, SomeException, try)
import Control.Exception.Lifted qualified as Exception.Lifted
import Control.Monad (join, when, (<=<))
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Aeson.Key qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.CaseInsensitive qualified as CI
import Data.Data (type (:~:)(Refl))
import Data.Foldable (Foldable(..), find)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Type.Equality (TestEquality(testEquality))
import Data.Version (showVersion)
import Data.Yaml qualified as Yaml
import Hasql.Connection.Settings qualified as Hasql
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Hasql
import Language.Marlowe.CLI.Types (AUTxO(AUTxO), MessageFormat(MessageFormatText, MessageFormatJson, MessageFormatYaml))
import Language.Marlowe.Runtime.Cardano.Api (fromPlutusSerialisedScript, fromCardanoAddressInEra, toCardanoScriptHash, fromCardanoTxIn, fromCardanoTxOutCtxUTxO)
import Language.Marlowe.Runtime.ChainSync.Api (paymentCredential, fromCardanoScriptHash, SlotNo(SlotNo), NodeTip(NodeTip), ChainTip(ChainTip), BlockHeader(BlockHeader))
import Language.Marlowe.Runtime.ChainSync.Api qualified as Core
-- import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..)) -- removed for now
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1))
import qualified Marlowe.Plutus.Semantics as V1
import Language.Marlowe.Runtime.Core.Api qualified as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry ( MarloweScripts(MarloweScripts, marloweScript, payoutScript, marloweScriptUTxOs, payoutScriptUTxOs), GetAllScripts(GetAllScripts), ReferenceScriptUtxo(ReferenceScriptUtxo, txOutRef, script, txOut), fromCardanoScriptInAnyLang, ScriptInPlutus, GetCurrentScripts(GetCurrentScripts) )
import Language.Marlowe.Runtime.Core.ScriptRegistry qualified as ScriptRegistry
import Language.Marlowe.Runtime.Web.Core.Object.Schema ()
import Language.Marlowe.Runtime.Plutus.V3.Api (toPlutusTxOutRef)
import Language.Marlowe.Runtime.Query (SomeContractState(SomeContractState), ContractState (ContractState, initialOutput, latestOutput))
import Language.Marlowe.Runtime.Query.Database ( hoistDatabaseQueries, logDatabaseQueries, DatabaseQueries(getContractState, getEraHistory), getNodeTip, GetEraHistoryError )
import Language.Marlowe.Runtime.Query.Database.PostgreSQL (databaseQueries)
import Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetContractState (GetContractState)
import Language.Marlowe.Runtime.Transaction.Api (LoadHelpersContextError, RoleTokensConfig, InitError(InitEraHistoryNotInitialized), ApplyInputsError(ApplyInputsEraHistoryNotInitialized))
import Language.Marlowe.Runtime.Transaction.Api qualified as T
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Transaction.Builders (execInit, execApplyInputs, LoadMarloweContext)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(MarloweContext), MintingSeed(MintingSeed))
import Language.Marlowe.Runtime.Transaction.Constraints qualified as Constraints
import Language.Marlowe.Runtime.Web.Server (runServer, runServerMExtract, serverWithOpenApi, ServerDependencies(..), RuntimeAPIWithOpenAPI)
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, InitContract, ApplyInputs, GetContractSource, WithBundleImporter)
import Log (LogLevel (..), runLogT, Logger, logAttention, LogT, MonadLog, logInfo)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Backend.StandardOutputInlined (withStdOutInlinedLogger)
import Marlowe.Plutus.Binaries.Devel qualified as Devel
import Marlowe.Plutus.Binaries.Production qualified as Production
import Marlowe.Plutus.RoleTokens (mkRoleTokens)
import Marlowe.Runtime.Server.Contrib.Servant.Err500 (err500, err500JSON)
import Marlowe.Runtime.Server.Contrib.Servant.ResponseRewriterMiddleware as ResponseRewriterMiddleware
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Logger qualified as Wai
import Network.Wai.Middleware.Cors ( CorsResourcePolicy (corsRequestHeaders), cors, simpleCorsResourcePolicy,)
import Options.Applicative ( Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, infoOption, long, metavar, option, progDesc, short, ReadM, asum, flag', eitherReader, strOption, auto, showDefault, value, optional)
import Paths_marlowe_runtime (version)
import PlutusLedgerApi.V3 qualified as PV3
import qualified Language.Marlowe.Runtime.Contract.Store as ContractStore
import qualified Language.Marlowe.Runtime.Contract.Store.Memory as StoreMemory
import qualified UnliftIO.STM
import UnliftIO (bracket)
import Servant ( Application, ServerError (..), hoistServer, serveWithContext, Handler (Handler), ErrorFormatter, ErrorFormatters, bodyParserErrorFormatter, urlParseErrorFormatter, headerParseErrorFormatter, defaultErrorFormatters, err400, Context(EmptyContext, (:.)))
import Servant.Server.Internal.ServerError (responseServerError)
import System.Environment.Blank (getEnv)
import System.Exit (die)
import Text.Read qualified as T
import Language.Marlowe.Runtime.Contract.TransferServer qualified as TransferServer
import Servant.Pipes ()
import Language.Marlowe.Runtime.Web.Contract.Source.Server (fromSourceId, toSourceId)
import qualified Language.Marlowe.Runtime.Contract.Store as Store

newtype Port = Port Int

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
  , nodeSocketPath :: FilePath
  , networkId :: C.NetworkId
  , port :: Port
  , scriptRegistryFile :: Maybe FilePath
  }

decodeFileStrict
  :: A.FromJSON a
  => MonadIO m
  => MessageFormat
  -> FilePath
  -> m a
decodeFileStrict msgFormat filePath = do
  result <- liftIO $ A.eitherDecodeFileStrict' filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the publishing info file. Details: " <> err
    Right v -> pure v

emitError :: MessageFormat -> String -> IO a
emitError messageFormat err =
  case messageFormat of
    MessageFormatText -> die err
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "compile command failed"
    MessageFormatYaml -> BS8.putStrLn (Yaml.encode err) >> die "compile command failed"

longOption :: ReadM a -> String -> String -> String -> Parser a
longOption reader longText helpText metavarText =
  option reader (long longText <> help helpText <> metavar metavarText)

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

portParser :: Parser Port
portParser = option
  (Port <$> auto)
  ( long "port"
    <> short 'p'
    <> help "The port to serve the server on"
    <> metavar "PORT"
    <> value (Port 8090)
  )

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
          , help "Enable trace-level logging"
          ]
    , flag' LogAttention $
        fold
          [ long "quiet"
          , help "Only emit attention-level logs"
          ]
    , pure LogInfo
    ]

newtype UseRoleTokenDevelScript = UseRoleTokenDevelScript Bool

mkRoleTokensPolicy :: Applicative m => UseRoleTokenDevelScript -> MkRoleTokenMintingPolicy m
mkRoleTokensPolicy (UseRoleTokenDevelScript useRoleTokenDevelScript) (MintingSeed txOutRef) tokens = do
  let
    mkPolicy = if useRoleTokenDevelScript then Devel.mkRoleTokensPolicyBytes else Production.mkRoleTokensPolicyBytes
    roleTokens = mkRoleTokens $ Map.toList tokens <&> \(Core.TokenName bs, amount) -> do
      (PV3.TokenName . PV3.toBuiltin $ bs, amount)
  pure . fromPlutusSerialisedScript C.PlutusScriptV3 . mkPolicy roleTokens $ toPlutusTxOutRef txOutRef

emptyLoadHelpersContext
  :: forall m v
   . Monad m
  => MarloweVersion v
  -> Either (Core.PolicyId, RoleTokensConfig) (Maybe Core.ContractId)
  -> m (Either LoadHelpersContextError Constraints.HelpersContext)
emptyLoadHelpersContext _version = do
  let
    emptyContext = Right $ Constraints.HelpersContext
      { currentHelperScripts = Map.empty
      , helperPolicyId = ""
      , helperScriptStates = Map.empty
      }
  -- FIXME: This is plain wrong and will fail at some point
  \case
    Left (_policyId, _roleTokens) -> pure emptyContext
    Right _contractId -> pure emptyContext

mkInitContract
  :: MonadUnliftIO m
  => MonadLog m
  => LedgerInfo
  -> m (Either GetEraHistoryError C.EraHistory)
  -- ^ Fetch a fresh era history for each request. Failing this fetch is a
  -- hard error: we refuse to fall back to a stale value because that would
  -- silently mask configuration drift between the indexer and the runtime.
  -> GetCurrentScripts
  -> GetContractSource m
  -> UseRoleTokenDevelScript
  -> InitContract m
mkInitContract (networkId, systemStart, protocolParams) fetchEraHistory getCurrentScripts getContractSource useRoleTokenDevelScript =
  \stakeCredential walletContext threadTokenName roleTokensConfig transactionMetadata optMinAda accounts contract -> do
    eraHistory <- fetchEraHistory
    case eraHistory of
      Left _ -> pure $ Left InitEraHistoryNotInitialized
      Right eh -> do
        let
          solveConstraints = Constraints.solveConstraints systemStart (C.toLedgerEpochInfo eh)
          analysisTimeout = 60
        execInit
          (mkRoleTokensPolicy useRoleTokenDevelScript)
          C.ConwayEra
          (getContractSource . toSourceId)
          getCurrentScripts
          solveConstraints
          protocolParams
          walletContext
          emptyLoadHelpersContext
          networkId
          stakeCredential
          MarloweV1
          threadTokenName
          roleTokensConfig
          transactionMetadata
          optMinAda
          accounts
          contract
          analysisTimeout

mkApplyInputs
  :: forall m
   . MonadUnliftIO m
  => MonadLog m
  => LedgerInfo
  -> m (Either GetEraHistoryError C.EraHistory)
  -- ^ Fetch a fresh era history for each request. Failing this fetch is a
  -- hard error: we refuse to fall back to a stale value because that would
  -- silently mask configuration drift between the indexer and the runtime.
  -> GetContractState m
  -> GetAllScripts
  -> m SlotNo
  -> (V1.TransactionInput -> m (Maybe V1.TransactionInput))
  -> GetContractSource m
  -> ApplyInputs m
mkApplyInputs (networkId, systemStart, protocolParams) fetchEraHistory getContractState getAllScripts getCurrentSlotNo merkleizeInputs getContractSource =
  \walletContext contractId transactionMetadata invalidBefore invalidHereafter inputs -> do
    eraHistory <- fetchEraHistory
    case eraHistory of
      Left _ -> pure $ Left ApplyInputsEraHistoryNotInitialized
      Right eh -> do
        let
          solveConstraints = Constraints.solveConstraints systemStart (C.toLedgerEpochInfo eh)
          loadMarloweContext :: LoadMarloweContext m
          loadMarloweContext = mkLoadMarloweContext
            networkId
            getContractState
            getAllScripts
          analysisTimeout = 60
        execApplyInputs
          merkleizeInputs
          C.ConwayEra
          protocolParams
          (getContractSource . toSourceId)
          getCurrentSlotNo
          systemStart
          eh
          solveConstraints
          walletContext
          loadMarloweContext
          emptyLoadHelpersContext
          networkId
          MarloweV1
          contractId
          transactionMetadata
          invalidBefore
          invalidHereafter
          inputs
          analysisTimeout

type LedgerInfo = (C.NetworkId, C.SystemStart, L.PParams (C.ShelleyLedgerEra C.ConwayEra))

mkServerMStore :: ServerM (ContractStore.ContractStore ServerM)
mkServerMStore = do
  let liftStage :: forall a. UnliftIO.STM.STM a -> ServerM a
      liftStage = liftIO . UnliftIO.STM.atomically
  stmStore <- liftIO $ UnliftIO.STM.atomically StoreMemory.createContractStoreInMemory
  pure $ Store.hoistContractStore liftStage stmStore

mkWithBundleImporter
  :: (MonadUnliftIO m, MonadLog m)
  => ContractStore.ContractStore m
  -> WithBundleImporter m a
mkWithBundleImporter store handler = do
  bracket
    (ContractStore.createContractStagingArea store)
    ContractStore.discard
    \stagingArea -> do
      let
        importBundle = TransferServer.mkImportBundle stagingArea
      handler importBundle

mkGetContractSource
  :: MonadIO m
  => ContractStore.ContractStore m
  -> GetContractSource m
mkGetContractSource store hash = do
  liftIO $ putStrLn ("Get contract source wrapper" :: String)
  store.getContract (fromSourceId hash)

merkleizeInputsWrapper
  :: forall m
   . MonadIO m
  => ContractStore.ContractStore m
  -> V1.TransactionInput -> m (Maybe V1.TransactionInput)
merkleizeInputsWrapper store tx = do
  liftIO $ putStrLn ("Merklize inputs wrapper" :: String)
  result <- store.merkleizeInputs undefined undefined tx
  case result of
    Right tx' -> pure $ Just tx'
    Left _ -> pure Nothing

queryLedgerInfo :: C.NetworkId -> C.LocalNodeConnectInfo -> IO LedgerInfo
queryLedgerInfo networkId connectInfo = do
  rawQueryResult <- C.executeLocalStateQueryExpr connectInfo C.VolatileTip $
    (,)
      <$> C.querySystemStart
      <*> C.queryProtocolParameters C.ShelleyBasedEraConway
  case rawQueryResult of
    Right (Right systemStart, Right (Right protocolParams)) -> do
      pure (networkId, systemStart, protocolParams)
    _ -> liftIO . die $ "Failed to query the cardano-node for necessary information."

mkGetAllScripts
  :: C.NetworkId
  -> Maybe (PublishingInfo C.ConwayEra)
  -> Either String GetAllScripts
mkGetAllScripts networkId = \case
  Nothing -> pure ScriptRegistry.getAllScripts
  Just publishingInfo ->
    case marloweScriptsFromPublishingInfo networkId publishingInfo of
      Nothing -> Left "Failed to parse the provided script registry file. Please ensure it is correct and try again."
      Just marloweScripts -> pure $ GetAllScripts \case
        MarloweV1 -> Set.singleton marloweScripts

mkGetCurrentScripts
  :: C.NetworkId
  -> Maybe (PublishingInfo C.ConwayEra)
  -> Either String GetCurrentScripts
mkGetCurrentScripts networkId = \case
  Nothing -> pure ScriptRegistry.getCurrentScripts
  Just publishingInfo ->
    case marloweScriptsFromPublishingInfo networkId publishingInfo of
      Nothing -> Left "Failed to parse the provided script registry file. Please ensure it is correct and try again."
      Just ms -> pure $ GetCurrentScripts \case
        MarloweV1 -> ms

mkServerDependencies
  :: Pool.Pool
  -> LedgerInfo
  -> GetAllScripts
  -> GetCurrentScripts
  -> ServerM (ServerDependencies ServerM)
mkServerDependencies pool ledgerInfo getAllScripts getCurrentScripts = do
  contractStore <- mkServerMStore
  let
    dbQueries :: DatabaseQueries ServerM
    dbQueries =
      logDatabaseQueries $
        hoistDatabaseQueries
          (either (liftIO . throwIO) pure <=< liftIO . Pool.use pool)
          databaseQueries
    fetchEraHistory = getEraHistory dbQueries
    getCurrentSlotNo =
      getNodeTip dbQueries >>= \case
        Left err -> liftIO . throwIO $ userError $ "Failed to get indexer tip: " <> show err
        Right (NodeTip (ChainTip possibleBlockHeader)) -> do
          case possibleBlockHeader of
            Just BlockHeader{..} -> pure slotNo
            Nothing -> pure $ SlotNo 0

    initContract = mkInitContract
      ledgerInfo
      fetchEraHistory
      getCurrentScripts
      (mkGetContractSource contractStore)
      (UseRoleTokenDevelScript True)

    applyInputs = mkApplyInputs
      ledgerInfo
      fetchEraHistory
      (getContractState dbQueries)
      getAllScripts
      getCurrentSlotNo
      (merkleizeInputsWrapper contractStore)
      (mkGetContractSource contractStore)

  let deps :: ServerDependencies ServerM
      deps =
        ServerDependencies
          { applyInputs
          , burnRoleTokens = undefined
          , getContractSource = mkGetContractSource contractStore
          , initContract
          , loadContract = fmap (fmap Right) . getContractState dbQueries
          , loadPayout = undefined
          , loadPayouts = undefined
          , loadTransaction = undefined
          , loadTransactions = undefined
          , loadWithdrawal = undefined
          , loadWithdrawals = undefined
          , withBundleImporter = mkWithBundleImporter contractStore
          , withdraw = undefined
          }
  pure deps

runApp :: Options -> IO ()
runApp opts = do
  pool <- do
    let
      cfg = Hasql.settings [ Hasql.staticConnectionSettings opts.databaseUri ]
    Pool.acquire cfg

  customPublishingInfo <- case opts.scriptRegistryFile of
    Nothing -> pure Nothing
    Just filePath -> do
      putStrLn $ "Loading script registry from " ++ filePath
      decodeFileStrict MessageFormatText filePath <&> Just

  let
    -- FIXME: Move these to Options
    debugInfoHttpResponse = True

  Wai.withStdoutLogger \waiLogger -> do
    let
      Port port = opts.port
      localNodeConnectInfo = C.LocalNodeConnectInfo
        { C.localConsensusModeParams = C.CardanoModeParams $ C.EpochSlots 21_600
        , C.localNodeNetworkId = opts.networkId
        , C.localNodeSocketPath = C.File opts.nodeSocketPath
        }
    ledgerInfo <- liftIO $ queryLedgerInfo opts.networkId localNodeConnectInfo
    getAllScripts  <- case mkGetAllScripts opts.networkId customPublishingInfo of
      Left err -> die err
      Right getAllScripts -> pure getAllScripts
    getCurrentScripts <- case mkGetCurrentScripts opts.networkId customPublishingInfo of
      Left err -> die err
      Right getCurrentScripts -> pure getCurrentScripts
    let
      prettyLog = True
      withLogger = if prettyLog then withStdOutLogger else withStdOutInlinedLogger
      errorRewriter = ResponseRewriterMiddleware.defaultErrorRewriter
      handleException :: SomeException -> Wai.Response
      handleException e = do
        let
          headers =
            [ (H.hContentType, "application/json")
            , ("Access-Control-Allow-Origin", "*")
            ]
        Wai.responseLBS
          H.internalServerError500
          headers
          (BSL.fromStrict . T.encodeUtf8 . T.pack $ show e)

      waiSettings =
        Wai.setOnExceptionResponse handleException $
          Wai.setPort port $
            Wai.setTimeout 600 $
              Wai.setLogger waiLogger Wai.defaultSettings

      api :: Proxy RuntimeAPIWithOpenAPI
      api = Proxy

    withLogger \appLogger -> do
      let
        scriptHashes = case getAllScripts of
          GetAllScripts f -> do
            let scripts = f MarloweV1
            A.object
              [ "marlowe" .= (Set.toList scripts <&> \ms -> ms.marloweScript)
              , "payout" .= (Set.toList scripts <&> \ms -> ms.payoutScript)
              ]

      runLogT "marlowe-runtime-server" appLogger opts.logLevel do
        logInfo "Starting Marlowe Runtime Server" $ A.object
          [ "port" .= port
          , "nodeSocketPath" .= opts.nodeSocketPath
          , "networkId" .= show opts.networkId
          , "scriptHashes" .= scriptHashes
          ]

      dependencies <- runServerMExtract undefined (mkServerDependencies pool ledgerInfo getAllScripts getCurrentScripts)

      Wai.runSettings waiSettings $
        ResponseRewriterMiddleware.mkMiddleware errorRewriter $
          serverMiddleware debugInfoHttpResponse appLogger opts.logLevel $
            serveWithContext api (customFormatters :. EmptyContext) $
              hoistServer
                api
                (Handler . ExceptT . try . runServer appLogger opts.logLevel dependencies)
                serverWithOpenApi

customErrorFormatter :: ErrorFormatter
customErrorFormatter tr _req errMsg =
  err400
    { errBody = A.encode $ A.object
        [ "error"   .= errMsg
        , "type"    .= show tr   -- e.g. Capture', Header', QueryParam', etc.
        ]
    , errHeaders = [("Content-Type", "application/json")]
    }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customErrorFormatter
  , urlParseErrorFormatter   = customErrorFormatter
  , headerParseErrorFormatter = customErrorFormatter
  }

scriptRegistryFileParser :: Parser FilePath
scriptRegistryFileParser =
  strOption
    ( long "script-registry-file"
        <> help "Path to a JSON file containing the script registry. If not provided, the server will use the default script registry."
        <> metavar "SCRIPT_REGISTRY_FILE"
    )

mkParser :: IO (Parser (IO ()))
mkParser = do
  nodeSocketParser <- mkNodeSocketParser
  networkIdParser <- mkNetworkIdParser
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser
      <*> nodeSocketParser
      <*> networkIdParser
      <*> portParser
      <*> optional scriptRegistryFileParser

    versionOption =
      infoOption ("marlowe-runtime-server " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

  pure $ helper <*> versionOption <*> (runApp <$> parserOptions)

main :: IO ()
main = do
  parser <- mkParser
  let
    parserInfo :: ParserInfo (IO ())
    parserInfo = do
      let
        description =
          fullDesc
            <> progDesc "Marlowe Runtime Server"
            <> header "marlowe-runtime-server - A server for the Marlowe Runtime API"
      info parser description
  join $ execParser parserInfo


decodeUtf8OrEncodeHex :: BS.ByteString -> T.Text
decodeUtf8OrEncodeHex bs =
  case T.decodeUtf8' bs of
    Left _ -> T.decodeUtf8 $ Base16.encode bs
    Right t -> t

runLogT' :: Logger -> LogLevel -> LogT IO a -> IO a
runLogT' = runLogT "marlowe-runtime-server"

serverMiddleware :: Bool -> Logger -> LogLevel -> Application -> Application
serverMiddleware debugInfoHttpResponse logger logLevel = do
  let
    accessControlAllowOriginKey = "Access-Control-Allow-Origin"
    accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
    handle500Errors errMsg req res = do
      reqBody <- Wai.getRequestBodyChunk req
      let headers =
            A.object $
              Wai.requestHeaders req <&> \(k, v) -> do
                let bs = CI.original k
                    key = decodeUtf8OrEncodeHex bs
                (A.fromText key, A.toJSON $ decodeUtf8OrEncodeHex v)
          err = A.object
            [ "msg" .= ("Captured 500" :: Text)
            , "body" .=
              A.object
                [ ("remaining-body", A.String $ T.decodeUtf8 reqBody)
                , ("original-error", A.String $ T.pack errMsg)
                , ("headers", headers)
                ]
            ]
      runLogT' logger logLevel $
        logAttention "500 Internal Server Error" err
      let errorResponse =
            if debugInfoHttpResponse
              then err500JSON err
              else err500 "Internal Server Error"
      res $ responseServerError errorResponse

    -- Catch any IO exception and return a 500 error with cors header
    catchMiddleware =
      protect
        [ ErrHandler $ \(e :: SomeException) req res -> handle500Errors (show e) req res
        ]
    -- Add missing CORS headers to the application raised error
    addCorsHeadersMiddleware =
      protect
        [ ErrHandler $ \err@ServerError{errHeaders, errHTTPCode} req res -> do
            let err' =
                  if accessControlAllowOriginKey `elem` fmap fst errHeaders
                    then err
                    else
                      err
                        { errHeaders =
                            ("Content-Type", "application/json") : accessControlAllowOrigin : errHeaders
                        }
            if errHTTPCode >= 500
              then handle500Errors (show err) req res
              else res $ responseServerError err'
        ]
    simpleCorsMiddleware = do
      let simpleCorsResourcePolicy' =
            simpleCorsResourcePolicy
              { corsRequestHeaders = ["Content-Type"]
              }
      cors (const $ Just simpleCorsResourcePolicy')
  addCorsHeadersMiddleware . simpleCorsMiddleware . catchMiddleware

data ErrHandler = forall e. (Exception e) => ErrHandler (e -> Application)

protect :: [ErrHandler] -> Wai.Middleware
protect handlers app req res = do
  let handlers' = fmap (\(ErrHandler h) -> Exception.Lifted.Handler \e -> h e req res) handlers
  Exception.Lifted.catches (app req res) handlers'

mkLoadMarloweContext
  :: forall m
   . Monad m
  => C.NetworkId
  -> GetContractState m
  -> GetAllScripts
  -> LoadMarloweContext m
mkLoadMarloweContext networkId getContractState (GetAllScripts getAllScripts) desiredVersion contractId = runExceptT do
  someContractState <- lift $ getContractState contractId
  case someContractState of
    Nothing -> throwE T.LoadMarloweContextErrorNotFound
    Just (SomeContractState actualVersion ContractState{initialOutput, latestOutput}) -> do
      case testEquality desiredVersion actualVersion of
        Nothing -> throwE $ T.LoadMarloweContextErrorVersionMismatch
          (T.Expected (Core.SomeMarloweVersion desiredVersion))
          (T.Actual (Core.SomeMarloweVersion actualVersion))
        Just Refl -> do
          let
            Core.TransactionScriptOutput{..} = initialOutput
          desiredMarloweScriptHash <- case paymentCredential address of
            Just (Core.ScriptCredential hash) -> pure hash
            _ -> throwE $ T.MarloweAddressNotScriptAddress address
          let
            matchesScriptHash MarloweScripts{..} = marloweScript == desiredMarloweScriptHash
            -- A set of marlowe scripts information which we
            -- lookup by marlowe validator hash and then
            -- by the specific field.
            scripts :: Set MarloweScripts
            scripts = getAllScripts actualVersion

          marloweScripts <- except
            . note (T.MarloweScriptNotPublished desiredMarloweScriptHash)
            $ find matchesScriptHash scripts

          marloweScriptUTxO <- except
            . note (T.MarloweScriptNotPublished marloweScripts.marloweScript)
            $ Map.lookup networkId marloweScripts.marloweScriptUTxOs

          payoutScriptUTxO <- except
            . note (T.PayoutScriptNotPublished marloweScripts.payoutScript)
            $ Map.lookup networkId marloweScripts.payoutScriptUTxOs

          cardanoScriptHash <- except
            . note (T.CardanoConversionFailure "MarloweScriptHash")
             $ toCardanoScriptHash desiredMarloweScriptHash

          pure MarloweContext
            { marloweAddress = address
            , payoutScriptHash = marloweScripts.payoutScript
            , marloweScriptHash = desiredMarloweScriptHash
            , payoutAddress =
                fromCardanoAddressInEra C.BabbageEra $
                  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
                    C.makeShelleyAddress
                      networkId
                      (C.PaymentCredentialByScript cardanoScriptHash)
                      C.NoStakeAddress
            , scriptOutput = latestOutput
            , marloweScriptUTxO
            , payoutScriptUTxO
            }

data PublishingInfo era = PublishingInfo
  { marlowe :: AUTxO era
  , payout :: AUTxO era
  , openRole :: AUTxO era
  }

instance A.ToJSON (AUTxO era) => A.ToJSON (PublishingInfo era) where
  toJSON (PublishingInfo{..}) = A.object
    [ "marlowe" .= marlowe
    , "payout" .= payout
    , "openRole" .= openRole
    ]

instance A.FromJSON (AUTxO era) => A.FromJSON (PublishingInfo era) where
  parseJSON = A.withObject "PublishingInfo" $ \o -> do
    marlowe <- o A..: "marlowe"
    payout <- o A..: "payout"
    openRole <- o A..: "openRole"
    pure PublishingInfo{..}

calcReferenceScriptHash
  :: AUTxO era
  -> Maybe Core.ScriptHash
calcReferenceScriptHash (AUTxO (_, C.TxOut _ _ _ C.ReferenceScriptNone)) = Nothing
calcReferenceScriptHash (AUTxO (_, C.TxOut _ _ _ (C.ReferenceScript _ scriptInAnyLang))) = do
  case scriptInAnyLang of
    C.ScriptInAnyLang _lang script -> do
      pure . fromCardanoScriptHash . C.hashScript $ script

fromCardanoReferenceScript
  :: C.ReferenceScript era
  -> Maybe ScriptInPlutus
fromCardanoReferenceScript C.ReferenceScriptNone = Nothing
fromCardanoReferenceScript (C.ReferenceScript _ script) =
  fromCardanoScriptInAnyLang script

referenceScriptUTxOFromAUTxO
  :: forall era
   . C.IsCardanoEra era
  => AUTxO era
  -> Maybe ReferenceScriptUtxo
referenceScriptUTxOFromAUTxO (AUTxO (txIn, txOutOrig)) = do
  let
    txOutRef = fromCardanoTxIn txIn
  script <- fromCardanoReferenceScript $ case txOutOrig of
    C.TxOut _ _ _ refScript -> refScript
  txOut <- fromCardanoTxOutCtxUTxO C.cardanoEra txOutOrig
  pure ReferenceScriptUtxo{..}

-- data MarloweScripts = MarloweScripts
--   { marloweScript :: ScriptHash
--   , payoutScript :: ScriptHash
--   , helperScripts :: Map HelperScript ScriptHash
--   , marloweScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
--   , payoutScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
--   , helperScriptUTxOs :: Map (HelperScript, NetworkId) ReferenceScriptUtxo
--   }
--   deriving (Show, Eq, Ord)
--
marloweScriptsFromPublishingInfo
  :: C.NetworkId
  -> PublishingInfo C.ConwayEra
  -> Maybe MarloweScripts
marloweScriptsFromPublishingInfo networkId PublishingInfo{..} = do
  marloweScriptHash <- calcReferenceScriptHash marlowe
  payoutScriptHash <- calcReferenceScriptHash payout
  let
    helpersScriptsPlaceholder = mempty
  marloweScriptUTxO <- referenceScriptUTxOFromAUTxO marlowe
  payoutScriptUTxO <- referenceScriptUTxOFromAUTxO payout
  pure $ MarloweScripts
    { marloweScript = marloweScriptHash
    , payoutScript = payoutScriptHash
    , helperScripts = helpersScriptsPlaceholder
    , marloweScriptUTxOs = Map.singleton networkId marloweScriptUTxO
    , payoutScriptUTxOs = Map.singleton networkId payoutScriptUTxO
    , helperScriptUTxOs = mempty
    }


