{-# LANGUAGE UndecidableInstances #-}

-- We "return" lambdas for clarity in some helpers.
{- HLINT ignore "Redundant lambda" -}

module Main where

-- import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..)) -- removed for now
import Cardano.Api qualified as C
import Cardano.Api.Monad.Error (except)
import Cardano.Ledger.Core qualified as L
import Control.Applicative (Alternative((<|>)))
import Control.Error (throwE, note, hush)
import Control.Exception (throwIO, Exception, SomeException, try)
import Control.Exception.Lifted qualified as Exception.Lifted
import Control.Monad (join, when, (<=<), unless, guard)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A
import Data.Bifunctor (Bifunctor(first))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Data (type (:~:)(Refl))
import Data.Foldable (Foldable(..), find)
import Data.Functor ((<&>))
import Data.List (uncons)
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Type.Equality (TestEquality(testEquality))
import Data.Version (showVersion)
import Hasql.Connection.Settings qualified as Hasql
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Hasql
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressInEra, toCardanoScriptHash)
import Language.Marlowe.Runtime.ChainSync.Api (paymentCredential, SlotNo(SlotNo), NodeTip(NodeTip), ChainTip(ChainTip), BlockHeader(BlockHeader))
import Language.Marlowe.Runtime.ChainSync.Api qualified as Core
import Language.Marlowe.Runtime.Contract.Store qualified as ContractStore
import Language.Marlowe.Runtime.Contract.Store qualified as Store
import Language.Marlowe.Runtime.Contract.Store.File qualified as StoreFile
import Language.Marlowe.Runtime.Contract.Store.Memory qualified as StoreMemory
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1), Transaction(Transaction, transactionId))
import Language.Marlowe.Runtime.Core.Api qualified as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), ScriptDetails(..), ScriptRegistry)
import Language.Marlowe.Runtime.Core.ScriptRegistry qualified as ScriptRegistry
import Language.Marlowe.Runtime.Query
    ( SomeContractState(SomeContractState),
      ContractState(ContractState, initialOutput, latestOutput),
      SomeTransactions(SomeTransactions) )
import Language.Marlowe.Runtime.Query.Database
  ( hoistDatabaseQueries
  , logDatabaseQueries
  , DatabaseQueries(getContractState, getTransaction, getTransactions, getEraHistory, getNetworkId, getNodeTip, getProtocolParameters, getSystemStart)
  , GetEraHistoryError
  , GetNetworkIdError(..)
  , GetProtocolParametersError(..)
  , GetSystemStartError(..)
  )
import Language.Marlowe.Runtime.Query.Database.PostgreSQL (databaseQueries)
import Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetContractState (GetContractState)
import Language.Marlowe.Runtime.Transaction.Api (LoadHelpersContextError, RoleTokensConfig, InitError(InitEraHistoryNotInitialized), ApplyInputsError(ApplyInputsEraHistoryNotInitialized))
import Language.Marlowe.Runtime.Transaction.Api qualified as T
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Transaction.Builders (execInit, execApplyInputs, LoadMarloweContext)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(MarloweContext))
import Language.Marlowe.Runtime.Transaction.Constraints qualified as Constraints
import Language.Marlowe.Runtime.Web.Contract.Source.Server (fromSourceId, toSourceId)
import Language.Marlowe.Runtime.Web.Core.Object.Schema ()
import Language.Marlowe.Runtime.Web.Server (runServer, serverWithOpenApi, ServerDependencies(..), RuntimeAPIWithOpenAPI)
import Language.Marlowe.Runtime.Web.Server.Monad
    ( ServerM,
      InitContract,
      ApplyInputs,
      LoadTransactions,
      GetContractSource,
      WithBundleImporter,
      LoadTxError(ContractNotFound, TxNotFound) )
import Log (LogLevel (..), runLogT, Logger, logAttention, LogT, MonadLog, logInfo)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Backend.StandardOutputInlined (withStdOutInlinedLogger)
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), emitError)
import Marlowe.Plutus.Semantics qualified as V1
import Marlowe.Runtime.Server.Contrib.Servant.Err500 (err500, err500JSON)
import Marlowe.Runtime.Server.Contrib.Servant.ResponseRewriterMiddleware as ResponseRewriterMiddleware
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (HostPreference)
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.Logger qualified as Wai
import Network.Wai.Middleware.Cors ( CorsResourcePolicy (corsRequestHeaders), cors, simpleCorsResourcePolicy,)
import Options.Applicative ( Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, infoOption, long, metavar, option, progDesc, short, ReadM, asum, flag', eitherReader, strOption, auto, value, optional, showDefault)
import Paths_marlowe_runtime (version)
import Servant ( Application, ServerError (..), hoistServer, serveWithContext, Handler (Handler), ErrorFormatter, ErrorFormatters, bodyParserErrorFormatter, urlParseErrorFormatter, headerParseErrorFormatter, defaultErrorFormatters, err400, Context(EmptyContext, (:.)))
import Servant.Pipes ()
import Servant.Server.Internal.ServerError (responseServerError)
import System.Environment.Blank (getEnv)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Read qualified as T
import UnliftIO (bracket)
import UnliftIO.STM qualified
import qualified Marlowe.Plutus.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.Query as Query
import Servant.Pagination (RangeOrder(RangeAsc, RangeDesc))
import Language.Marlowe.Runtime.Web.Server.Util (applyRangeToAscList)

newtype Port = Port Int

newtype GetAllScripts = GetAllScripts (forall v. MarloweVersion v -> Set MarloweScripts)
newtype GetCurrentScripts = GetCurrentScripts (forall v. MarloweVersion v -> MarloweScripts)

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
  , networkId :: C.NetworkId
  , host :: String
  , port :: Port
  , scriptRegistry :: Maybe FilePath
  , storeConfig :: StoreConfig
  }

data StoreConfig
  = FileStoreConfig
      { fileStoreDir :: FilePath
      , fileStoreMaxContractAgeSeconds :: Integer
      , fileStoreMaxStoreSizeBytes :: Integer
      }
  | InMemoryStoreConfig

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

portParser :: Parser Port
portParser = option
  (Port <$> auto)
  ( long "port"
    <> short 'p'
    <> help "The port to serve the server on"
    <> metavar "PORT"
    <> value (Port 8090)
  )

hostParser :: Parser String
hostParser =
  strOption
    ( long "host"
        <> short 'h'
        <> metavar "HOST_NAME"
        <> help
            "The host name to bind the HTTP server to. Defaults to 0.0.0.0 (all interfaces). Use this only when you specifically need to constrain the bind -- for access control, use the host firewall."
        <> value "0.0.0.0"
        <> showDefault
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

-- FIXME: paluh - we will likely need to depend on some helper executable that
-- produces a role policy at runtime. For now this is a stub that errors on
-- use; the flow needs to be rethought and replaced with a proper external
-- call.
mkRoleTokensPolicy :: UseRoleTokenDevelScript -> MkRoleTokenMintingPolicy m
mkRoleTokensPolicy _ _ = pure $ error "marlowe-runtime:server:Main.mkRoleTokensPolicy:Role-token minting policy is not configured"

-- FIXME: paluh - This is plain wrong and will fail at some point
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
mkInitContract (networkId, systemStart, protocolParams) fetchEraHistory resolvedCurrentScripts getContractSource useRoleTokenDevelScript =
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
          (case resolvedCurrentScripts of GetCurrentScripts f -> f MarloweV1)
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
  -> GetContractState m
  -> GetAllScripts
  -> m SlotNo
  -> (V1.Contract -> V1.State -> V1.TransactionInput -> m (Maybe V1.TransactionInput))
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

mkServerMStore :: StoreConfig -> IO (ContractStore.ContractStore ServerM)
mkServerMStore InMemoryStoreConfig = do
  let liftStage :: forall a. UnliftIO.STM.STM a -> ServerM a
      liftStage = UnliftIO.STM.atomically
  stmStore <- UnliftIO.STM.atomically StoreMemory.createContractStoreInMemory
  pure $ Store.hoistContractStore liftStage stmStore
mkServerMStore FileStoreConfig{..} = do
  store <- StoreFile.createContractStore
    StoreFile.ContractStoreOptions
      { contractStoreDirectory = fileStoreDir
      , contractStoreStagingDirectory = fileStoreDir </> "staging"
      , lockingMicrosecondsBetweenRetries = 500_000
      , maxContractAge = fromIntegral fileStoreMaxContractAgeSeconds
      , maxStoreSize = fileStoreMaxStoreSizeBytes
      }
  pure $ Store.hoistContractStore liftIO store

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
        importer = Store.mkBundleImporter stagingArea
      handler importer

queryLedgerInfo
  :: DatabaseQueries IO
  -> C.NetworkId
  -> IO LedgerInfo
queryLedgerInfo dbQueries networkId = do
  -- Verify the network id recorded by the indexer matches the one we were
  -- asked to operate on. A mismatch usually means we are talking to a
  -- different cardano-node than the indexer and we should refuse to start
  -- rather than produce wrong transactions.
  storedNetworkId <- dbQueries.getNetworkId networkId >>= \case
    Right nid -> pure nid
    Left MissingNetworkId -> die "marlowe-runtime: network id has not been seeded by the indexer yet. Run the marlowe-indexer to warm up the database before starting the runtime server."
    Left (InvalidNetworkId bs) -> die $ "marlowe-runtime: stored network id is invalid: " <> show bs
    Left (NetworkIdMismatch stored provided) -> die $
      "marlowe-runtime: stored network id (" <> show stored
        <> ") does not match the configured one (" <> show provided
        <> "). Check --network-id against the indexer's configuration."
  systemStart <- dbQueries.getSystemStart >>= \case
    Right ss -> pure ss
    Left MissingSystemStart -> die "marlowe-runtime: system start has not been seeded by the indexer yet."
    Left (InvalidSystemStart bs) -> die $ "marlowe-runtime: stored system start is invalid: " <> show bs
  protocolParams <- dbQueries.getProtocolParameters >>= \case
    Right pp -> pure pp
    Left MissingProtocolParameters -> die "marlowe-runtime: protocol parameters have not been seeded by the indexer yet."
    Left (InvalidProtocolParameters bs) -> die $ "marlowe-runtime: stored protocol parameters are invalid: " <> show bs
  pure (storedNetworkId, systemStart, protocolParams)

mkGetAllScripts
  :: Maybe ScriptRegistry
  -> IO (Either String GetAllScripts)
mkGetAllScripts customScriptRegistry = runExceptT do
  ScriptRegistry.ScriptRegistry _ scripts <- case customScriptRegistry of
    Nothing -> ExceptT $ first show <$> ScriptRegistry.loadDefaultScriptRegistry
    Just scriptRegistry -> pure scriptRegistry
  pure $ GetAllScripts \_ -> Set.fromList . Map.elems . NEMap.toMap $ scripts

mkGetCurrentScripts
  :: C.NetworkId
  -> Maybe ScriptRegistry
  -> IO (Either String GetCurrentScripts)
mkGetCurrentScripts networkId customScriptRegistry = runExceptT do
  marloweScripts <- case customScriptRegistry of
    Nothing -> ExceptT $ first show <$> ScriptRegistry.loadDefaultMarloweScripts
    Just scriptRegistry -> pure $ ScriptRegistry.getCurrentScripts scriptRegistry

  let
    ms@(ScriptRegistry.MarloweScripts { marloweScript, payoutScript }) = marloweScripts

    isPublished :: ScriptRegistry.ScriptDetails -> Bool
    isPublished (ScriptRegistry.ScriptDetails { scriptUTxOs }) = isJust $ Map.lookup networkId scriptUTxOs

  unless (all isPublished [marloweScript, payoutScript ]) $
    fail "Provided ScriptRegistry does not cover expected network"

  pure $ GetCurrentScripts \case
    MarloweV1 -> ms

mkServerDependencies
  :: Pool.Pool
  -> LedgerInfo
  -> GetAllScripts
  -> GetCurrentScripts
  -> StoreConfig
  -> IO (ServerDependencies ServerM)
mkServerDependencies pool ledgerInfo getAllScripts resolvedCurrentScripts storeConfig = do
  contractStore <- mkServerMStore storeConfig
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

    getContractSource hash = contractStore.getContract (fromSourceId hash)

    initContract = mkInitContract
      ledgerInfo
      fetchEraHistory
      resolvedCurrentScripts
      getContractSource
      (UseRoleTokenDevelScript True)


    merkleizeInputs contract state inputs =
      hush <$> contractStore.merkleizeInputs contract state inputs

    applyInputs = mkApplyInputs
      ledgerInfo
      fetchEraHistory
      (getContractState dbQueries)
      getAllScripts
      getCurrentSlotNo
      merkleizeInputs
      getContractSource

    loadTransactions :: LoadTransactions ServerM
    loadTransactions = \contractId Query.Range{..} -> do
      mTxs <- dbQueries.getTransactions contractId
      pure do
        SomeTransactions MarloweV1 txs <- note ContractNotFound mTxs
        let totalCount = length txs
        let direction = case rangeDirection of
              Query.Ascending -> RangeAsc
              Query.Descending -> RangeDesc
        items <- note TxNotFound $ applyRangeToAscList transactionId rangeStart rangeLimit rangeOffset direction txs
        pure
          Query.Page
            { items
            , nextRange = do
                guard $ length items == rangeLimit
                (Transaction{transactionId}, _) <- uncons $ reverse items
                pure $
                  Query.Range
                    { rangeStart = Just transactionId
                    , ..
                    }
            , totalCount
            }

    loadTransaction = const dbQueries.getTransaction

    deps :: ServerDependencies ServerM
    deps =
        ServerDependencies
          { applyInputs
          , burnRoleTokens = undefined
          , getContractSource
          , initContract
          , loadContract = fmap (fmap Right) . dbQueries.getContractState
          , loadPayout = undefined
          , loadPayouts = undefined
          , loadTransaction
          , loadTransactions
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

  customScriptRegistry <- case opts.scriptRegistry of
    Nothing -> pure Nothing
    Just filePath -> do
      putStrLn $ "Loading script registry from " ++ filePath
      decodeFileStrict MessageFormatText filePath <&> Just

  let
    -- FIXME: paluh - Move these to Options
    debugInfoHttpResponse = True

  Wai.withStdoutLogger \waiLogger -> do
    let
      Port port = opts.port
      host = opts.host
      dbQueries :: DatabaseQueries IO
      dbQueries =
        hoistDatabaseQueries
          (either throwIO pure <=< Pool.use pool)
          databaseQueries
    ledgerInfo <- liftIO $ queryLedgerInfo dbQueries opts.networkId

    getAllScripts  <- mkGetAllScripts customScriptRegistry >>= \case
      Left err -> die err
      Right getAllScripts -> pure getAllScripts

    resolvedCurrentScripts <- mkGetCurrentScripts opts.networkId customScriptRegistry >>= \case
      Left err -> die err
      Right resolvedCurrentScripts -> pure resolvedCurrentScripts

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
            Wai.setHost (fromString host :: HostPreference) $
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
          , "networkId" .= show opts.networkId
          , "scriptHashes" .= scriptHashes
          ]

      dependencies <- mkServerDependencies pool ledgerInfo getAllScripts resolvedCurrentScripts opts.storeConfig
      Wai.runSettings waiSettings $
        ResponseRewriterMiddleware.mkMiddleware errorRewriter $
          serverMiddleware debugInfoHttpResponse appLogger opts.logLevel $
            serveWithContext api (customFormatters :. EmptyContext) $
              hoistServer
                api
                ( Handler . ExceptT . try . runServer appLogger opts.logLevel dependencies)
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

scriptRegistryParser :: Parser FilePath
scriptRegistryParser =
  strOption
    ( long "script-registry"
        <> help "Path to a JSON file containing the script registry. If not provided, the server will use the default script registry."
        <> metavar "SCRIPT_REGISTRY"
    )

storeDirParser :: Parser FilePath
storeDirParser =
  strOption
    ( long "store-dir"
        <> short 's'
        <> metavar "DIR"
        <> help
            "Directory used to persist the contract store. Enables the file-system backed contract store. Requires --max-contract-age / --max-store-size to also be valid for this backend. Mutually exclusive with --in-memory-store."
    )

maxContractAgeParser :: Parser Integer
maxContractAgeParser =
  option auto
    ( long "max-contract-age"
        <> metavar "SECONDS"
        <> help
            "The maximum age, in seconds, a contract in the store may reach before it becomes eligible for garbage collection. Only meaningful with --store-dir."
        <> value (24 * 60 * 60)
        <> showDefault
    )

maxStoreSizeParser :: Parser Integer
maxStoreSizeParser =
  option auto
    ( long "max-store-size"
        <> metavar "BYTES"
        <> help
            "The maximum allowed size of the contract store, in bytes. Only meaningful with --store-dir."
        <> value (32 * 1024 * 1024 * 1024)
        <> showDefault
    )

inMemoryStoreParser :: Parser StoreConfig
inMemoryStoreParser =
  flag' InMemoryStoreConfig
    ( long "in-memory-store"
        <> help
            "Use a non-persistent in-memory contract store. Intended only for debug/devel environments; contract data does not survive restarts. Mutually exclusive with --store-dir."
    )

fileStoreParser :: Parser StoreConfig
fileStoreParser =
  FileStoreConfig
    <$> storeDirParser
    <*> maxContractAgeParser
    <*> maxStoreSizeParser

storeConfigParser :: Parser StoreConfig
storeConfigParser = fileStoreParser <|> inMemoryStoreParser

mkParser :: IO (Parser (IO ()))
mkParser = do
  networkIdParser <- mkNetworkIdParser
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser
      <*> networkIdParser
      <*> hostParser
      <*> portParser
      <*> optional scriptRegistryParser
      <*> storeConfigParser

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
            scripts :: Set MarloweScripts
            scripts = getAllScripts actualVersion

            matchesScriptHash MarloweScripts{..} = marloweScript.scriptHash == desiredMarloweScriptHash

          marloweScripts <- except
            . note (T.MarloweScriptNotPublished desiredMarloweScriptHash)
            $ find matchesScriptHash scripts

          marloweScriptUTxO <- except
            . note (T.MarloweScriptNotPublished marloweScripts.marloweScript.scriptHash)
            $ Map.lookup networkId marloweScripts.marloweScript.scriptUTxOs

          payoutScriptUTxO <- except
            . note (T.PayoutScriptNotPublished marloweScripts.payoutScript.scriptHash)
            $ Map.lookup networkId marloweScripts.payoutScript.scriptUTxOs

          cardanoScriptHash <- except
            . note (T.CardanoConversionFailure "MarloweScriptHash")
             $ toCardanoScriptHash desiredMarloweScriptHash

          pure MarloweContext
            { marloweAddress = address
            , payoutScriptHash = marloweScripts.payoutScript.scriptHash
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

