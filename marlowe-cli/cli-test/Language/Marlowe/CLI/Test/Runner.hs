{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Language.Marlowe.CLI.Test.Runner where

import Cardano.Api (
  AddressInEra,
  BabbageEraOnwards,
  IsShelleyBasedEra,
  LocalNodeConnectInfo (..),
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger (EraPParams (ppProtocolVersionL))
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api qualified as CS
import Contrib.Cardano.Api (ledgerProtVerToPlutusMajorProtocolVersion, lovelaceFromInt, lovelaceToInt, lppPParamsL)
import Contrib.Control.Exception (liftEitherIO)
import Contrib.Control.Monad.Trans.State.IO (unsafeExecIOStateT)
import Marlowe.Contrib.Foldable (foldMapFlipped)
import Contrib.UnliftIO.Async.Pool qualified as UnliftIO
import Contrib.UnliftIO.Control.Concurrent (threadDelayBy)
import Control.Concurrent.STM (
  TVar,
  modifyTVar,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
  writeTVar,
 )
import Control.Exception (Exception, SomeException, onException, throwIO)
import Control.Lens (makeLenses, view, views, (^.))
import Control.Monad (unless, void, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, runExceptT)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks, withReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.List.Extra qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as S (singleton)
import Data.Text.Lazy qualified as TL
import Data.Time.Units (Second)
import Data.Traversable (for)
import GHC.Generics (Generic)
import GHC.IO (catch)
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Cardano.Api (
  txOutValueValue,
 )
import Language.Marlowe.CLI.Cardano.Api.Value (
  lovelaceToPlutusValue,
  toPlutusValue,
 )
import Language.Marlowe.CLI.Test.Contrib.Monad.Loops (
  MaxRetries (..),
  PrevResult (PrevResult),
  retryTillJust,
  retryTillRight,
 )
import Language.Marlowe.CLI.Test.ExecutionMode qualified as ExecutionMode
import Language.Marlowe.CLI.Test.Interpret (interpret)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError (..))
import Language.Marlowe.CLI.Test.Runtime.Monitor qualified as Runtime.Monitor
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeError (..))
import Language.Marlowe.CLI.Test.Runtime.Types qualified as R
import Language.Marlowe.CLI.Test.Runtime.Types qualified as Runtime
import Language.Marlowe.CLI.Test.TestCase (
  TxCostsUpperBounds (TxCostsUpperBounds),
  testFaucetBudgetUpperBound,
  testTxsFeesUpperBound,
  testsFaucetBudgetUpperBound,
 )
import Language.Marlowe.CLI.Test.Types (
  FailureError (InterpreterError, RuntimeMonitorError),
  FailureReport (FailureReport),
  FaucetsNumber (FaucetsNumber),
  InterpretEnv (..),
  InterpretState (..),
  MaxConcurrentRunners (MaxConcurrentRunners),
  RuntimeConfig (..),
  TestCase (..),
  TestName (TestName),
  TestOperation (WalletOperation),
  TestResult (..),
  involvesRuntime,
  mkInterpretState,
  plutusValueToJSON,
  someTxBodyToJSON,
  testResultToJSON,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  SomeTxBody,
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline),
  WalletNickname (WalletNickname),
  Wallets (Wallets),
  faucetWallet,
  mkWallets,
  nonFaucetWalletsMap,
  overSomeTxBody',
  txBodyFee,
  waSubmittedTransactions,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Transaction (maximumFee, queryUtxos)
import Language.Marlowe.CLI.Types (
  CliEnv (CliEnv),
  CliError (..),
  PrintStats,
  SomePaymentSigningKey,
  TxBuildupContext (..),
 )
import Language.Marlowe.CLI.Types qualified as T
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Language.Marlowe.Runtime.App.Types qualified as Apps
import Network.Protocol.Connection qualified as Network.Protocol
import Network.Protocol.Driver qualified as Network.Protocol
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import PlutusLedgerApi.V2 qualified as P
import PlutusTx.Monoid qualified as P
import System.IO (hPrint, hPutStrLn)
import UnliftIO (MonadUnliftIO (withRunInIO), atomically, race, writeIORef)

data TestRunnerError
  = TestRunnerError String
  | TestRunnerCliError String
  | -- | We run the test interpreter as a part of the runner
    -- to do wallet provisioning and cleanups.
    TestRunnerInterpreterError InterpreterError
  deriving (Show, Generic, Exception, ToJSON, FromJSON)

fromCLIError :: CliError -> TestRunnerError
fromCLIError (CliError e) = TestRunnerCliError e

-- A helper structure which keeps track of the funded transactions
-- across test runs.
data TestRunnerFaucet era = TestRunnerFaucet
  { _trAddress :: AddressInEra era
  , _trSigningKey :: SomePaymentSigningKey
  , _trInitialBalance :: P.Value
  , _trSubmittedTransactions :: ![SomeTxBody]
  -- ^ Transactions submitted directly by the test runner (like sub-faucets funding).
  , _trFundedTransactions :: ![SomeTxBody]
  -- ^ Transactions submitted non directly (sub-faucets and test wallets)
  }

makeLenses 'TestRunnerFaucet

testRunnerFaucetToJSON
  :: (C.IsCardanoEra era) => TestRunnerFaucet era -> A.Value
testRunnerFaucetToJSON TestRunnerFaucet{..} = do
  A.object
    [ "address" .= _trAddress
    , "initialBalance" .= plutusValueToJSON _trInitialBalance
    , "submittedTransactions" .= (_trSubmittedTransactions <&> someTxBodyToJSON)
    , "fundedTransactions" .= (_trFundedTransactions <&> someTxBodyToJSON)
    ]

newtype MasterFaucet era = MasterFaucet
  { _mfFaucet :: TestRunnerFaucet era
  }

makeLenses 'MasterFaucet

type TestsResults lang era = Map.Map (FilePath, TestName) (TestResult lang era)

-- We parametrize over `resource`.
-- Test suite runner allocates faucets funded from master faucet and updates the
-- master faucet at the end.
-- Single test runner allocates a wallet from the pool and updates it at the end.
data Env lang era resource = Env
  { _envEra :: BabbageEraOnwards era
  , _envConnection :: LocalNodeConnectInfo
  , _envResource :: resource
  , _envRuntimeConfig :: Maybe RuntimeConfig
  , _envPrintStats :: PrintStats
  , _envStreamJSON :: Bool
  , _envSlotConfig :: SlotConfig
  , _envProtocolParams :: CS.LedgerProtocolParameters era
  , _envCostModelParams :: [Integer]
  , _envMaxRetries :: Int
  , _envTxBuildupContext :: TxBuildupContext era
  , _envReportDir :: FilePath
  }

makeLenses 'Env

instance ExecutionMode.HasInterpretEnv (Env lang era resource) era where
  eraL = envEra
  txBuildupContextL = envTxBuildupContext

setEnvResource :: Env lang era resource -> resource' -> Env lang era resource'
setEnvResource Env{..} resource = Env{_envResource = resource, ..}

hasRuntimeConfigured :: Env lang era resource -> Bool
hasRuntimeConfigured env = isJust $ view envRuntimeConfig env

-- We use `ReaderT` directly because we want to use `withReaderT` to hide or replace the resource:

-- * test suite runner bracket has to access "master faucet"

-- * test runner loop has to access "faucets pool"

-- * test interpreter can only access its interpret state reference.
type EnvReader lang era resource a = ReaderT (Env lang era resource) IO a

withReaderEnvResource
  :: resource'
  -> EnvReader lang era resource' a
  -> EnvReader lang era resource a
withReaderEnvResource resource = withReaderT (`setEnvResource` resource)

data FaucetToWalletConversion
  = IncludeAllTransactions
  | ExcludeAllTransactions

-- When we run test we use a faucet from our pool and we turn
-- it into a test faucet. At the end of the test we update
-- the faucet with the transactions it submitted and funded.
toTestWallet
  :: (C.IsCardanoEra era)
  => (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => TestRunnerFaucet era
  -> FaucetToWalletConversion
  -> m (Wallet era)
toTestWallet faucet@TestRunnerFaucet{..} conversion = do
  let expectedBalance =
        _trInitialBalance
          <> P.inv
            ( _trSubmittedTransactions
                `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee)
            )
          <> P.inv
            ( _trFundedTransactions
                `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee)
            )
  actualBalance <- toPlutusValue <$> fetchAddressTotalValue _trAddress
  when (actualBalance /= expectedBalance) $ do
    liftIO $
      hPutStrLn stderr $
        "Faucet: " <> TL.unpack (A.encodeToLazyText (testRunnerFaucetToJSON faucet))
    liftIO $ hPutStrLn stderr "has unexpected balance: "
    liftIO $
      hPutStrLn stderr $
        TL.unpack (A.encodeToLazyText (A.toJSON actualBalance))
  pure $
    case conversion of
      ExcludeAllTransactions ->
        Wallet
          { _waAddress = _trAddress
          , _waBalanceCheckBaseline = actualBalance
          , _waSigningKey = _trSigningKey
          , _waSubmittedTransactions = mempty
          , _waExternal = False
          , _waPublishingCosts = mempty
          }
      IncludeAllTransactions -> do
        let actualBalance' =
              actualBalance
                <> P.inv
                  ( _trSubmittedTransactions
                      `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee)
                  )
                <> P.inv
                  ( _trFundedTransactions
                      `foldMapFlipped` (lovelaceToPlutusValue . overSomeTxBody' txBodyFee)
                  )
        Wallet
          { _waAddress = _trAddress
          , _waBalanceCheckBaseline = actualBalance'
          , _waSigningKey = _trSigningKey
          , _waSubmittedTransactions =
              _trSubmittedTransactions <> _trFundedTransactions
          , _waExternal = False
          , _waPublishingCosts = mempty
          }

updateFaucet
  :: TestRunnerFaucet era -> InterpretState lang era -> TestRunnerFaucet era
updateFaucet TestRunnerFaucet{..} InterpretState{_isWallets = wallets} =
  TestRunnerFaucet
    { _trAddress = _trAddress
    , _trSigningKey = _trSigningKey
    , _trInitialBalance = _trInitialBalance
    , _trSubmittedTransactions =
        _trSubmittedTransactions
          <> view waSubmittedTransactions (faucetWallet wallets)
    , _trFundedTransactions =
        _trFundedTransactions
          <> concatMap (view waSubmittedTransactions) (nonFaucetWalletsMap wallets)
    }

newtype TestFaucetBudget
  = TestFaucetBudget Ledger.Coin

type CliAction era a =
  forall m
   . (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (MonadError CliError m)
  => m a

runCli
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => CliAction era a
  -> m a
runCli action = do
  era <- view envEra
  res <- liftIO $ runExceptT $ runReaderT action (CliEnv era)
  case res of
    Left err -> liftIO $ throwIO $ TestRunnerCliError $ show err
    Right a -> pure a

fetchAddressUTxOs
  :: forall era lang m resource
   . (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m (C.UTxO era)
fetchAddressUTxOs address = do
  (res :: Either InterpreterError (C.UTxO era)) <-
    runExceptT $ ExecutionMode.queryUTxOs . C.QueryUTxOByAddress . S.singleton . T.toAddressAny' $ address
  case res of
    Left err -> liftIO $ throwIO $ TestRunnerInterpreterError err
    Right a -> pure a

fetchAddressTotalValue
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m CS.Value
fetchAddressTotalValue addr = do
  utxos <- fetchAddressUTxOs addr
  pure $ foldMap txOutValueValue . Map.elems . C.unUTxO $ utxos

fetchAddressTotalLovelace
  :: (MonadIO m)
  => (MonadReader (Env lang era resource) m)
  => AddressInEra era
  -> m Ledger.Coin
fetchAddressTotalLovelace address = do
  value <- fetchAddressTotalValue address
  pure $ C.selectLovelace value

liftCliEither :: (MonadIO m) => Either CliError a -> m a
liftCliEither = liftIO . liftEitherIO . first fromCLIError

-- Useful helper to use the interpreter as a part of the test runner.
-- We don't run runtime monitor thread here because we don't need it.
execWalletOperations
  :: forall era m resource
   . (IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (Env C.PlutusScriptV2 era resource) m)
  => InterpretState C.PlutusScriptV2 era
  -> [Wallet.WalletOperation]
  -> m
      ( Either
          ( TestRunnerError
          , InterpretState C.PlutusScriptV2 era
          )
          (InterpretState C.PlutusScriptV2 era)
      )
execWalletOperations interpretState operations =
  runExceptT
    do
      (testEnv, _) <- lift setupTestInterpretEnv
      let operations' = operations <&> WalletOperation
      stateRef <- liftIO $ newIORef interpretState
      let loop =
            unsafeExecIOStateT stateRef $
              runExceptT $
                flip runReaderT testEnv $
                  for operations' interpret
      result <- liftIO loop
      interpretState' <- liftIO $ readIORef stateRef
      case result of
        Left err ->
          throwError (TestRunnerInterpreterError err, interpretState')
        Right _ -> pure interpretState'

setupTestInterpretEnv
  :: (MonadReader (Env lang era resource) m)
  => (MonadIO m)
  => m (InterpretEnv lang era, Maybe R.RuntimeMonitor)
setupTestInterpretEnv = do
  connection <- view envConnection
  era <- view envEra
  txBuildupContext <- view envTxBuildupContext
  printStats <- view envPrintStats
  possibleRuntimeConfig <- view envRuntimeConfig
  reportDir <- view envReportDir
  runtimeSetup <-
    for
      possibleRuntimeConfig
      \RuntimeConfig{..} -> do
        let connector
              :: (Network.Protocol.Connector Marlowe.Protocol.MarloweRuntimeClient IO)
            connector =
              Network.Protocol.tcpClient
                rcRuntimeHost
                rcRuntimePort
                Marlowe.Protocol.marloweRuntimeClientPeer
            config =
              Apps.Config
                { Apps.runtimeHost = rcRuntimeHost
                , Apps.runtimePort = rcRuntimePort
                , Apps.timeoutSeconds = 900
                , Apps.buildSeconds = 3
                , Apps.confirmSeconds = 3
                , Apps.retrySeconds = 10
                , Apps.retryLimit = 5
                }
        (connector,) <$> liftIO (Runtime.Monitor.mkRuntimeMonitor config)
  protocolParams <- view envProtocolParams
  slotConfig <- view envSlotConfig
  costModelParams <- view envCostModelParams
  let protocolVersion :: P.MajorProtocolVersion =
        C.babbageEraOnwardsConstraints era $
          ledgerProtVerToPlutusMajorProtocolVersion $
            protocolParams ^. lppPParamsL . ppProtocolVersionL
      env =
        InterpretEnv
          { _ieTxBuildupContext = txBuildupContext
          , _ieConnection = connection
          , _ieCostModelParams = costModelParams
          , _ieEra = era
          , _ieProtocolVersion = protocolVersion
          , _ieSlotConfig = slotConfig
          , _iePrintStats = printStats
          , _ieRuntimeMonitor =
              do
                (_, (runtimeMonitorContracts, runtimeMonitorInput, _)) <-
                  runtimeSetup
                pure (runtimeMonitorContracts, runtimeMonitorInput)
          , _ieRuntimeClientConnector = fst <$> runtimeSetup
          , _ieReportDir = reportDir
          }
  pure $
    (env,)
      do
        (_, (_, _, runtimeMonitor)) <- runtimeSetup
        pure runtimeMonitor

-- This `IORef` implies that the test operations are executed sequentially and not in parallel.
-- Internally we call `unsafeExecIOStateT` but we don't leak this reference anywhere outside this context.
-- We use it to reference the interpreter state even in a case of a exception or a rollback.
type TestInterpreterM lang era a =
  EnvReader lang era (TestRunId, IORef (InterpretState lang era)) a

-- The resulting `Either a a` is the way to express
-- retries with short circuiting (by using `retryTillRight` somewhere below).
-- Result semantics is as follows:
--  * `Right` - close the loop immediately. It is a definite result.
--  * `Left` - possibly try again. the result is not definite and retry could change it.
interpretTest
  :: forall era
   . (IsShelleyBasedEra era)
  => [TestOperation]
  -> PrevResult (TestResult C.PlutusScriptV2 era) (TestResult C.PlutusScriptV2 era)
  -> TestInterpreterM C.PlutusScriptV2 era (Either (TestResult C.PlutusScriptV2 era) (TestResult C.PlutusScriptV2 era))
interpretTest testOperations (PrevResult possiblePrevResult) = do
  stateRef <- views envResource snd
  (testEnv, possibleRuntimeMonitor) <- setupTestInterpretEnv
  let runInterpretLoop =
        liftIO $ do
          let loop =
                unsafeExecIOStateT stateRef $
                  runExceptT $
                    flip runReaderT testEnv $
                      for testOperations \operation -> do interpret operation
          loop `catch` \(err :: SomeException) -> do
            pure $ Left $ TestExecutionFailed (show err) []
  res <-
    case possibleRuntimeMonitor of
      Just runtimeMonitor -> do
        let runRuntimeMonitor =
              liftIO
                do
                  Runtime.runMonitor runtimeMonitor `catch` \(err :: SomeException) -> do
                    pure $ RuntimeExecutionFailure (show err)
        runInterpretLoop `race` runRuntimeMonitor
      Nothing -> do
        Left <$> runInterpretLoop
  let prevFailures =
        case either id id <$> possiblePrevResult of
          Just TestSucceeded{} -> []
          Just (TestFailed failure retries) -> failure : retries
          Just (TestSkipped _) -> []
          Nothing -> []
      testFailed failure = TestFailed failure prevFailures
  state <- liftIO $ readIORef stateRef
  -- We perform retries only when meaningful rollback occurs
  -- throughout the execution (indicated by the runtime monitor) or
  -- upon `CliOperationFailed` or operation timeout.
  pure $
    case res of
      Left testInterpretResult ->
        case testInterpretResult of
          Left err@CliOperationFailed{} ->
            Left $ testFailed (FailureReport (InterpreterError err) state)
          Left err@TimeOutReached{} ->
            Left $ testFailed (FailureReport (InterpreterError err) state)
          Left err ->
            Right $ testFailed (FailureReport (InterpreterError err) state)
          Right _ -> Right $ TestSucceeded state
      Right runtimeMonitorError ->
        case runtimeMonitorError of
          runtimeError@(RuntimeRollbackError _) ->
            Left $
              testFailed (FailureReport (RuntimeMonitorError runtimeError) state)
          runtimeError ->
            Right $
              testFailed (FailureReport (RuntimeMonitorError runtimeError) state)

-- Shared counter which we can use to possibly mark stderr debug logs etc.
newtype TestRunId
  = TestRunId Int
  deriving (Eq, Ord, Show)
  deriving newtype (Enum)

-- Test runner modifies the results and some faucets (used during testing and retries).
data TestRunnerEnvResource lang era = TestRunnerEnvResource
  { _trcFaucets :: TVar [TestRunnerFaucet era]
  , _trcRunId :: TVar TestRunId
  , _trcResults :: TVar (TestsResults lang era)
  }

makeLenses ''TestRunnerEnvResource

type TestRunnerEnv lang era = Env lang era (TestRunnerEnvResource lang era)

acquireFaucet
  :: (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => m (TestRunnerFaucet era)
acquireFaucet = do
  faucetsRef <- view (envResource . trcFaucets)
  atomically $ do
    faucets <- readTVar faucetsRef
    case faucets of
      [] -> retry
      faucet : faucets' -> do
        writeTVar faucetsRef faucets'
        pure faucet

releaseFaucet
  :: (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => TestRunnerFaucet era
  -> m ()
releaseFaucet faucet = do
  faucetsRef <- view (envResource . trcFaucets)
  atomically $ modifyTVar faucetsRef (++ [faucet])

-- Resource which we acquire during the test execution is a faucet. We also
-- track all the wallets and transactions which were submitted throughout the
-- test execution.
-- The actually interpreting function has an access to the interpreter state
-- only. The acquired faucet is used later on during the cleanup phase.
type TestInterpretContext lang era =
  (IORef (InterpretState lang era), TestRunnerFaucet era)

acquireTestInterpretContext
  :: (C.IsCardanoEra era)
  => (MonadIO m)
  => (MonadReader (TestRunnerEnv lang era) m)
  => m (TestInterpretContext lang era)
acquireTestInterpretContext = do
  faucet <- acquireFaucet
  stateRef <-
    do
      fw <- toTestWallet faucet ExcludeAllTransactions
      liftIO $ newIORef (mkInterpretState $ mkWallets fw)
  pure (stateRef, faucet)

releaseTestInterpretContext
  :: (MonadIO m)
  => (IsShelleyBasedEra era)
  => (MonadReader (TestRunnerEnv C.PlutusScriptV2 era) m)
  => TestInterpretContext C.PlutusScriptV2 era
  -> m ()
releaseTestInterpretContext (stateRef, faucet) =
  -- In the last phase we execute two cleanup operations
  -- using existing test interpreter state so we extend the
  -- tx set with these two and perform a full faucet update
  -- at the end.
  do
    let operations = [Wallet.BurnAll mempty, Wallet.ReturnFunds]
    void $
      retryTillJust
        (MaxRetries 5)
        \_ -> do
          interpretState <- liftIO $ readIORef stateRef
          execWalletOperations interpretState operations >>= \case
            Left (err, interpretState') -> do
              liftIO $ hPutStrLn stderr $ "Faucet cleanup failure: " <> show err
              writeIORef stateRef interpretState'
              threadDelayBy (10 :: Second)
              pure Nothing
            Right interpretState' -> do
              writeIORef stateRef interpretState'
              pure $ Just ()
    interpretState <- liftIO $ readIORef stateRef
    let faucet' = updateFaucet faucet interpretState
    releaseFaucet faucet'

type TestRunnerM lang era a = ReaderT (TestRunnerEnv lang era) IO a

runTest
  :: forall era
   . (IsShelleyBasedEra era)
  => (FilePath, TestCase)
  -> TestRunnerM C.PlutusScriptV2 era ()
runTest (testFile, testCase@TestCase{testName, operations = testOperations}) = do
  liftIO $ hPutStrLn stderr ""
  liftIO $ hPutStrLn stderr $ "***** Test " <> coerce testName <> " *****"
  maxRetries <- view envMaxRetries
  runtimeConfigured <- asks hasRuntimeConfigured
  simulationMode <- do
    txBuildupContext <- view envTxBuildupContext
    case txBuildupContext of
      PureTxBuildup{} -> pure True
      _ -> pure False
  result <-
    if involvesRuntime testCase && (not runtimeConfigured || simulationMode)
      then
        pure
          if simulationMode
            then TestSkipped $ "In simulation mode. Skipping runtime test " <> coerce testName
            else TestSkipped $ "Runtime is not configured for the test " <> coerce testName
      else
        either id id
          <$> retryTillRight
            (MaxRetries maxRetries)
            \_ prevResult ->
              unmaskedReleaseBracket'
                acquireTestInterpretContext
                releaseTestInterpretContext
                \(stateRef, _) -> do
                  testRunIdRef <- view (envResource . trcRunId)
                  testRunId <-
                    atomically $ do
                      testRunId <- readTVar testRunIdRef
                      writeTVar testRunIdRef (succ testRunId)
                      pure testRunId
                  withReaderEnvResource
                    (testRunId, stateRef)
                    (interpretTest testOperations prevResult)
  resultsRef <- view (envResource . trcResults)
  atomically $ modifyTVar' resultsRef (Map.insert (testFile, testName) result)
  whenM
    (view envStreamJSON)
    do
      resultJson <- testResultToJSON testFile testName result
      liftIO $ putStrLn $ TL.unpack $ A.encodeToLazyText resultJson
  let printResultMsg msg =
        liftIO $
          hPutStrLn stderr $
            "***** " <> coerce testName <> ": " <> msg <> " *****"
  case result of
    TestSucceeded{} -> printResultMsg "SUCCEEDED"
    TestFailed{} -> printResultMsg "FAILED"
    TestSkipped{} -> printResultMsg "SKIPPED"

-- Internally we pass a reference to a master faucet through `bracket`
-- so we can record all the txs.
type TestSuiteRunnerInternalEnv lang era =
  Env lang era (TVar (MasterFaucet era))

acquireFaucets
  :: forall era m
   . (IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (TestSuiteRunnerInternalEnv C.PlutusScriptV2 era) m)
  => TestFaucetBudget
  -> FaucetsNumber
  -> m (TVar [TestRunnerFaucet era])
acquireFaucets (TestFaucetBudget testFaucetBudget) (FaucetsNumber faucetNumber) = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef
  wallets <-
    do
      let faucetNicknames =
            [1 .. faucetNumber] <&> \i -> WalletNickname ("Faucet-" <> show i)
          operations = do
            let createWallets =
                  faucetNicknames <&> \walletNickname ->
                    Wallet.CreateWallet walletNickname Nothing
            List.snoc
              createWallets
              (Wallet.Fund faucetNicknames [testFaucetBudget])
      fw <- toTestWallet masterFaucet ExcludeAllTransactions
      let updateMasterFaucet interpretState' =
            atomically $
              modifyTVar' masterFaucetRef $ \(MasterFaucet masterFaucet') ->
                MasterFaucet (updateFaucet masterFaucet' interpretState')
          wallets = mkWallets fw
          interpretState = mkInterpretState wallets
      execWalletOperations interpretState operations >>= \case
        Left (err, interpretState') -> do
          updateMasterFaucet interpretState'
          liftIO $
            throwIO $
              TestRunnerError $
                "Failed to create faucet wallets: " <> show err
        Right interpretState'@InterpretState{_isWallets = ws} -> do
          updateMasterFaucet interpretState'
          pure ws
  let subFaucetsWallets = nonFaucetWalletsMap wallets
      fromFreshWallet Wallet{..} = do
        unless (null _waSubmittedTransactions) $
          liftIO $
            throwIO $
              TestRunnerError
                "TestRunnerFaucet should be initialized from a fresh wallet"
        initialBalance <- toPlutusValue <$> fetchAddressTotalValue _waAddress
        pure $
          TestRunnerFaucet
            { _trAddress = _waAddress
            , _trSigningKey = _waSigningKey
            , _trInitialBalance = initialBalance
            , _trSubmittedTransactions = mempty
            , _trFundedTransactions = mempty
            }
  subFaucets <- Foldable.toList <$> for subFaucetsWallets fromFreshWallet
  liftIO $ newTVarIO subFaucets

releaseFaucets
  :: forall era m
   . (IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadReader (TestSuiteRunnerInternalEnv C.PlutusScriptV2 era) m)
  => TVar [TestRunnerFaucet era]
  -> m ()
releaseFaucets subFaucetsRef = do
  masterFaucetRef <- view envResource
  MasterFaucet masterFaucet <- liftIO $ readTVarIO masterFaucetRef
  masterFaucetWallet <- toTestWallet masterFaucet ExcludeAllTransactions
  subFaucets <- liftIO $ readTVarIO subFaucetsRef
  subFaucetsWallets <-
    fmap Map.fromList $
      for
        (zip [(1 :: Integer) ..] subFaucets)
        \(i, subFaucet) ->
          -- We include all the transactions because we want to update the master wallet using the state.
          do
            subFaucetWallet <- toTestWallet subFaucet IncludeAllTransactions
            pure (WalletNickname ("Faucet-" <> show i), subFaucetWallet)
  let wallets = Wallets subFaucetsWallets masterFaucetWallet
      interpretState = mkInterpretState wallets
      operations = [Wallet.ReturnFunds]
  -- FIXME: This loop should be part of the `ReturnFund` interpreter
  interpretState' <-
    fmap (either id id) $
      retryTillRight
        (MaxRetries 5)
        \_ (PrevResult res) ->
          -- We pass the interpret state from the previous iteration so we keep track of all the txs.
          do
            let interpretState' = maybe interpretState (either id id) res
            execWalletOperations interpretState' operations >>= \case
              Left (err, interpretState'') -> do
                liftIO $
                  hPutStrLn stderr "Failed to return funds to the master wallet"
                liftIO $ hPrint stderr err
                -- Should we throw here or only add to the test report this failure?
                -- liftIO $ throwIO $ TestRunnerError $ "Failed to return funds to the master wallet: " <> show err
                threadDelayBy (10 :: Second)
                pure $ Left interpretState''
              Right interpretState'' ->
                -- Let's update the master faucet transactions set with the funding one.
                do
                  pure $ Right interpretState''
  atomically $
    modifyTVar' masterFaucetRef $ \(MasterFaucet masterFaucet') ->
      MasterFaucet (updateFaucet masterFaucet' interpretState')

-- | Types which are part of public facing API
type TestSuiteRunnerEnv lang era =
  Env lang era (AddressInEra era, SomePaymentSigningKey)

type TestSuiteRunnerM lang era a = ReaderT (TestSuiteRunnerEnv lang era) IO a

fetchBalance
  :: (MonadReader (Env lang era resource) m)
  => (MonadIO m)
  => AddressInEra era
  -> m P.Value
fetchBalance address = do
  connection <- view envConnection
  C.UTxO utxo <- runCli $ queryUtxos connection address
  pure $ foldMap (toPlutusValue . txOutValueValue) (Map.elems utxo)

mkMasterFaucet
  :: (MonadReader (TestSuiteRunnerEnv lang era) m)
  => (MonadIO m)
  => m (MasterFaucet era)
mkMasterFaucet = do
  (address, signingKey) <- view envResource
  total <- fetchBalance address
  pure $
    MasterFaucet $
      TestRunnerFaucet
        { _trAddress = address
        , _trSigningKey = signingKey
        , _trInitialBalance = total
        , _trSubmittedTransactions = []
        , _trFundedTransactions = []
        }

data MasterFaucetInfo = MasterFaucetInfo
  { _mfiInitialBalance :: P.Value
  , _mfiCurrentBalance :: P.Value
  , _mfiSubmittedTransactions :: [SomeTxBody]
  , _mfiFundedTransactions :: [SomeTxBody]
  }

masterFaucetInfo :: MasterFaucetInfo -> A.Value
masterFaucetInfo MasterFaucetInfo{..} =
  A.object
    [ "initialBalance" .= plutusValueToJSON _mfiInitialBalance
    , "currentBalance" .= plutusValueToJSON _mfiCurrentBalance
    , "totalKnownFees" .= do
        foldMap (overSomeTxBody' txBodyFee) _mfiSubmittedTransactions
          <> foldMap (overSomeTxBody' txBodyFee) _mfiFundedTransactions
    , "submittedTxs" .= (_mfiSubmittedTransactions <&> someTxBodyToJSON)
    , "fundedTxs" .= (_mfiFundedTransactions <&> someTxBodyToJSON)
    ]

data TestSuiteResult lang era = TestSuiteResult
  { _tsResult :: TestsResults lang era
  , _tsMasterFaucetInfo :: MasterFaucetInfo
  }

testSuiteResultToJSON
  :: (C.IsCardanoEra era)
  => (MonadIO m)
  => TestSuiteResult lang era
  -> m A.Value
testSuiteResultToJSON TestSuiteResult{..} = do
  resultsJSON <-
    for
      (Map.toList _tsResult)
      \((testFile, testName), result) ->
        testResultToJSON testFile testName result
  pure $
    A.object
      [ "results" .= resultsJSON
      , "masterFaucetInfo" A..= masterFaucetInfo _tsMasterFaucetInfo
      ]

-- It is a bit surprising but the standard bracketing fails (hangs)
-- when we have to perform node queries during the release action.
unmaskedReleaseBracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
unmaskedReleaseBracket before after thing = do
  a <- before
  r <- thing a `onException` after a
  _ <- after a
  return r

unmaskedReleaseBracket'
  :: (MonadUnliftIO m) => m a -> (a -> m b) -> (a -> m c) -> m c
unmaskedReleaseBracket' before after thing =
  withRunInIO
    \run -> do unmaskedReleaseBracket (run before) (run . after) (run . thing)

makeLenses ''TestSuiteResult

runTests
  :: forall era
   . (IsShelleyBasedEra era)
  => [(FilePath, TestCase)]
  -> MaxConcurrentRunners
  -> TestSuiteRunnerM C.PlutusScriptV2 era (TestSuiteResult C.PlutusScriptV2 era)
runTests tests (MaxConcurrentRunners maxConcurrentRunners) = do
  protocolParams <- view envProtocolParams
  era <- view envEra
  let concurrentRunners = min maxConcurrentRunners (length tests)
      -- Our coin selection algorithm bounds the tx fees using this 2x factor.
      txCosts =
        TxCostsUpperBounds
          (2 * (C.babbageEraOnwardsConstraints era $ maximumFee protocolParams))
          (Ledger.Coin 100_000_000)
      subFaucetBudget = testsFaucetBudgetUpperBound txCosts (map snd tests)
      requiredFunds =
        lovelaceFromInt (lovelaceToInt subFaucetBudget * concurrentRunners)
      totalTxCost = sum (testTxsFeesUpperBound txCosts <$> map snd tests)
      acquireFaucets' =
        acquireFaucets
          (TestFaucetBudget subFaucetBudget)
          (FaucetsNumber concurrentRunners)
      showAdaAmount lovelaceAmount =
        show (toInteger lovelaceAmount `div` 1_000_000) <> " ADA"
  liftIO
    do
      hPutStrLn stderr $
        "Estimated required funds: "
          <> showAdaAmount requiredFunds
      hPutStrLn stderr $
        "Estimated subFaucet budget: "
          <> showAdaAmount subFaucetBudget
      hPutStrLn stderr $
        "Estimated test total tx fees: "
          <> showAdaAmount totalTxCost
      hPutStrLn stderr $ "Concurrent runners: " <> show concurrentRunners
      for_ tests $ \(testFile, operations) -> do
        let budgetUpperBound = testFaucetBudgetUpperBound txCosts operations
            txsFeesUpperBound = testTxsFeesUpperBound txCosts operations
        hPutStrLn stderr $ testFile <> ":"
        hPutStrLn stderr $
          "Estimated test budget: "
            <> showAdaAmount budgetUpperBound
        hPutStrLn stderr $
          "Estimated test tx fees: "
            <> showAdaAmount txsFeesUpperBound
  masterFaucet <- mkMasterFaucet
  masterFaucetRef <- liftIO $ newTVarIO masterFaucet
  resultsRef <- liftIO $ newTVarIO mempty
  void $
    withReaderEnvResource masterFaucetRef $
      unmaskedReleaseBracket'
        acquireFaucets'
        releaseFaucets
        \subFaucetsRef -> do
          runCounterRef <- liftIO $ newTVarIO (TestRunId 1)
          let testRunnerEnvResource =
                TestRunnerEnvResource
                  { _trcFaucets = subFaucetsRef
                  , _trcRunId = runCounterRef
                  , _trcResults = resultsRef
                  }
          withReaderEnvResource testRunnerEnvResource $
            UnliftIO.withTaskGroup
              concurrentRunners
              \taskGroup -> do UnliftIO.mapTasksE taskGroup $ fmap runTest tests
  masterFaucet' <- liftIO $ readTVarIO masterFaucetRef
  currentBalance <- fetchBalance (masterFaucet' ^. (mfFaucet . trAddress))
  let masterFaucetInfo' =
        MasterFaucetInfo
          { _mfiInitialBalance = masterFaucet' ^. (mfFaucet . trInitialBalance)
          , _mfiCurrentBalance = currentBalance
          , _mfiSubmittedTransactions =
              masterFaucet' ^. (mfFaucet . trSubmittedTransactions)
          , _mfiFundedTransactions =
              masterFaucet' ^. (mfFaucet . trFundedTransactions)
          }
  result <- liftIO $ readTVarIO resultsRef
  pure $ TestSuiteResult result masterFaucetInfo'
