{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.CLI.Test.Types where

import Cardano.Api (AddressInEra, BabbageEraOnwards, IsCardanoEra, LocalNodeConnectInfo, NetworkId)
import Cardano.Api qualified as C
import Marlowe.Contrib.Foldable (foldMapM)
import Contrib.Data.Time.Units.Aeson qualified as A
import Control.Category ((<<<))
import Control.Lens (makeLenses, view, (^.), _1, _2, _Just)
import Control.Lens.Getter qualified as Getter
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON, ToJSON (..), (.=))
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as A
import Data.Aeson.Key qualified as A.Key
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as A
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString.Lazy qualified as BSL
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.CLI.Types (CLIContracts (CLIContracts), CLIOperation, HasInterpretEnv (..))
import Language.Marlowe.CLI.Test.CLI.Types qualified as CLI
import Language.Marlowe.CLI.Test.Contract.ContractNickname (ContractNickname (ContractNickname))
import Language.Marlowe.CLI.Test.ExecutionMode (HasInterpretEnv (..), UseExecutionMode)
import Language.Marlowe.CLI.Test.ExecutionMode qualified as ExecutionMode
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError, ieMessage)
import Language.Marlowe.CLI.Test.Log (HasInterpretEnv (..), Logs)
import Language.Marlowe.CLI.Test.Log qualified as Log
import Language.Marlowe.CLI.Test.Operation.Aeson (
  ConstructorName (ConstructorName),
  preprocessConstructorJSON,
  rewriteToSingleFieldConstructor,
 )
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Runtime.Types (RuntimeMonitorInput, RuntimeMonitorState, RuntimeOperation)
import Language.Marlowe.CLI.Test.Runtime.Types qualified as Runtime
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currencies (Currencies, unCurrencies),
  Currency (..),
  HasInterpretEnv (..),
  SomeTxBody,
  Wallet (..),
  allWalletsMap,
  overSomeTxBody',
  txBodyFee,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (MarloweScriptsRefs, PrintStats, TxBuildupContext, toQueryContext)
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Network.Protocol.Connection qualified as Network.Protocol
import Network.Socket (PortNumber)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.V1.Value qualified as P.Value
import PlutusLedgerApi.V2 qualified as P
import PlutusTx.AssocMap qualified as P.AssocMap
import System.IO.Temp (emptySystemTempFile)

data RuntimeConfig = RuntimeConfig
  { rcRuntimeHost :: String
  , rcRuntimePort :: PortNumber
  }
  deriving stock (Eq, Generic, Show)

newtype MaxConcurrentRunners = MaxConcurrentRunners Int
  deriving stock (Eq, Generic, Show)

data ReportingStrategy
  = StreamJSON
  | WriteJSONFile FilePath
  | StreamAndWriteJSONFile FilePath
  deriving stock (Eq, Generic, Show)

data TestSuite era a = TestSuite
  { tsNetwork :: NetworkId
  -- ^ The network ID, if any.
  , tsSocketPath :: FilePath
  -- ^ The path to the node socket.
  , tsFaucetSigningKeyFile :: FilePath
  -- ^ The file containing the faucet's signing key.
  , tsFaucetAddress :: AddressInEra era
  -- ^ The faucet address.
  , tsUseExecutionMode :: UseExecutionMode
  , tsTests :: [a]
  -- ^ Input for the tests.
  , tsRuntime :: RuntimeConfig
  , tsConcurrentRunners :: MaxConcurrentRunners
  , tsReportingStrategy :: Maybe ReportingStrategy
  , tsMaxRetries :: Int
  }

--  deriving stock (Eq, Generic, Show)

newtype TestName = TestName String
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON TestName where
  parseJSON json = TestName <$> A.parseJSON json

instance ToJSON TestName where
  toJSON (TestName name) = A.toJSON name

-- | An on-chain test of the Marlowe contract and payout validators.
data TestCase = TestCase
  { testName :: TestName
  -- ^ The name of the test.
  , operations :: [TestOperation]
  -- ^ The sequence of test operations.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON) -- , ToJSON)

data FailureError
  = InterpreterError InterpreterError
  | RuntimeMonitorError Runtime.RuntimeError
  deriving stock (Eq, Generic, Show)

instance A.FromJSON FailureError where
  parseJSON = Operation.parseSingleFieldConstructorBasedJSON

instance A.ToJSON FailureError where
  toJSON = Operation.toSingleFieldConstructorBasedJSON

data FailureReport lang era = FailureReport
  { _frErr :: FailureError
  , _frInterpretState :: InterpretState lang era
  }

data TestResult lang era
  = TestSucceeded (InterpretState lang era)
  | TestSkipped String
  | TestFailed
      { result :: FailureReport lang era
      , retries :: [FailureReport lang era]
      }

testResultLogs :: TestResult lang era -> Maybe Logs
testResultLogs = \case
  TestSucceeded InterpretState{_isLogs} -> Just _isLogs
  TestSkipped _ -> Nothing
  TestFailed FailureReport{_frInterpretState = InterpretState{_isLogs}} _ -> Just _isLogs

data TestOperation
  = CLIOperation CLIOperation
  | RuntimeOperation RuntimeOperation
  | WalletOperation Wallet.WalletOperation
  | Fail String
  | Sleep A.Second
  | Comment String
  | ShouldFail TestOperation
  deriving stock (Eq, Generic, Show)

isRuntimeOperation :: TestOperation -> Bool
isRuntimeOperation = \case
  RuntimeOperation _ -> True
  _ -> False

involvesRuntime :: TestCase -> Bool
involvesRuntime = any isRuntimeOperation . operations

-- | FIXME: Constructor names could be generically derived from the types.
cliConstructors :: [String]
cliConstructors =
  [ "AutoRun"
  , "Initialize"
  , "Prepare"
  , "Publish"
  , "Withdraw"
  ]

runtimeConstructors :: [String]
runtimeConstructors =
  [ "RuntimeAwaitClosed"
  , "RuntimeInitContract"
  , "RuntimeApplyInputs"
  , "RuntimeWithdraw"
  ]

walletConstructors :: [String]
walletConstructors =
  [ "BurnAll"
  , "CreateWallet"
  , "CheckBalance"
  , "Fund"
  , "Mint"
  , "SplitWallet"
  , "ReturnFunds"
  , "ExternalCurrency"
  , "ExternalWallet"
  ]

instance FromJSON TestOperation where
  parseJSON json = do
    let rewriteSubOperation = preprocessConstructorJSON \constructorName _ -> case constructorName of
          ConstructorName cn | cn `List.elem` cliConstructors -> Just $ A.object [("tag", A.String "CLIOperation"), ("contents", json)]
          ConstructorName cn | cn `List.elem` runtimeConstructors -> Just $ A.object [("tag", A.String "RuntimeOperation"), ("contents", json)]
          ConstructorName cn | cn `List.elem` walletConstructors -> Just $ A.object [("tag", A.String "WalletOperation"), ("contents", json)]
          _ -> Nothing
        rewriteSleep = rewriteToSingleFieldConstructor (ConstructorName "Sleep")
        rewriteFail = rewriteToSingleFieldConstructor (ConstructorName "Fail")
        rewriteComment = rewriteToSingleFieldConstructor (ConstructorName "Comment")
        rewriteShouldFail = rewriteToSingleFieldConstructor (ConstructorName "ShouldFail")

        preprocess = rewriteSubOperation <> rewriteSleep <> rewriteFail <> rewriteComment <> rewriteShouldFail

    Operation.parseConstructorBasedJSON "-" preprocess json

instance ToJSON TestOperation where
  toJSON (CLIOperation op) = toJSON op
  toJSON (RuntimeOperation op) = toJSON op
  toJSON (WalletOperation op) = toJSON op
  toJSON (Fail msg) = A.object [("Fail", toJSON msg)]
  toJSON (Sleep seconds) = A.object [("Sleep", toJSON seconds)]
  toJSON (Comment comment) = A.object [("Comment", toJSON comment)]
  toJSON (ShouldFail op) = A.object [("ShouldFail", toJSON op)]

-- Let's serialize it to object of objects:
plutusValueToJSON :: P.Value -> A.Value
plutusValueToJSON (P.Value value) = do
  let -- `AssocMap` has not `Functor` instance
      -- so we have to convert to list it and then map
      -- over it.
      value' = fmap (fmap P.AssocMap.toList) $ P.AssocMap.toList value
  A.object $
    value' <&> \(cs, inner) -> do
      let inner' = A.object $ bimap (A.Key.fromString . P.Value.toString) A.toJSON <$> inner
      (A.Key.fromString . show $ cs, inner')

data InterpretState lang era = InterpretState
  { -- We should flatten these two contractInfo representations.
    _isCLIContracts :: CLIContracts lang era
  , _isKnownContracts :: Map ContractNickname Runtime.ContractInfo
  , _isPublishedScripts :: Maybe (MarloweScriptsRefs lang era)
  , _isCurrencies :: Wallet.Currencies
  , _isWallets :: Wallet.Wallets era
  , _isLogs :: Logs
  }

mkInterpretState :: Wallet.Wallets era -> InterpretState lang era
mkInterpretState wallets =
  InterpretState
    { _isCLIContracts = CLIContracts mempty
    , _isPublishedScripts = Nothing
    , _isCurrencies = Currencies mempty
    , _isWallets = wallets
    , _isKnownContracts = mempty
    , _isLogs = mempty
    }

data InterpretEnv lang era = InterpretEnv
  { _ieRuntimeMonitor :: Maybe (RuntimeMonitorInput, RuntimeMonitorState)
  , _ieRuntimeClientConnector :: Maybe (Network.Protocol.Connector Marlowe.Protocol.MarloweRuntimeClient IO)
  , _ieConnection :: LocalNodeConnectInfo
  , _ieEra :: BabbageEraOnwards era
  , _iePrintStats :: PrintStats
  , _ieSlotConfig :: SlotConfig
  , _ieCostModelParams :: [Integer]
  , _ieProtocolVersion :: MajorProtocolVersion
  , _ieTxBuildupContext :: TxBuildupContext era
  , _ieReportDir :: FilePath
  }

type InterpretMonad m lang era =
  ( MonadState (InterpretState lang era) m
  , MonadReader (InterpretEnv lang era) m
  , MonadError InterpreterError m
  , MonadIO m
  )

makeLenses 'InterpretEnv
makeLenses 'InterpretState

instance Log.HasInterpretEnv (InterpretEnv lang era) era where
  eraL = ieEra
  queryCtxL = Getter.to $ toQueryContext . view ieTxBuildupContext
  reportDirL = ieReportDir

instance Log.HasInterpretState (InterpretState lang era) where
  logStoreL = isLogs

instance Wallet.HasInterpretEnv (InterpretEnv lang era) era where
  eraL = ieEra
  printStatsL = iePrintStats
  txBuildupContextL = ieTxBuildupContext

instance Wallet.HasInterpretState (InterpretState lang era) era where
  walletsL = isWallets
  currenciesL = isCurrencies

instance CLI.HasInterpretEnv (InterpretEnv lang era) lang era where
  connectionL = ieConnection
  eraL = ieEra
  printStatsL = iePrintStats
  slotConfigL = ieSlotConfig
  costModelParamsL = ieCostModelParams
  protocolVersionL = ieProtocolVersion
  txBuildupContextL = ieTxBuildupContext

instance CLI.HasInterpretState (InterpretState lang era) lang era where
  walletsL = isWallets
  currenciesL = isCurrencies
  contractsL = isCLIContracts
  publishedScriptsL = isPublishedScripts

instance Runtime.HasInterpretState (InterpretState lang era) era where
  knownContractsL = isKnownContracts
  walletsL = isWallets
  currenciesL = isCurrencies

instance Runtime.HasInterpretEnv (InterpretEnv lang era) era where
  runtimeMonitorStateT = ieRuntimeMonitor <<< _Just <<< _2
  runtimeMonitorInputT = ieRuntimeMonitor <<< _Just <<< _1
  runtimeClientConnectorT = ieRuntimeClientConnector . _Just
  connectionT = ieConnection
  slotConfigL = ieSlotConfig
  eraL = ieEra
  txBuildupContextL = ieTxBuildupContext

instance ExecutionMode.HasInterpretEnv (InterpretEnv lang era) era where
  eraL = ieEra
  txBuildupContextL = ieTxBuildupContext

-- txId = overSomeTxBody (const C.getTxId)
someTxBodyToJSON :: SomeTxBody -> A.Value
someTxBodyToJSON someTxBody =
  A.object
    [ "txId" .= overSomeTxBody' C.getTxId someTxBody
    , "txFee" .= overSomeTxBody' txBodyFee someTxBody
    ]

walletToJSON :: (IsCardanoEra era) => Wallet era -> A.Value
walletToJSON Wallet{..} =
  A.object
    [ "address" .= _waAddress
    , "balanceCheckBaseline" .= plutusValueToJSON _waBalanceCheckBaseline
    , "txs" .= (_waSubmittedTransactions <&> someTxBodyToJSON)
    ]

currencyToJSON :: Currency -> A.Value
currencyToJSON Currency{..} =
  A.object
    [ "issuer" .= ccIssuer
    , "policyId" .= ccPolicyId
    ]

contractInfoToJSON :: (MonadIO m) => ContractNickname -> Runtime.ContractInfo -> m [A.Pair]
contractInfoToJSON (ContractNickname nickname) Runtime.ContractInfo{..} = do
  contractFile <- liftIO do
    let filenameBase = "contract-" <> nickname <> ".json"
    filename <- emptySystemTempFile filenameBase
    BSL.writeFile filename $ A.encodePretty _ciContract
    pure filename
  continuationsFile <- for _ciContinuations \continuations -> do
    let filenameBase = "contract-" <> nickname <> "-continuations.json"
    liftIO $ do
      filename <- emptySystemTempFile filenameBase
      BSL.writeFile filename $ A.encodePretty continuations
      pure filename
  pure
    [
      ( Key.fromString nickname
      , A.object
          [ ("contractId", A.toJSON _ciContractId)
          , ("contractFile", A.toJSON contractFile)
          , ("continuationsFile", A.toJSON continuationsFile)
          , ("roleCurrency", A.toJSON _ciRoleCurrency)
          ]
      )
    ]

interpretStateToJSONPairs
  :: forall a era lang m
   . (MonadIO m)
  => (IsCardanoEra era)
  => (Aeson.KeyValue Aeson.Value a)
  => InterpretState lang era
  -> m [a]
interpretStateToJSONPairs InterpretState{..} = do
  contractsJSON <- A.object <$> foldMapM (uncurry contractInfoToJSON) (Map.toList _isKnownContracts)
  pure
    [ "wallets" .= do
        let step n wallet = [(n, walletToJSON wallet)]
        Map.foldMapWithKey step $ allWalletsMap _isWallets
    , "currencies" .= do
        let step n curr = [(n, currencyToJSON curr)]
        Map.foldMapWithKey step $ unCurrencies _isCurrencies
    , "contracts" .= contractsJSON
    ]

failureReportToJSON
  :: forall era lang m
   . (IsCardanoEra era)
  => (MonadIO m)
  => FilePath
  -> TestName
  -> FailureReport lang era
  -> [FailureReport lang era]
  -> m Aeson.Value
failureReportToJSON testFile (TestName name) failure@FailureReport{_frInterpretState = interpretState@InterpretState{..}} retries = do
  let err = _frErr failure
      -- It is a bit easier to read the log when we have the error message itself at the end.
      logs = case err of
        InterpreterError err' -> reverse (("Error", err' ^. ieMessage, []) : _isLogs)
        _ -> reverse _isLogs
  statePairs <- interpretStateToJSONPairs interpretState
  pure $
    A.object $
      [ "name" .= name
      , "testSource" .= testFile
      , "result" A..= ("failed" :: String)
      , "error" .= err
      , "retries" .= fmap _frErr retries
      , "logs" .= logsToJSON logs
      ]
        <> statePairs

logsToJSON :: Logs -> [A.Value]
logsToJSON logs = logs <&> \(l, msg, info) -> A.object ["label" .= l, "message" .= msg, "info" .= A.object info]

testResultToJSON
  :: (IsCardanoEra era)
  => (MonadIO m)
  => FilePath
  -> TestName
  -> TestResult lang era
  -> m Aeson.Value
testResultToJSON testFile testName@(TestName name) = \case
  TestSucceeded i@InterpretState{_isLogs} -> do
    statePairs <- interpretStateToJSONPairs i
    pure $
      A.object $
        [ "testName" A..= name
        , "testSource" A..= testFile
        , "result" A..= ("passed" :: String)
        , "logs" A..= logsToJSON _isLogs
        ]
          <> statePairs
  TestSkipped msg ->
    pure $
      A.object
        [ "testName" A..= name
        , "testSource" A..= testFile
        , "result" A..= ("skipped" :: String)
        , "reason" A..= msg
        ]
  TestFailed err retries -> failureReportToJSON testFile testName err retries

newtype FaucetsNumber = FaucetsNumber Int

makeLenses 'FailureReport
