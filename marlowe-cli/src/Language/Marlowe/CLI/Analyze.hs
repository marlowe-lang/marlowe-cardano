-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Analyze Marlowe contracts.
module Language.Marlowe.CLI.Analyze (
  -- * Types
  ContractInstance (..),

  -- * Analysis
  analyze,
  checkExecutionCost,
  calcMarloweTxExBudgets,
) where

import Control.Monad (guard, liftM2, (<=<))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (object, (.=))
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (maximumBy, nub, (\\))
import Data.Maybe (catMaybes, isJust)
import Data.String (IsString (..))
import Marlowe.Plutus.Analysis.Safety.Ledger (worstValueSize)
import Marlowe.Plutus.Analysis.Safety.Transaction (
  CurrentState (AlreadyInitialized),
  LockedRoles (..),
  MarloweExBudget (..),
  UseReferenceInput (..),
  calcMarloweTxExBudget,
  executeTransaction,
  findTransactions,
  foldTransactionsM,
  inputsRequiredRoles,
  unitAnnotator,
 )
import Marlowe.Plutus.Analysis.Safety.Types (Transaction (..))
import Language.Marlowe.CLI.Cardano.Api (babbageEraOnwardsToAlonzoEraOnwards, toTxOutDatumInTx)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (toScriptLanguageInEra)
import Language.Marlowe.CLI.IO (decodeFileStrict, liftCli, liftCliExceptT, liftCliMaybe)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress, toCardanoAddressInEra, toCardanoValue)
import Language.Marlowe.CLI.Types (
  CliError (CliError),
  MarloweTransaction (
    MarloweTransaction,
    mtContinuations,
    mtContract,
    mtInputs,
    mtOpenRoleValidator,
    mtPayments,
    mtRange,
    mtRoleValidator,
    mtRolesCurrency,
    mtSlotConfig,
    mtState,
    mtValidator
  ),
  SomeMarloweTransaction (SomeMarloweTransaction),
  ValidatorInfo (..),
  validatorInfoScriptOrReference,
 )
import Marlowe.Plutus.Merkle (Continuations, MerkleizedContract (..))
import Marlowe.Plutus.Plate
import Marlowe.Plutus.Semantics (
  MarloweData (MarloweData, marloweContract, marloweParams, marloweState),
  MarloweParams (..),
  Payment (Payment),
  TransactionInput (..),
  TransactionOutput (Error, TransactionOutput, txOutContract, txOutPayments, txOutState, txOutWarnings),
  totalBalance,
 )
import Marlowe.Plutus.Semantics.Types (
  ChoiceId (ChoiceId),
  Contract (..),
  Input (MerkleizedInput),
  InputContent (IChoice, IDeposit),
  Party (Address, Role),
  Payee (Party),
  State (..),
  Token (..),
  getInputContent,
 )
import Marlowe.Plutus.Semantics.Types.Address (mainnet)

import Cardano.Api (unsafeHashableScriptData)
import Cardano.Api qualified as C
import Cardano.Api.Serialise.Raw (deserialiseFromRawBytesHex)
import Cardano.Api.Ledger (ppProtocolVersionL)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api qualified as Api
import Cardano.Ledger.Alonzo.Core (ppCostModelsL, ppMaxTxSizeL, ppMaxValSizeL)
import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus qualified as P
import Contrib.Cardano.Api (ledgerProtVerToPlutusMajorProtocolVersion, lppPParamsL)
import Control.Monad.Writer (WriterT (..))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A (Pair)
import Data.ByteString qualified as BS (length)
import Data.ByteString.Char8 qualified as BS8 (putStr, pack)
import Data.Foldable.Extra (foldlM)
import Data.Map.Strict qualified as M (lookup)
import Data.SatInt (SatInt (..))
import Data.Set qualified as S (filter, member)
import Data.Word (Word32)
import Data.Yaml qualified as Y (encode)
import GHC.Natural (naturalToInteger)
import Language.Marlowe.CLI.Transaction (babbageEraOnwardsToAllegraEraOnwards, mkTxOutValue)
import Lens.Micro ((^.))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))
import Plutus.V1.Ledger.Ada qualified as P (lovelaceValueOf)
import Plutus.V1.Ledger.SlotConfig qualified as P (SlotConfig, posixTimeToEnclosingSlot)
import PlutusLedgerApi.V2 (deserialiseScript)
import PlutusLedgerApi.V2 qualified as PV2 hiding (evaluateScriptCounting)
import PlutusTx (toBuiltinData)
import PlutusTx.AssocMap qualified as AM
import PlutusTx.Prelude qualified as P
import Marlowe.Plutus.Scripts.Types (marloweTxInputsFromInputs)

-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyze
  :: forall m
   . (MonadError CliError m)
  => (MonadIO m)
  => Api.LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> Bool
  -- ^ Whether to check preconditions for Marlowe state.
  -> Bool
  -- ^ Whether to check lengths of role names.
  -> Bool
  -- ^ Whether to check lengths of token names.
  -> Bool
  -- ^ Whether to check the `maxValueSize` protocol limit.
  -> Bool
  -- ^ Whether to check the `utxoCostPerWord` protocol limit.
  -> Bool
  -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
  -> Bool
  -- ^ Whether to check the `maxTxSize` protocol limits.
  -> Bool
  -- ^ Whether to compute tight estimates of worst-case bounds.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m ()
  -- ^ Print estimates of worst-case bounds.
analyze connection marloweFile preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best verbose =
  do
    SomeMarloweTransaction _ era marlowe <- decodeFileStrict marloweFile
    do
      protocol <-
        (liftCli <=< liftCliExceptT)
          . Api.queryNodeLocalState connection VolatileTip
          . Api.QueryInEra
          $ Api.QueryInShelleyBasedEra (Api.babbageEraOnwardsToShelleyBasedEra era) Api.QueryProtocolParameters
      result <-
        analyzeImpl
          era
          (Api.LedgerProtocolParameters protocol)
          marlowe
          preconditions
          roles
          tokens
          maximumValue
          minimumUtxo
          executionCost
          transactionSize
          best
          verbose
      liftIO . BS8.putStr $ Y.encode result

-- | A bundle of information describing a contract instance.
data ContractInstance lang era = ContractInstance
  { ciRolesCurrency :: PV2.CurrencySymbol
  -- ^ The roles currency.
  , ciState :: State
  -- ^ The initial state of the contract.
  , ciContract :: Contract
  -- ^ The initial contract.
  , ciContinuations :: Continuations
  -- ^ The merkleized continuations for the contract.
  , ciSemanticsValidator :: ValidatorInfo lang era
  -- ^ The semantics validator.
  , ciPayoutValidator :: ValidatorInfo lang era
  -- ^ The payout validator.
  , ciOpenRoleValidator :: ValidatorInfo lang era
  -- ^ The open role validator.
  , ciSlotConfig :: P.SlotConfig
  -- ^ The slot configuration.
  }
  deriving (Show)

-- | Analyze a Marlowe contract for protocol-limit or other violations.
analyzeImpl
  :: forall m lang era
   . (C.IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.BabbageEraOnwards era
  -- ^ The era.
  -> Api.LedgerProtocolParameters era
  -- ^ The connection info for the local node.
  -> MarloweTransaction lang era
  -- ^ The the Marlowe inputs, final state, and final contract.
  -> Bool
  -- ^ Whether to check preconditions for Marlowe state.
  -> Bool
  -- ^ Whether to check lengths of role names.
  -> Bool
  -- ^ Whether to check lengths of token names.
  -> Bool
  -- ^ Whether to check the `maxValueSize` protocol limit.
  -> Bool
  -- ^ Whether to check the `utxoCostPerWord` protocol limit.
  -> Bool
  -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
  -> Bool
  -- ^ Whether to check the `maxTxSize` protocol limits.
  -> Bool
  -- ^ Whether to compute tight estimates of worst-case bounds.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action for finding estimates of worst-case bounds.
analyzeImpl era protocol MarloweTransaction{..} preconditions roles tokens maximumValue minimumUtxo executionCost transactionSize best verbose =
  do
    let checkAll = not $ preconditions || roles || tokens || maximumValue || minimumUtxo || executionCost || transactionSize
        ci =
          ContractInstance
            mtRolesCurrency
            mtState
            mtContract
            mtContinuations
            mtValidator
            mtRoleValidator
            mtOpenRoleValidator
            mtSlotConfig
    transactions <-
      if checkAll || executionCost || transactionSize || best && (maximumValue || minimumUtxo)
        then findTransactions unitAnnotator True (MerkleizedContract mtContract mtContinuations) (AlreadyInitialized mtState)
        else pure mempty
    let perhapsTransactions = if best then Right transactions else Left ci
        guardValue condition x =
          if condition
            then Just <$> x
            else pure Nothing
    A.toJSON
      . catMaybes
      <$> sequence
        [ guardValue (preconditions || checkAll)
            . pure
            $ checkPreconditions ci
        , guardValue (roles || checkAll)
            . pure
            $ checkRoles ci
        , guardValue (tokens || checkAll)
            . pure
            $ checkTokens ci
        , guardValue (maximumValue || checkAll) $
            checkMaximumValue era protocol perhapsTransactions verbose
        , guardValue (minimumUtxo || checkAll) $
            Api.shelleyBasedEraConstraints (Api.babbageEraOnwardsToShelleyBasedEra era) $
              checkMinimumUtxo era protocol perhapsTransactions verbose
        , guardValue (executionCost || checkAll) $
            checkExecutionCost era protocol ci transactions verbose
        , guardValue (transactionSize || checkAll) $
            checkTransactionSizes era protocol ci transactions verbose
        ]

-- | Report invalid properties in the Marlowe state:
--
--   * Invalid roles currency.
--   * Invalid currency symbol or token name in an account.
--   * Account balances that are not positive.
--   * Duplicate accounts, choices, or bound values.
checkPreconditions
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ A report on preconditions.
checkPreconditions ContractInstance{ciRolesCurrency, ciState = State{..}} =
  let nonPositiveBalances = filter ((<= 0) . snd) . AM.toList
      duplicates (AM.keys -> x) = x \\ nub x
   in putJson
        "Preconditions"
        [ "Invalid roles currency" .= invalidCurrency ciRolesCurrency
        , "Invalid account tokens" .= filter invalidToken (snd <$> AM.keys accounts)
        , "Invalid account parties" .= filter invalidParty (fst <$> AM.keys accounts)
        , "Invalid choice parties" .= filter invalidChoiceParty (AM.keys choices)
        , "Non-positive account balances" .= nonPositiveBalances accounts
        , "Duplicate accounts" .= duplicates accounts
        , "Duplicate choices" .= duplicates choices
        , "Duplicate bound values" .= duplicates boundValues
        ]

-- | Detect an invalid currency symbol.
invalidCurrency :: PV2.CurrencySymbol -> Bool
invalidCurrency currency@PV2.CurrencySymbol{..} = currency /= PV2.adaSymbol && P.lengthOfByteString unCurrencySymbol /= 28

-- | Detect an invalid token.
invalidToken :: Token -> Bool
invalidToken (Token currency@PV2.CurrencySymbol{..} token@PV2.TokenName{..}) =
  not $
    currency == PV2.adaSymbol
      && token == PV2.adaToken
      || P.lengthOfByteString unCurrencySymbol == 28
        && P.lengthOfByteString unTokenName <= 32

-- | Detect an invalid party in a choice.
invalidChoiceParty :: ChoiceId -> Bool
invalidChoiceParty (ChoiceId _ (Role role)) = invalidRole role
invalidChoiceParty _ = False

-- | Detect an invalid party.
invalidParty :: Party -> Bool
invalidParty (Role role) = invalidRole role
invalidParty _ = False

-- | Detect an invalid role name.
invalidRole :: PV2.TokenName -> Bool
invalidRole PV2.TokenName{..} = P.lengthOfByteString unTokenName > 32

-- | Check that all tokens are valid.
checkTokens
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ A report on token validity.
checkTokens ContractInstance{..} =
  putJson
    "Tokens"
    [ "Invalid tokens" .= invalidToken `S.filter` extractAllWithContinuations ciContract ciContinuations
    ]

-- | Check that all roles are valid.
checkRoles
  :: ContractInstance lang era
  -- ^ The bundle of contract information.
  -> A.Value
  -- ^ Action to print a report on role-name validity.
checkRoles ContractInstance{..} =
  let roles = extractAllWithContinuations ciContract ciContinuations
   in putJson
        "Role names"
        [ "Invalid role names" .= invalidRole `S.filter` roles
        , "Blank role names" .= PV2.adaToken `S.member` roles
        ]

-- | Check that the protocol limit on maximum value in a UTxO is not violated.
checkMaximumValue
  :: (MonadError CliError m)
  => Api.BabbageEraOnwards era
  -> Api.LedgerProtocolParameters era
  -- ^ The `maxValue` protocol parameter.
  -> Either (ContractInstance lang era) [Transaction ()]
  -- ^ The bundle of contract information, or the transactions to traverse.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on `maxValue` validity.
checkMaximumValue era protocol info verbose = Api.babbageEraOnwardsConstraints era $ do
  let maxValue = protocol ^. lppPParamsL . ppMaxValSizeL
      (size, worst) =
        case info of
          Right transactions ->
            let measure tx@(Transaction _ contract _ _ ()) = [(worstValueSize $ extractAll contract, Just tx)]
             in maximumBy (compare `on` fst) $ foldMap measure transactions
          Left ContractInstance{..} -> (worstValueSize $ extractAllWithContinuations ciContract ciContinuations, Nothing)
  pure $
    putJson "Maximum value" $
      [ "Actual" .= size
      , "Maximum" .= maxValue
      , "Unit" .= ("byte" :: String)
      , "Invalid" .= (fromIntegral size > maxValue)
      , "Percentage" .= (100 * fromIntegral size / fromIntegral maxValue :: Double)
      ]
        <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> worst)

-- | Check that the protocol limit on minimum UTxO is not violated.
checkMinimumUtxo
  :: forall lang era m
   . (Api.IsShelleyBasedEra era)
  => (MonadError CliError m)
  => Api.BabbageEraOnwards era
  -- ^ The era.
  -> Api.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> Either (ContractInstance lang era) [Transaction ()]
  -- ^ The bundle of contract information, or the transactions to traverse.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on the `minimumUTxO` validity.
checkMinimumUtxo era protocol info verbose =
  do
    let compute tokens =
          do
            value <- liftCli (toCardanoValue $ mconcat [PV2.singleton cs tn 1 | Token cs tn <- toList tokens])
            scriptHash <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "88888888888888888888888888888888888888888888888888888888")
            stakeKeyHash <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "99999999999999999999999999999999999999999999999999999999")
            datumHash <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "5555555555555555555555555555555555555555555555555555555555555555")
            let address =
                  Api.makeShelleyAddressInEra
                    (Api.babbageEraOnwardsToShelleyBasedEra era)
                    Api.Mainnet
                    (Api.PaymentCredentialByScript scriptHash)
                    (Api.StakeAddressByValue $ Api.StakeCredentialByKey stakeKeyHash)
                out =
                  Api.TxOut
                    address
                    (mkTxOutValue era value)
                    ( Api.TxOutDatumHash
                        (babbageEraOnwardsToAlonzoEraOnwards era)
                        datumHash
                    )
                    Api.ReferenceScriptNone
            pure $ Api.calculateMinimumUTxO Api.shelleyBasedEra (Api.unLedgerProtocolParameters protocol) out
    (value, worst) <-
      case info of
        Right transactions ->
          let measure tx@(Transaction _ contract _ _ ()) = pure . (,Just tx) <$> compute (extractAll contract)
           in ( maximumBy (compare `on` fst)
                  :: [(Ledger.Coin, Maybe (Transaction ()))] -> (Ledger.Coin, Maybe (Transaction ()))
              )
                <$> foldTransactionsM measure transactions
        Left ContractInstance{..} -> fmap (,Nothing) . compute $ extractAllWithContinuations ciContract ciContinuations
    pure $
      putJson "Minimum UTxO" $
        [ "Requirement" .= value
        ]
          <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> worst)

-- | Check that transactions satisfy the execution-cost protocol limits.
checkExecutionCost
  :: (MonadError CliError m)
  => Api.BabbageEraOnwards era
  -> Api.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-paths through the contract.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on validity of transaction execution costs.
checkExecutionCost era protocol ContractInstance{..} transactions verbose = Api.babbageEraOnwardsConstraints era $
  do
    costModel <-
      liftCliMaybe "Plutus cost model not found." $
        P.PlutusV2 `M.lookup` P.costModelsValid (protocol ^. lppPParamsL . ppCostModelsL)
    (evaluationContext, _) <- liftCli $ runWriterT $ PV2.mkEvaluationContext $ P.getCostModelParams costModel
    let creatorAddress =
          PV2.Address
            (PV2.PubKeyCredential "88888888888888888888888888888888888888888888888888888888")
            (Just . PV2.StakingHash $ PV2.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
        referenceInput = do
          let referenceAddress =
                PV2.Address
                  (PV2.PubKeyCredential "77777777777777777777777777777777777777777777777777777777")
                  (Just . PV2.StakingHash $ PV2.PubKeyCredential "99999999999999999999999999999999999999999999999999999999")
              semanticsHash = PV2.ScriptHash . PV2.toBuiltin $ Api.serialiseToRawBytes $ viHash ciSemanticsValidator
          case viTxIn ciSemanticsValidator of
            Nothing -> mempty
            Just _ ->
              pure $
                PV2.TxInInfo
                  (PV2.TxOutRef "6666666666666666666666666666666666666666666666666666666666666666" 1)
                  (PV2.TxOut referenceAddress (P.lovelaceValueOf 1) PV2.NoOutputDatum (Just semanticsHash))
    semanticsAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciSemanticsValidator
    payoutAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciPayoutValidator
    let protocolVersion = ledgerProtVerToPlutusMajorProtocolVersion $ protocol ^. lppPParamsL . ppProtocolVersionL
    script <- liftCli $ deserialiseScript protocolVersion $ viBytes ciSemanticsValidator
    let executor =
          executeTransaction
            evaluationContext
            script
            semanticsAddress
            payoutAddress
            referenceInput
            creatorAddress
            (MarloweParams ciRolesCurrency)
    (steps, memories) <-
      unzip
        . fmap (\(tx, PV2.ExBudget{..}) -> ((tx, exBudgetCPU), (tx, exBudgetMemory)))
        <$> mapM (liftM2 (<$>) (,) executor) transactions
    let (worstSteps, PV2.ExCPU (unSatInt -> actualSteps)) = maximumBy (compare `on` snd) steps
        (worstMemory, PV2.ExMemory (unSatInt -> actualMemory)) = maximumBy (compare `on` snd) memories
        ExUnits maximumMemory maximumSteps = protocol ^. lppPParamsL . Ledger.ppMaxTxExUnitsL
    pure $
      putJson
        "Execution cost"
        [ "Steps"
            .= object
              ( [ "Actual" .= actualSteps
                , "Maximum" .= maximumSteps
                , "Percentage" .= (100 * fromIntegral actualSteps / fromIntegral maximumSteps :: Double)
                , "Invalid" .= (fromEnum actualSteps > (fromIntegral . naturalToInteger $ maximumSteps))
                ]
                  <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> Just worstSteps)
              )
        , "Memory"
            .= object
              ( [ "Actual" .= actualMemory
                , "Maximum" .= maximumMemory
                , "Percentage" .= (100 * fromIntegral actualMemory / fromIntegral maximumMemory :: Double)
                , "Invalid" .= (fromEnum actualMemory > (fromIntegral . naturalToInteger $ maximumMemory))
                ]
                  <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> Just worstMemory)
              )
        ]

calcMarloweTxExBudgets
  :: (MonadError CliError m)
  => Api.BabbageEraOnwards era
  -> Api.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-path through the contract. It should be a list of *consecutive* transactions.
  -> [PV2.TokenName]
  -> m [MarloweExBudget]
  -- ^ Action to print a report on validity of transaction execution costs.
calcMarloweTxExBudgets era protocol ContractInstance{..} transactionsPath lockedRoles = Api.babbageEraOnwardsConstraints era $ do
  costModel <-
    liftCliMaybe "Plutus cost model not found." $
      P.PlutusV2 `M.lookup` P.costModelsValid (protocol ^. lppPParamsL . ppCostModelsL)
  (evaluationContext, _) <- liftCli $ runWriterT $ PV2.mkEvaluationContext $ P.getCostModelParams costModel
  let useSemanticsReferenceInput = UseReferenceInput $ isJust $ viTxIn ciSemanticsValidator
      useOperatorReferenceInput = UseReferenceInput $ isJust $ viTxIn ciOpenRoleValidator

  semanticsAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciSemanticsValidator
  payoutAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciPayoutValidator
  openRoleAddress <- fmap snd . liftCli $ marloweAddressFromCardanoAddress $ viAddress ciOpenRoleValidator
  let protocolVersion = ledgerProtVerToPlutusMajorProtocolVersion $ protocol ^. lppPParamsL . ppProtocolVersionL
  semanticsScript <- liftCli $ deserialiseScript protocolVersion $ viBytes ciSemanticsValidator
  openRolesScript <- liftCli $ deserialiseScript protocolVersion $ viBytes ciOpenRoleValidator
  let calcTxBudget transaction stillLockedRoles =
        calcMarloweTxExBudget
          evaluationContext
          (semanticsScript, semanticsAddress, useSemanticsReferenceInput)
          (openRolesScript, openRoleAddress, useOperatorReferenceInput, LockedRoles stillLockedRoles)
          payoutAddress
          (MarloweParams ciRolesCurrency)
          transaction

      step (budgets, stillLockedRoles) transaction = do
        let Transaction _ _ TransactionInput{txInputs = txInputs} _ () = transaction
            requiredRoles = inputsRequiredRoles txInputs
            stillLockedRoles' = filter (`notElem` requiredRoles) stillLockedRoles
        txBudgets <- calcTxBudget transaction stillLockedRoles
        -- result@(MarloweExBudget _ (_, stillLockedRoles')) <- evaluator transaction stillLockeRoles
        pure (txBudgets : budgets, stillLockedRoles')
  reverse . fst <$> foldlM step ([], lockedRoles) transactionsPath

-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSizes
  :: forall lang era m
   . (C.IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.BabbageEraOnwards era
  -- ^ The era.
  -> Api.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> [Transaction ()]
  -- ^ The transaction-paths through the contract.
  -> Bool
  -- ^ Whether to include worst-case example in output.
  -> m A.Value
  -- ^ Action to print a report on validity of transaction size.
checkTransactionSizes era protocol ci transactions verbose =
  do
    sizes <- mapM (liftM2 (<$>) (,) $ checkTransactionSize era protocol ci) transactions
    let (worst, actual) = maximumBy (compare `on` snd) sizes
        (limit :: Word32) = Api.babbageEraOnwardsConstraints era $ protocol ^. lppPParamsL . ppMaxTxSizeL
    pure $
      putJson "Transaction size" $
        [ "Actual" .= actual
        , "Maximum" .= limit
        , "Percentage" .= (100 * fromIntegral actual / fromIntegral limit :: Double)
        , "Invalid" .= (actual > fromEnum limit)
        ]
          <> maybe [] (pure . ("Worst case" .=)) (guard verbose >> pure worst)

-- | Check that transactions satisfy the transaction-size protocol limit.
checkTransactionSize
  :: forall lang era m
   . (C.IsPlutusScriptLanguage lang)
  => (MonadError CliError m)
  => (MonadIO m)
  => Api.BabbageEraOnwards era
  -- ^ The era.
  -> Api.LedgerProtocolParameters era
  -- ^ The protocol parameters.
  -> ContractInstance lang era
  -- ^ The bundle of contract information.
  -> Transaction ()
  -- ^ The transaction-paths through the contract.
  -> m Int
  -- ^ Action to measure the transaction size.
checkTransactionSize era protocol ContractInstance{..} (Transaction marloweState marloweContract TransactionInput{..} output ()) =
  do
    -- We only attend to details that make affect the *size* of the transaction, so
    -- the transaction itself does not actually need to be valid.
    scriptInEra <- liftCliMaybe "Script language not supported in era" $ toScriptLanguageInEra era
    (txOutState, txOutContract, txOutPayments) <-
      case output of
        TransactionOutput{..} -> pure (txOutState, txOutContract, txOutPayments)
        Error e -> throwError . CliError $ show e
    creatorAddress <- do
      scriptHash <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "88888888888888888888888888888888888888888888888888888888")
      stakeKeyHash <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "99999999999999999999999999999999999999999999999999999999")
      pure $
        Api.makeShelleyAddressInEra
          (Api.babbageEraOnwardsToShelleyBasedEra era)
          Api.Mainnet
          (Api.PaymentCredentialByScript scriptHash)
          (Api.StakeAddressByValue $ Api.StakeCredentialByKey stakeKeyHash)
    let
      -- NOTE:
      -- All the below variable assignments were written using a monadic `bind` like:
      -- `onLovelace <- pure $ Api.lovelaceToValue 1`
      -- Could it be that this `pure` was necessary to enforce a particular evaluation
      -- order? I can't see that was the case or that actually mattered.
      oneLovelace = Api.lovelaceToValue 1
      marloweParams = MarloweParams ciRolesCurrency
      inDatum =  MarloweData{..}
      redeemer = marloweTxInputsFromInputs txInputs
    txIdForScript <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "0000000000000000000000000000000000000000000000000000000000000000")
    let inScript =
          ( Api.TxIn txIdForScript $ Api.TxIx 0
          , Api.BuildTxWith
              . Api.ScriptWitness Api.ScriptWitnessForSpending
              $ Api.PlutusScriptWitness
                scriptInEra
                C.plutusScriptVersion
                (validatorInfoScriptOrReference ciSemanticsValidator)
                (Api.ScriptDatumForTxIn . Just . unsafeHashableScriptData . Api.fromPlutusData $ PV2.toData inDatum)
                (unsafeHashableScriptData . Api.fromPlutusData $ PV2.toData redeemer)
                (Api.ExecutionUnits 0 0)
          )
    outValue <- liftCli . toCardanoValue $ totalBalance $ accounts txOutState
    let outScript =
          if txOutContract == Close
            then mempty
            else
              let outDatum = MarloweData marloweParams txOutState txOutContract
               in pure $
                    Api.TxOut
                      (viAddress ciSemanticsValidator)
                      (mkTxOutValue era outValue)
                      ( toTxOutDatumInTx era $
                          PV2.Datum $ toBuiltinData outDatum
                      )
                      Api.ReferenceScriptNone
        makePayment (Payment _ (Party (Address network address)) (Token currency name) amount) =
          do
            value <- liftCli . toCardanoValue $ PV2.singleton currency name amount
            address' <-
              liftCli $
                toCardanoAddressInEra
                  (if network == mainnet then Api.Mainnet else Api.Testnet $ Api.NetworkMagic 2)
                  address
            address'' <-
              case address' of
                Api.AddressInEra (Api.ShelleyAddressInEra _) address'' -> pure address''
                _ -> throwError $ CliError "Byron addresses are not supported."
            pure
              [ Api.TxOut
                  (Api.AddressInEra (Api.ShelleyAddressInEra $ Api.babbageEraOnwardsToShelleyBasedEra era) address'')
                  (mkTxOutValue era value)
                  Api.TxOutDatumNone
                  Api.ReferenceScriptNone
              ]
        makePayment (Payment _ (Party (Role role)) (Token currency name) amount) =
          do
            value <- liftCli $ toCardanoValue $ PV2.singleton currency name amount
            pure
              [ Api.TxOut
                  (viAddress ciPayoutValidator)
                  (mkTxOutValue era value)
                  ( toTxOutDatumInTx era $
                      PV2.Datum $ toBuiltinData (ciRolesCurrency, role)
                  )
                  Api.ReferenceScriptNone
              ]
        makePayment _ = pure []
    outPayments <- concat <$> mapM makePayment txOutPayments
    let findRole :: InputContent -> [PV2.TokenName]
        findRole (IDeposit _ (Role role) _ _) = pure role
        findRole (IChoice (ChoiceId _ (Role role)) _) = pure role
        findRole _ = mempty
    roles <-
      liftCli
        . mapM toCardanoValue
        $ fmap (flip (PV2.singleton ciRolesCurrency) 1) (foldMap (findRole . getInputContent) txInputs)
    txIdForRoles <- liftCli $ deserialiseFromRawBytesHex (BS8.pack "1111111111111111111111111111111111111111111111111111111111111111")
    let inRoles =
          (,Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending)
            . Api.TxIn txIdForRoles
            . Api.TxIx
            <$> [1 .. (toEnum $ length roles)]
        outRoles =
          [ Api.TxOut
            creatorAddress
            (mkTxOutValue era role)
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
          | role <- roles
          ]
        outMerkle =
          concat
            [ case input of
              MerkleizedInput _ _ contract ->
                pure $
                  Api.TxOut
                    creatorAddress
                    (mkTxOutValue era oneLovelace)
                    ( toTxOutDatumInTx era $
                        PV2.Datum $ toBuiltinData contract
                    )
                    Api.ReferenceScriptNone
              _ -> mempty
            | input <- txInputs
            ]
        inFunds =
          ( Api.TxIn txIdForRoles $ Api.TxIx 0
          , Api.BuildTxWith $ Api.KeyWitness Api.KeyWitnessForSpending
          )
        outChange =
          Api.TxOut
            creatorAddress
            (mkTxOutValue era oneLovelace)
            Api.TxOutDatumNone
            Api.ReferenceScriptNone
        txIns =
          inFunds
            : inScript
            : inRoles
        txInsCollateral = Api.TxInsCollateral (babbageEraOnwardsToAlonzoEraOnwards era) []
        txInsReference = Api.TxInsReferenceNone
        txOuts =
          outChange
            : outScript
              <> outPayments
              <> outRoles
              <> outMerkle
        txReturnCollateral = Api.TxReturnCollateralNone
        txTotalCollateral = Api.TxTotalCollateralNone
        txFee = Api.TxFeeExplicit (C.babbageEraOnwardsToShelleyBasedEra era) 1
        convertSlot = Api.SlotNo . fromIntegral . P.posixTimeToEnclosingSlot ciSlotConfig
        (txValidityLowerBound, txValidityUpperBound) =
          bimap
            (Api.TxValidityLowerBound (babbageEraOnwardsToAllegraEraOnwards era) . convertSlot)
            (Api.TxValidityUpperBound (C.babbageEraOnwardsToShelleyBasedEra era) . Just . convertSlot)
            txInterval
        txMetadata = Api.TxMetadataNone
        txAuxScripts = Api.TxAuxScriptsNone
        txExtraKeyWits =
          Api.TxExtraKeyWitnesses
            (babbageEraOnwardsToAlonzoEraOnwards era)
            $ concat
              [ case address of
                Api.AddressInEra _ (Api.ShelleyAddress _ (Shelley.KeyHashObj pkh) _) -> pure $ Api.PaymentKeyHash pkh
                _ -> mempty
              | Api.TxOut address _ _ _ <- outPayments
              ]
        txProtocolParams = Api.BuildTxWith $ Just protocol
        txWithdrawals = Api.TxWithdrawalsNone
        txCertificates = Api.TxCertificatesNone
        txUpdateProposal = Api.TxUpdateProposalNone
        txMintValue = Api.TxMintNone
        txScriptValidity = Api.TxScriptValidityNone
        txProposalProcedures = Nothing
        txVotingProcedures = Nothing
        txCurrentTreasuryValue = Nothing
        txTreasuryDonation = Nothing
    body <- liftCli $ Api.createAndValidateTransactionBody (C.babbageEraOnwardsToShelleyBasedEra era) Api.TxBodyContent{..}
    keys <-
      liftIO $
        sequence
          [ Api.WitnessPaymentKey <$> Api.generateSigningKey Api.AsPaymentKey
          | let wits = case txExtraKeyWits of
                  Api.TxExtraKeyWitnesses _ wits' -> wits'
                  _ -> mempty
          , _ <- wits
          ]
    let tx = Api.signShelleyTransaction (C.babbageEraOnwardsToShelleyBasedEra era) body keys
    pure . BS.length $ Api.shelleyBasedEraConstraints (Api.babbageEraOnwardsToShelleyBasedEra era) $ Api.serialiseToCBOR tx

-- | Format results of checking as JSON.
putJson
  :: String
  -- ^ The section name.
  -> [A.Pair]
  -- ^ The results.
  -> A.Value
  -- ^ Action to print the results as YAML.
putJson section = object . pure . (fromString section .=) . object
