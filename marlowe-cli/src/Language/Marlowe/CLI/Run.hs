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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-overlapping-patterns -Wno-deprecations #-}

-- | Run Marlowe contracts.
module Language.Marlowe.CLI.Run (
  -- * Computation
  adjustMinimumUTxO,
  autoRunTransaction,
  autoRunTransactionImpl,
  initializeTransaction,
  initializeTransactionImpl,
  initializeTransactionUsingScriptRefsImpl,
  makeMarlowe,
  prepareTransaction,
  prepareTransactionImpl,
  runTransaction,
  runTransactionImpl,

  -- * Roles
  autoWithdrawFunds,
  autoWithdrawFundsImpl,
  withdrawFunds,

  -- * Input
  makeChoice,
  makeDeposit,
  makeNotification,

  -- * Party Addresses
  marloweAddressFromCardanoAddress,
  marloweAddressToCardanoAddress,

  -- * Utilities
  toCardanoAddressInEra,
  toCardanoAssetName,
  toCardanoValue,
  toCardanoPolicyId,
  toPlutusAddress,
) where

import Cardano.Api (
  AddressInEra (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  PlutusScriptVersion (..),
  QueryInShelleyBasedEra (..),
  QueryUTxOFilter (..),
  SerialiseAsRawBytes (..),
  SlotNo (..),
  StakeAddressReference (..),
  TxBody,
  TxId,
  TxIn,
  TxMetadataInEra,
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  UTxO (..),
  getTxId,
  lovelaceToValue,
  txOutValueToValue,
  writeFileTextEnvelope,
 )
import Cardano.Api qualified as Api (Value)
import Cardano.Api qualified as C
import Cardano.Api (ReferenceScript (ReferenceScriptNone))
import Cardano.Api qualified as CS
import Cardano.Chain.Common (addrToBase58)
import Cardano.Ledger.BaseTypes qualified as LC (Network (..))
import Control.Monad (forM_, guard, unless, void, when)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy)
import Data.Map.Strict qualified as M (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as S (fromList, singleton)
import Data.String (IsString (..))
import Data.Time.Units (Second)
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Data.Type.Equality (type (:~:) (..))
import Language.Marlowe.CLI.Cardano.Api (adjustMinimumUTxO, toTxOutDatumInTx)
import Language.Marlowe.CLI.Cardano.Api.Address (toShelleyStakeReference)
import Language.Marlowe.CLI.Export (
  buildMarloweDatum,
  buildRedeemer,
  buildRoleDatum,
  buildRoleRedeemer,
  marloweValidatorInfo,
  openRoleValidatorInfo,
  payoutValidatorInfo,
 )
import Language.Marlowe.CLI.IO (
  decodeFileStrict,
  getLedgerProtocolParams,
  liftCli,
  liftCliIO,
  maybeWriteJson,
  queryByAddress,
  queryInEra,
  queryUTxOs,
  readMaybeMetadata,
  readSigningKey,
 )
import Language.Marlowe.CLI.Merkle (merkleizeMarlowe)
import Language.Marlowe.CLI.Orphans ()
import Language.Marlowe.CLI.Transaction (
  buildBody,
  buildPayFromScript,
  buildPayToScript,
  ensureMinUtxo,
  findMarloweScriptsRefs,
  hashSigningKey,
  makeTxOut',
  selectCoins,
  submitBody,
 )
import Language.Marlowe.CLI.Types (
  AUTxO (AUTxO, unAUTxO),
  CliEnv,
  CliError (..),
  DatumInfo (..),
  MarlowePlutusVersion,
  MarloweScriptsRefs (MarloweScriptsRefs, mrMarloweValidator, mrRolePayoutValidator),
  MarloweTransaction (..),
  PayFromScript (..),
  PrintStats (PrintStats),
  PublishingStrategy,
  QueryExecutionContext (..),
  RedeemerInfo (..),
  SigningKeyFile (..),
  SomeMarloweTransaction (..),
  SomePaymentSigningKey,
  TxFile (TxFile),
  TxBuildupContext (..),
  ValidatorInfo (..),
  askEra,
  defaultCoinSelectionStrategy,
  doWithShelleyBasedEra,
  mkNodeTxBuildup,
  mrOpenRoleValidator,
  toAddressAny',
  toQueryContext,
  toShelleyAddress,
  txIn,
  validatorInfoScriptOrReference, MarloweScriptsInfo (..)
 )
import Marlowe.Plutus.Merkle (MerkleizedContract (..), merkleizeInputs)
import Marlowe.Plutus.Semantics (
  MarloweParams (..),
  Payment (..),
  TransactionInput (..),
  TransactionOutput (..),
  TransactionWarning,
  computeTransaction,
  paymentMoney,
 )
import Marlowe.Plutus.Semantics.Types (
  AccountId,
  ChoiceId (..),
  ChoiceName,
  ChosenNum,
  Contract,
  Input (..),
  InputContent (..),
  Party (..),
  Payee (..),
  State (accounts),
  Token (..),
  TokenName (..),
  getInputContent,
 )
import Marlowe.Plutus.Semantics.Types.Address (Network, mainnet, testnet)
import Plutus.V1.Ledger.Ada (fromValue)
import Plutus.V1.Ledger.SlotConfig (SlotConfig, posixTimeToEnclosingSlot, slotToBeginPOSIXTime, slotToEndPOSIXTime)
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.V1 (Credential (..), DatumHash (..), fromBuiltin)
import PlutusLedgerApi.V1 qualified as P
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  Value (..),
  assetClass,
  assetClassValue,
  flattenValue,
  singleton,
 )
import PlutusLedgerApi.V2 (
  Address,
  Datum (..),
  POSIXTime,
  adaSymbol,
  adaToken,
  toBuiltinData,
 )
import PlutusTx.AssocMap qualified as AM (toList)
import Prettyprinter (Pretty (..))
import System.IO (hPutStrLn, stderr)

-- | Serialise a deposit input to a file.
makeDeposit
  :: (MonadIO m)
  => AccountId
  -- ^ The account for the deposit.
  -> Party
  -- ^ The party making the deposit.
  -> Maybe Token
  -- ^ The token being deposited.
  -> Integer
  -- ^ The amount of the token deposited.
  -> Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeDeposit accountId party token amount outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IDeposit accountId party (fromMaybe (Token adaSymbol adaToken) token) amount

-- | Serialise a choice input to a file.
makeChoice
  :: (MonadIO m)
  => ChoiceName
  -- ^ The name of the choice made.
  -> Party
  -- ^ The party making the choice.
  -> ChosenNum
  -- ^ The number chosen.
  -> Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeChoice name party chosen outputFile =
  maybeWriteJson outputFile
    . NormalInput
    $ IChoice (ChoiceId name party) chosen

-- | Serialise a notification input to a file.
makeNotification
  :: (MonadIO m)
  => Maybe FilePath
  -- ^ The output JSON file representing the input.
  -> m ()
  -- ^ Action to write the input to the file.
makeNotification outputFile =
  maybeWriteJson outputFile $
    NormalInput INotify

-- | Create an initial Marlowe transaction.
initializeTransaction
  :: forall lang era m
   . MonadError CliError m
  => MonadIO m
  => (MonadReader (CliEnv era) m)
  => (C.IsPlutusScriptLanguage lang)
  => MarloweScriptsInfo lang era
  -> MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -- ^ The POSIXTime-to-slot configuration.
  -> StakeAddressReference
  -- ^ The stake address.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
initializeTransaction marloweScripts marloweParams slotConfig stake contractFile stateFile outputFile merkleize printStats =
  do
    era <- askEra
    contract <- decodeFileStrict contractFile
    state <- decodeFileStrict stateFile
    marloweTransaction <-
      initializeTransactionImpl
        marloweScripts
        marloweParams
        slotConfig
        stake
        contract
        state
        merkleize
        printStats
    maybeWriteJson outputFile $
      SomeMarloweTransaction
        (C.plutusScriptVersion @lang)
        era
        marloweTransaction

-- | Create an initial Marlowe transaction.
initializeTransactionImpl
  :: forall lang m era
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => (C.IsPlutusScriptLanguage lang)
  => MarloweScriptsInfo lang era
  -> MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The initial Marlowe contract.
  -> State
  -- ^ The initial Marlowe state.
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m (MarloweTransaction lang era)
  -- ^ Action to return a MarloweTransaction
initializeTransactionImpl marloweScripts marloweParams mtSlotConfig stake mtContract mtState merkleize printStats = case C.plutusScriptVersion @lang of
  PlutusScriptV1 -> throwError "Plutus Script V1 not supported"
  PlutusScriptV3 -> do
    era <- askEra
    let mtRolesCurrency = rolesCurrency marloweParams
    (mtValidator, mtRoleValidator, mtOpenRoleValidator) <- do
      let
        MarloweScriptsInfo {..} = marloweScripts
      vi' <-
        case toShelleyAddress $ viAddress msMarloweValidator of
          Nothing -> throwError "Expecting shelley address in reference validator info"
          Just (CS.ShelleyAddress n p _) ->
            pure $
              msMarloweValidator
                { viAddress =
                    C.shelleyAddressInEra (C.babbageEraOnwardsToShelleyBasedEra era) $
                      CS.ShelleyAddress n p $
                        toShelleyStakeReference stake
                }
      pure (vi', msRolePayoutValidator, msOpenRoleValidator)
    let ValidatorInfo{..} = mtValidator
        mtContinuations = mempty
        mtRange = Nothing
        mtInputs = []
        mtPayments = []
    liftIO $
      when printStats $
        do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Validator size: " <> show viSize
          hPutStrLn stderr $ "Base-validator cost: " <> show viCost
    let marloweTransaction = MarloweTransaction{..}
    pure $ if merkleize then merkleizeMarlowe marloweTransaction else marloweTransaction

-- | Create an initial Marlowe transaction using reference scripts.
initializeTransactionUsingScriptRefsImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadIO m)
  => (C.IsShelleyBasedEra era)
  => MarloweParams
  -- ^ The Marlowe contract parameters.
  -> SlotConfig
  -- ^ The POSIXTime-to-slot configuration.
  -> MarloweScriptsRefs lang era
  -- ^ The reference scripts.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The initial Marlowe contract.
  -> State
  -- ^ The initial Marlowe state.
  -> Bool
  -- ^ Whether to deeply merkleize the contract.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m (MarloweTransaction lang era)
  -- ^ Action to return a MarloweTransaction
initializeTransactionUsingScriptRefsImpl marloweParams mtSlotConfig scriptRefs stake mtContract mtState merkleize printStats =
  do
    let mtRolesCurrency = rolesCurrency marloweParams
        setupStaking vi@ValidatorInfo{viAddress} = do
          case toShelleyAddress viAddress of
            Nothing -> throwError "Expecting shelley address in reference validator info"
            Just (CS.ShelleyAddress n p _) -> do
              let viAddress' =
                    C.shelleyAddressInEra (C.shelleyBasedEra @era) $
                      CS.ShelleyAddress
                        n
                        p
                        (toShelleyStakeReference stake)
              pure $ vi{viAddress = viAddress'}

        MarloweScriptsRefs{mrMarloweValidator = (_, mv), mrRolePayoutValidator = (_, pv), mrOpenRoleValidator = (_, ov)} = scriptRefs

    mtValidator <- setupStaking mv
    mtRoleValidator <- setupStaking pv
    mtOpenRoleValidator <- setupStaking ov

    let ValidatorInfo{..} = mtValidator
        mtContinuations = mempty
        mtRange = Nothing
        mtInputs = []
        mtPayments = []
    liftIO $
      when printStats $
        do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "Validator size: " <> show viSize
          hPutStrLn stderr $ "Base-validator cost: " <> show viCost
    let marloweTransaction = MarloweTransaction{..}
    pure $ if merkleize then merkleizeMarlowe marloweTransaction else marloweTransaction

-- | Prepare the next step in a Marlowe contract.
prepareTransaction
  :: (MonadError CliError m)
  => (MonadIO m)
  => FilePath
  -- ^ The JSON file with the Marlowe initial state and initial contract.
  -> [Input]
  -- ^ The contract's inputs.
  -> POSIXTime
  -- ^ The first valid time for the transaction.
  -> POSIXTime
  -- ^ The last valid time for the transaction.
  -> Maybe FilePath
  -- ^ The output JSON file with the results of the computation.
  -> Bool
  -- ^ Whether to print statistics about the result.
  -> m ()
  -- ^ Action to compute the next step in the contract.
prepareTransaction marloweFile txInputs minimumTime maximumTime outputFile printStats =
  do
    SomeMarloweTransaction lang era marloweIn <- decodeFileStrict marloweFile
    marloweOut <- prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats
    maybeWriteJson outputFile $ SomeMarloweTransaction lang era marloweOut

-- | Implementation of Prepare function
prepareTransactionImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweTransaction lang era
  -- ^ Marlowe transaction to be prepared.
  -> [Input]
  -- ^ The contract's inputs.
  -> POSIXTime
  -- ^ The first valid time for the transaction.
  -> POSIXTime
  -- ^ The last valid time for the transaction.
  -> Bool
  -- ^ Whether to print statistics about the result.
  -> m (MarloweTransaction lang era)
  -- ^ Action to compute the next step in the contract.
prepareTransactionImpl marloweIn txInputs minimumTime maximumTime printStats =
  do
    let txInterval = (minimumTime, maximumTime)
        mp = MarloweParams { rolesCurrency = mtRolesCurrency marloweIn }
    (warnings, marloweOut@MarloweTransaction{..}) <- makeMarlowe marloweIn (TransactionInput txInterval txInputs)
    liftIO $
      do
        when printStats $
          do
            hPutStrLn stderr ""
            unless (null warnings) $
              do
                hPutStrLn stderr "Warnings:"
                forM_ warnings $
                  hPutStrLn stderr . ("  " <>) . show
            hPutStrLn stderr $ "Datum size: " <> show (diSize $ buildMarloweDatum mp mtContract mtState)
        sequence_
          [ do
            when printStats $
              do
                hPutStrLn stderr $ "Payment " <> show (i :: Int)
                hPutStrLn stderr $ "  Account: " <> show accountId
                hPutStrLn stderr $ "  Payee: " <> show payee
                hPutStrLn stderr $ "  Ada: " <> show (fromValue money)
            sequence_
              [ hPutStrLn stderr $ "  " <> show (pretty symbol) <> "." <> show (pretty token) <> ": " <> show amount
              | (symbol, tokenAmounts) <- AM.toList $ getValue money
              , symbol /= adaSymbol
              , (token, amount) <- AM.toList tokenAmounts
              ]
          | (i, payment@(Payment accountId payee _ _)) <- zip [1 ..] mtPayments
          , let money = paymentMoney payment
          ]
    pure marloweOut

-- | Prepare the next step in a Marlowe contract.
makeMarlowe
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweTransaction lang era
  -- ^ The Marlowe initial state and initial contract.
  -> TransactionInput
  -- ^ The transaction input.
  -> m ([TransactionWarning], MarloweTransaction lang era)
  -- ^ Action to compute the next step in the contract.
makeMarlowe marloweIn@MarloweTransaction{..} transactionInput =
  do
    let toSlot = posixTimeToEnclosingSlot mtSlotConfig
        toSlotNo = SlotNo . fromIntegral . toSlot

        roundTxInterval ti@TransactionInput{txInterval = txInterval@(minimumTime, maximumTime)} = do
          let txInterval' =
                ( slotToBeginPOSIXTime mtSlotConfig . toSlot $ minimumTime
                , slotToEndPOSIXTime mtSlotConfig . toSlot $ maximumTime
                )
          when (txInterval' /= txInterval) $
            liftIO $
              hPutStrLn stderr $
                "Rounding `TransactionInput` txInterval boundaries to:" <> show txInterval'
          pure ti{txInterval = txInterval'}

    transactionInput' <- roundTxInterval transactionInput
    liftIO $ print transactionInput'
    transactionInput''@TransactionInput{..} <-
      merkleizeInputs (MerkleizedContract mtContract mtContinuations) mtState transactionInput'
        `catchError` const (pure transactionInput') -- TODO: Consider not catching errors here.
    case computeTransaction transactionInput'' mtState mtContract of
      Error message -> throwError . CliError . show $ message
      TransactionOutput{..} ->
        pure
          ( txOutWarnings
          , marloweIn
              { mtState = txOutState
              , mtContract = txOutContract
              , mtRange = Just $ bimap toSlotNo toSlotNo txInterval
              , mtInputs = txInputs
              , mtPayments = txOutPayments
              }
          )

readMarloweTransactionFile
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadIO m)
  => C.BabbageEraOnwards era
  -> PlutusScriptVersion lang
  -> FilePath
  -> m (MarloweTransaction lang era)
readMarloweTransactionFile era lang marloweInFile = do
  SomeMarloweTransaction lang' era' marloweIn <- decodeFileStrict marloweInFile
  case era of
    C.BabbageEraOnwardsBabbage -> case (era', lang, lang') of
      (C.BabbageEraOnwardsBabbage, PlutusScriptV1, PlutusScriptV1) -> pure marloweIn
      (C.BabbageEraOnwardsBabbage, PlutusScriptV3, PlutusScriptV3) -> pure marloweIn
      (C.BabbageEraOnwardsConway, _, _) -> throwError "Running in Babbage era, read file in Conway era"
      _ ->
        throwError . CliError $
          "Expecting a different version of plutus in the Marlowe file. Expected: " <> show lang <> " but got: " <> show lang'
    C.BabbageEraOnwardsConway -> case (era', lang, lang') of
      (C.BabbageEraOnwardsConway, PlutusScriptV1, PlutusScriptV1) -> pure marloweIn
      (C.BabbageEraOnwardsConway, PlutusScriptV3, PlutusScriptV3) -> pure marloweIn
      (C.BabbageEraOnwardsBabbage, _, _) -> throwError "Running in Conway era, read file in Babbage era"
      _ ->
        throwError . CliError $
          "Expecting a different version of plutus in the Marlowe file. Expected: " <> show lang <> " but got: " <> show lang'

-- | Run a Marlowe transaction using FS.
runTransaction
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> Maybe (FilePath, TxIn, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
runTransaction connection marloweInBundle marloweOutFile inputs outputs changeAddress signingKeyFiles metadataFile (TxFile txFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let go :: forall lang. (C.IsPlutusScriptLanguage lang) => MarloweTransaction lang era -> m TxId
        go marloweOut'' = do
          marloweInBundle' <- case marloweInBundle of
            Nothing -> pure Nothing
            Just (marloweInFile, marloweTxIn, collateralTxIn) -> do
              marloweIn <- readMarloweTransactionFile era (C.plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
              pure $ Just (marloweIn, marloweTxIn, collateralTxIn)

          (body :: TxBody era) <-
            runTransactionImpl
              (mkNodeTxBuildup connection timeout)
              marloweInBundle'
              marloweOut''
              inputs
              outputs
              changeAddress
              signingKeys
              metadata
              printStats
              invalid
          doWithShelleyBasedEra
            $ liftCliIO
            $ writeFileTextEnvelope (File txFile) Nothing
            $ C.makeSignedTransaction [] body
          pure $ getTxId body

    case testSameEra era era' of
      Just Refl -> go marloweOut'
      Nothing -> throwError $ fromString $ "Running in " <> show era <> ", read file in " <> show era'

testSameEra :: C.BabbageEraOnwards era -> C.BabbageEraOnwards era' -> Maybe (era :~: era')
testSameEra = \case
  C.BabbageEraOnwardsBabbage -> \case
    C.BabbageEraOnwardsBabbage -> Just Refl
    _ -> Nothing
  C.BabbageEraOnwardsConway -> \case
    C.BabbageEraOnwardsConway -> Just Refl
    _ -> Nothing

-- | Run a Marlowe transaction.
runTransactionImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Maybe (MarloweTransaction lang era, TxIn, TxIn)
  -- ^ The Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> MarloweTransaction lang era
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -- ^ Required signing keys.
  -> TxMetadataInEra era
  -- ^ Tx metadata.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
runTransactionImpl txBuildupCtx marloweInBundle marloweOut' inputs outputs changeAddress signingKeys metadata printStats invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    liftIO $ hPutStrLn stderr $ "Running transcation with marlowe out:" <> show marloweOut'
    era <- askEra @era
    protocol <- getLedgerProtocolParams queryCtx
    let marloweParams = MarloweParams { rolesCurrency = mtRolesCurrency marloweOut' }
        go :: MarloweTransaction lang era -> m (TxBody era)
        go marloweOut = do
          (spend, collateral, datumOutputs) <-
            case marloweInBundle of
              Nothing -> pure ([], Nothing, [])
              Just (marloweIn, spend, collateral) -> do
                let validatorInfo = mtValidator marloweIn
                    validator = validatorInfoScriptOrReference validatorInfo

                    marloweParams' = MarloweParams { rolesCurrency = mtRolesCurrency marloweIn }
                    redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                    -- inputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
                    spend' = buildPayFromScript validator Nothing redeemer spend
                    -- SCP-3610: Remove when Babbage era features become available and the validator is revised.
                    merkles =
                      catMaybes
                        [ case input of
                          NormalInput _ -> Nothing
                          MerkleizedInput _ _ continuation ->
                            Just
                              ( -- Send the ancillary datum to the change address.
                                changeAddress
                              , -- Astonishing that this eUTxO can be spent without script or redeemer!
                                toTxOutDatumInTx era . Datum $ toBuiltinData continuation
                              , -- FIXME: Replace with protocol-dependent min-Ada.
                                lovelaceToValue 1_500_000
                              )
                        | input <- mtInputs marloweOut
                        ]
                when (marloweParams /= marloweParams') $
                  throwError "MarloweParams value is not preserved in continuation"
                pure ([spend'], Just collateral, merkles)
          let scriptAddress = viAddress $ mtValidator marloweOut
              outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
          outputValue <-
            mconcat
              <$> sequence
                [ liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
                | ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
                ]
          let continue =
                do
                  guard (outputValue /= mempty)
                  pure $
                    buildPayToScript era scriptAddress outputValue outputDatum
              roleAddress = viAddress $ mtRoleValidator marloweOut
          (payments :: [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]) <-
            catMaybes
              <$> sequence
                [ case payee of
                  Party (Address network address) -> do
                    address' <- doWithShelleyBasedEra $ marloweAddressToCardanoAddress network address
                    money' <-
                      liftCli $
                        toCardanoValue money
                    let (_, money'') = adjustMinimumUTxO era protocol address' C.TxOutDatumNone money' ReferenceScriptNone
                    pure $ Just (address', C.TxOutDatumNone, money'')
                  Party (Role role) -> do
                    money' <-
                      liftCli $
                        toCardanoValue money
                    let datum = toTxOutDatumInTx era $ diDatum $ buildRoleDatum (Token (rolesCurrency marloweParams) role)
                    let (_, money'') = adjustMinimumUTxO era protocol roleAddress datum money' ReferenceScriptNone
                    pure $ Just (roleAddress, datum, money'')
                  Account _ -> pure Nothing
                | (payee, money) <-
                    bimap head mconcat . unzip
                      <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                        [ (payee, money)
                        | payment@(Payment _ payee _ _) <- mtPayments marloweOut
                        , let money = paymentMoney payment
                        ]
                ]
          outputs' <- for (payments <> outputs <> datumOutputs) (uncurry3 makeTxOut')
          liftIO $ putStrLn "Selecting coins and building transaction body..."
          body <-
            doWithShelleyBasedEra $
              buildBody
                queryCtx
                spend
                continue
                []
                inputs
                outputs'
                collateral
                changeAddress
                (mtRange marloweOut)
                (hashSigningKey <$> signingKeys)
                TxMintNone
                metadata
                printStats
                invalid
          void $ submitBody txBuildupCtx body signingKeys invalid
          pure body
    go marloweOut'

-- | Withdraw funds for a specific role from the role address.
withdrawFunds
  :: (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe state and contract.
  -> TokenName
  -- ^ The role name for the redemption.
  -> TxIn
  -- ^ The collateral.
  -> [TxIn]
  -- ^ The transaction inputs.
  -> [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
  -- ^ The transaction outputs.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxFile
  -- ^ The output file for the transaction body.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
withdrawFunds connection marloweOutFile roleName collateral inputs outputs changeAddress signingKeyFiles metadataFile (TxFile txFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ _ marloweOut <- decodeFileStrict marloweOutFile
    let rolesCurrency = mtRolesCurrency marloweOut
        roleToken = Token rolesCurrency roleName
    signingKeys <- mapM readSigningKey signingKeyFiles
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ buildRoleDatum roleToken
    let validatorInfo = mtRoleValidator marloweOut
        roleScript = validatorInfoScriptOrReference validatorInfo
        roleAddress = viAddress validatorInfo
        roleDatum = diDatum $ buildRoleDatum roleToken
        roleRedeemer = riRedeemer buildRoleRedeemer
        checkRole (TxOut _ _ datum _) =
          case datum of
            TxOutDatumInline _ _ -> False
            TxOutDatumNone -> False
            TxOutDatumHash _ datumHash -> datumHash == roleHash
    utxos <-
      fmap (filter (checkRole . snd) . M.toList . unUTxO)
        . queryInEra connection
        . QueryUTxO
        . QueryUTxOByAddress
        . S.singleton
        . toAddressAny'
        $ roleAddress
    let spend = buildPayFromScript roleScript (Just roleDatum) roleRedeemer . fst <$> utxos
        withdrawal = (changeAddress, C.TxOutDatumNone, mconcat [txOutValueToValue value | (_, TxOut _ value _ _) <- utxos])
    outputs' <- mapM (uncurry3 makeTxOut') $ withdrawal : outputs
    body <-
      doWithShelleyBasedEra $
        buildBody
          (QueryNode connection)
          spend
          Nothing
          []
          inputs
          outputs'
          (Just collateral)
          changeAddress
          Nothing
          (hashSigningKey <$> signingKeys)
          TxMintNone
          metadata
          printStats
          invalid
    doWithShelleyBasedEra
      $ liftCliIO
      $ writeFileTextEnvelope (File txFile) Nothing
      $ C.makeSignedTransaction [] body
    let txBuildupCtx = mkNodeTxBuildup connection timeout
    submitBody txBuildupCtx body signingKeys invalid

-- | Run a Marlowe transaction using FS, without selecting inputs or outputs.
autoRunTransaction
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> Maybe (FilePath, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spent and the collateral, unless the transaction opens the contract.
  -> FilePath
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxFile
  -- ^ The output file for the unsigned transaction.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
autoRunTransaction connection marloweInBundle marloweOutFile changeAddress signingKeyFiles metadataFile (TxFile txFile) timeout printStats invalid =
  do
    metadata <- readMaybeMetadata metadataFile
    SomeMarloweTransaction _ era' marloweOut' <- decodeFileStrict marloweOutFile
    era <- askEra @era
    signingKeys <- mapM readSigningKey signingKeyFiles
    let go :: forall lang. (C.IsPlutusScriptLanguage lang) => MarloweTransaction lang era -> m TxId
        go marloweOut'' = do
          marloweInBundle' <- case marloweInBundle of
            Nothing -> pure Nothing
            Just (marloweInFile, marloweTxIn) -> do
              marloweIn <- readMarloweTransactionFile era (C.plutusScriptVersion :: PlutusScriptVersion lang) marloweInFile
              pure $ Just (marloweIn, marloweTxIn)

          (body :: TxBody era) <-
            autoRunTransactionImpl
              (mkNodeTxBuildup connection timeout)
              marloweInBundle'
              marloweOut''
              []
              changeAddress
              signingKeys
              metadata
              printStats
              invalid
          -- Write the transaction file.
          doWithShelleyBasedEra
            $ liftCliIO
            $ writeFileTextEnvelope (File txFile) Nothing
            $ C.makeSignedTransaction [] body
          pure $ getTxId body

    case testSameEra era era' of
      Just Refl -> go marloweOut'
      Nothing -> throwError $ fromString $ "Running in " <> show era <> ", read file in " <> show era'

-- | Run a Marlowe transaction, without selecting inputs or outputs.
autoRunTransactionImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Maybe (MarloweTransaction lang era, TxIn)
  -- ^ The JSON file with the Marlowe initial state and initial contract, unless the transaction opens the contract.
  -> MarloweTransaction lang era
  -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
  -> [PayFromScript lang]
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -> TxMetadataInEra era
  -- ^ The file containing JSON metadata, if any.
  -> Bool
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
autoRunTransactionImpl txBuildupCtx marloweInBundle marloweOut' extraSpend changeAddress signingKeys metadata printStats invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    era <- askEra @era
    protocol <- getLedgerProtocolParams queryCtx
    -- Read the Marlowe transaction information for the output.
    -- Fetch the era.
    let go :: MarloweTransaction lang era -> m (TxBody era)
        go marloweOut = do
          let rolesCurrency = mtRolesCurrency marloweOut
              marloweParams = MarloweParams { rolesCurrency = mtRolesCurrency marloweOut }
          -- Find the script UTxO to spend and the datum for outputs to the role payout address.
          (spend, datumOutputs) <-
            case marloweInBundle of
              -- This is a creation transaction.
              Nothing -> pure (extraSpend, [])
              -- This is a non-creation transaction.
              Just (marloweIn, spend) -> do
                -- Find the results of the previous transaction.
                let -- Fetch the validator.
                    validatorInfo = mtValidator marloweIn
                    validator = validatorInfoScriptOrReference validatorInfo
                    -- Build the redeemer.
                    redeemer = riRedeemer $ buildRedeemer (mtInputs marloweOut)
                    -- Build the datum.
                    inputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweIn) (mtState marloweIn)
                    -- Build the spending witness.
                    spend' = buildPayFromScript validator (Just inputDatum) redeemer spend
                    -- Handle merkleization.
                    merkles =
                      catMaybes
                        [ case input of
                          NormalInput _ -> Nothing
                          MerkleizedInput _ _ continuation ->
                            Just
                              ( -- Send the ancillary datum to the change address.
                                changeAddress
                              , -- Astonishing that this eUTxO can be spent without script or redeemer!
                                toTxOutDatumInTx era . Datum $ toBuiltinData continuation
                              , -- FIXME: Replace with protocol-dependent min-Ada.
                                lovelaceToValue 1_500_000
                              )
                        | input <- mtInputs marloweOut
                        ]
                -- Return the spending witness and the extra datum for demerkleization.
                pure (spend' : extraSpend, merkles)
          let -- Compute the script address.
              scriptAddress = viAddress $ mtValidator marloweOut
              -- Build the datum output to the script.
              outputDatum = diDatum $ buildMarloweDatum marloweParams (mtContract marloweOut) (mtState marloweOut)
          -- Determine how much value the script should receive.
          outputValue <-
            mconcat
              <$> sequence
                [ liftCli . toCardanoValue $ assetClassValue (AssetClass (currency, name)) amount
                | ((_, Token currency name), amount) <- AM.toList . accounts $ mtState marloweOut
                ]
          let -- Build the continuing output, if any.
              continue =
                do
                  guard (outputValue /= mempty)
                  pure $
                    buildPayToScript era scriptAddress outputValue outputDatum

              -- Compute the role-payout address.
              roleAddress = viAddress $ mtRoleValidator marloweOut
          -- Build the payments to the role-payout address.
          payments <-
            catMaybes
              <$> sequence
                [ case payee of
                  Party (Address network address) -> do
                    address' <- doWithShelleyBasedEra $ marloweAddressToCardanoAddress network address
                    money' <- liftCli $ toCardanoValue money
                    Just <$> ensureMinUtxo protocol (address', C.TxOutDatumNone, money')
                  Party (Role role) -> do
                    money' <- liftCli $ toCardanoValue money
                    let datum = toTxOutDatumInTx era . diDatum $ buildRoleDatum (Token rolesCurrency role)
                    Just <$> ensureMinUtxo protocol (roleAddress, datum, money')
                  Account _ -> pure Nothing
                | (payee, money) <-
                    bimap head mconcat . unzip
                      <$> (groupBy ((==) `on` fst) . sortBy (compare `on` fst))
                        [ (payee, money)
                        | payment@(Payment _ payee _ _) <- mtPayments marloweOut
                        , let money = paymentMoney payment
                        ]
                ]
          let -- Extract the roles needed to authorize the inputs.
              roles [] = []
              roles (input : inputs) =
                case getInputContent input of
                  IDeposit _ (Role role) _ _ -> role : roles inputs
                  IChoice (ChoiceId _ (Role role)) _ -> role : roles inputs
                  _ -> roles inputs
              -- Extract the value coming from an UTxO.
              incomingValue (txIn', TxOut _ value _ _)
                | txIn' `elem` fmap txIn spend = txOutValueToValue value
                | otherwise = mempty
          -- Compute the value coming from the script.
          incoming <- do
            let txIns = S.fromList $ spend <&> \PayFromScript{txIn} -> txIn
            fmap (mconcat . fmap incomingValue . M.toList . unUTxO)
              . queryUTxOs queryCtx
              . QueryUTxOByTxIn
              $ txIns

          -- Build the outputs for role tokens.
          roleOutputs <-
            sequence
              [ do
                value <- liftCli . toCardanoValue $ singleton (mtRolesCurrency marloweOut) role 1
                ensureMinUtxo protocol (changeAddress, C.TxOutDatumNone, value)
              | role <- roles $ mtInputs marloweOut
              ]
          txOuts <-
            traverse
              (uncurry3 makeTxOut')
              (payments <> roleOutputs <> datumOutputs)

          -- Select the coins.
          (collateral, extraInputs, revisedOutputs) <-
            selectCoins
              queryCtx
              incoming
              txOuts
              continue
              changeAddress
              defaultCoinSelectionStrategy
              Nothing
          -- Build the transaction body.
          body <-
            doWithShelleyBasedEra $
              buildBody
                queryCtx
                spend
                continue
                []
                extraInputs
                revisedOutputs
                (Just collateral)
                changeAddress
                (mtRange marloweOut)
                (hashSigningKey <$> signingKeys)
                TxMintNone
                metadata
                printStats
                invalid
          void $ submitBody txBuildupCtx body signingKeys invalid
          -- Return the transaction body.
          pure body
    go marloweOut'

-- | Convert a Marlowe party address to a Cardano address.
marloweAddressToCardanoAddress
  :: forall era m
   . (CS.IsShelleyBasedEra era)
  => (MonadError CliError m)
  => Network
  -> Address
  -> m (AddressInEra era)
marloweAddressToCardanoAddress network address =
  do
    address' <-
      liftCli $
        toCardanoAddressInEra
          (if network == mainnet then Mainnet else Testnet (NetworkMagic 1))
          address
    address'' <-
      case address' of
        AddressInEra (CS.ShelleyAddressInEra _) address'' -> pure address''
        _ -> throwError "Byron addresses are not supported."
    let era = CS.shelleyBasedEra
    pure $ AddressInEra (CS.ShelleyAddressInEra era) address''

-- | Convert a cardano address to a Marlowe party address.
marloweAddressFromCardanoAddress
  :: (MonadError CliError m)
  => AddressInEra era
  -> m (Network, Address)
marloweAddressFromCardanoAddress address =
  do
    network' <-
      case address of
        AddressInEra _ (CS.ShelleyAddress network _ _) -> pure $ if network == LC.Mainnet then mainnet else testnet
        _ -> throwError "Byron addresses are not supported."
    pure (network', toPlutusAddress address)

toPlutusAddress :: AddressInEra era -> PV1.Address
toPlutusAddress address = PV1.Address (cardanoAddressCredential address) (cardanoStakingCredential address)

cardanoAddressCredential :: AddressInEra era -> PV1.Credential
cardanoAddressCredential (AddressInEra C.ByronAddressInAnyEra (C.ByronAddress address)) =
  PV1.PubKeyCredential $
    PV1.PubKeyHash $
      PV1.toBuiltin $
        addrToBase58 address
cardanoAddressCredential (AddressInEra _ (CS.ShelleyAddress _ paymentCredential _)) =
  case CS.fromShelleyPaymentCredential paymentCredential of
    C.PaymentCredentialByKey paymentKeyHash ->
      PV1.PubKeyCredential $
        PV1.PubKeyHash $
          PV1.toBuiltin $
            serialiseToRawBytes paymentKeyHash
    C.PaymentCredentialByScript scriptHash ->
      PV1.ScriptCredential $ toPlutusScriptHash scriptHash

cardanoStakingCredential :: AddressInEra era -> Maybe PV1.StakingCredential
cardanoStakingCredential (AddressInEra C.ByronAddressInAnyEra _) = Nothing
cardanoStakingCredential (AddressInEra _ (CS.ShelleyAddress _ _ stakeAddressReference)) =
  case CS.fromShelleyStakeReference stakeAddressReference of
    NoStakeAddress -> Nothing
    (StakeAddressByValue stakeCredential) ->
      Just (PV1.StakingHash $ fromCardanoStakeCredential stakeCredential)
    StakeAddressByPointer{} -> Nothing -- Not supported
  where
    fromCardanoStakeCredential :: C.StakeCredential -> PV1.Credential
    fromCardanoStakeCredential (CS.StakeCredentialByKey stakeKeyHash) =
      PV1.PubKeyCredential $
        PV1.PubKeyHash $
          PV1.toBuiltin $
            serialiseToRawBytes stakeKeyHash
    fromCardanoStakeCredential (CS.StakeCredentialByScript scriptHash) = PV1.ScriptCredential (toPlutusScriptHash scriptHash)

toPlutusScriptHash :: C.ScriptHash -> PV1.ScriptHash
toPlutusScriptHash = PV1.ScriptHash . PV1.toBuiltin . serialiseToRawBytes

-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFunds
  :: forall era m
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => LocalNodeConnectInfo
  -- ^ The connection info for the local node.
  -> FilePath
  -- ^ The JSON file with the Marlowe state and contract.
  -> TokenName
  -- ^ The role name for the redemption.
  -> AddressInEra era
  -- ^ The change address.
  -> [SigningKeyFile]
  -- ^ The files for required signing keys.
  -> Maybe FilePath
  -- ^ The file containing JSON metadata, if any.
  -> TxFile
  -- ^ The output file for the transaction.
  -> Maybe Second
  -- ^ Number of seconds to wait for the transaction to be confirmed, if it is to be confirmed.
  -> PrintStats
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m TxId
  -- ^ Action to build the transaction body.
autoWithdrawFunds connection marloweOutFile roleName changeAddress signingKeyFiles metadataFile (TxFile txFile) timeout printStats invalid =
  do
    era <- askEra
    -- Read the Marlowe transaction information that was used to populate the role-payout address.
    SomeMarloweTransaction _ era' marloweOut <- decodeFileStrict marloweOutFile
    let go :: forall lang. (C.IsPlutusScriptLanguage lang) => MarloweTransaction lang era -> m TxId
        go marloweOut' = do
          -- Read the signing keys.
          signingKeys <- mapM readSigningKey signingKeyFiles
          -- Read any metadata for the transaction.
          metadata <- readMaybeMetadata metadataFile
          let token = Token (mtRolesCurrency marloweOut') roleName
              -- Compute the validator information.
              rolePayoutValidator = mtRoleValidator marloweOut'

          -- Write the transaction file.
          (txBody :: TxBody era) <-
            autoWithdrawFundsImpl
              (mkNodeTxBuildup connection timeout)
              token
              rolePayoutValidator
              Nothing
              changeAddress
              signingKeys
              Nothing
              metadata
              printStats
              invalid

          doWithShelleyBasedEra
            $ liftCliIO
            $ writeFileTextEnvelope (File txFile) Nothing
            $ C.makeSignedTransaction [] txBody
          pure $ getTxId txBody

    case testSameEra era era' of
      Just Refl -> go marloweOut
      Nothing -> throwError $ fromString $ "Running in " <> show era <> ", read file in " <> show era'

type FilterPayouts era = [AUTxO era] -> [AUTxO era]

-- | Withdraw funds for a specific role from the role address, without selecting inputs or outputs.
autoWithdrawFundsImpl
  :: forall era lang m
   . (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (C.IsPlutusScriptLanguage lang)
  => TxBuildupContext era
  -- ^ The connection info for the local node.
  -> Token
  -- ^ The role token.
  -> ValidatorInfo lang era
  -- ^ The role payout validator.
  -> Maybe (SlotNo, SlotNo)
  -- ^ Transaction validity range.
  -> AddressInEra era
  -- ^ The change address.
  -> [SomePaymentSigningKey]
  -- ^ Required signing keys.
  -> Maybe (FilterPayouts era)
  -- ^ Filtering option so transaction limits can be imposed.
  -> TxMetadataInEra era
  -- ^ The file containing JSON metadata, if any.
  -> PrintStats
  -- ^ Whether to print statistics about the transaction.
  -> Bool
  -- ^ Assertion that the transaction is invalid.
  -> m (TxBody era)
  -- ^ Action to build the transaction body.
autoWithdrawFundsImpl txBuildupCtx token validatorInfo range changeAddress signingKeys possibleFilter metadata (PrintStats printStats) invalid =
  do
    let queryCtx = toQueryContext txBuildupCtx
    -- Fetch the protocol parameters.
    protocol <- getLedgerProtocolParams queryCtx
    let Token rolesCurrency roleName = token
        filterPayoutUtxos = fromMaybe id possibleFilter
        -- Build the datum corresponding to the role name.
        roleDatum = buildRoleDatum token
    -- Compute the hash of the role name.
    roleHash <- liftCli . toCardanoScriptDataHash . diHash $ roleDatum
    let -- Fetch the role-payout validator script.
        roleScript = validatorInfoScriptOrReference validatorInfo
        -- Fetch the role address.
        roleAddress = viAddress validatorInfo
        -- Build the necessary redeemer.
        roleRedeemer = riRedeemer buildRoleRedeemer
        -- Test if a `TxOut` contains the role token datum.
        checkRole (TxOut _ _ datum _) =
          case datum of
            TxOutDatumInline _ _ -> False
            TxOutDatumNone -> False
            TxOutDatumHash _ datumHash -> datumHash == roleHash
    -- Find the role token.
    allPayouts <-
      fmap (map AUTxO . filter (checkRole . snd) . M.toList . unUTxO)
        . queryByAddress queryCtx
        $ roleAddress
    -- Set the value of one role token.
    role <- liftCli $ toCardanoValue $ singleton rolesCurrency roleName 1
    let utxos = filterPayoutUtxos allPayouts
        -- Build the spending from the script.
        spend = buildPayFromScript roleScript (Just $ diDatum roleDatum) roleRedeemer . fst . unAUTxO <$> utxos
        -- Find how much is being spent from the script.
        -- The output value should include the spending from the script and the role token.
        withdrawn = mconcat [txOutValueToValue value | AUTxO (_, TxOut _ value _ _) <- utxos]
    -- Ensure that the output meets the min-Ada requirement.
    output <- ensureMinUtxo protocol (changeAddress, C.TxOutDatumNone, withdrawn <> role) >>= uncurry3 makeTxOut'

    -- Select the coins.
    (collateral, extraInputs, revisedOutputs) <-
      selectCoins
        queryCtx
        withdrawn
        [output]
        Nothing
        changeAddress
        defaultCoinSelectionStrategy
        Nothing
    -- Build the transaction body.
    body <-
      doWithShelleyBasedEra $
        buildBody
          queryCtx
          spend
          Nothing
          []
          extraInputs
          revisedOutputs
          (Just collateral)
          changeAddress
          range
          (hashSigningKey <$> signingKeys)
          TxMintNone
          metadata
          printStats
          invalid
    void $ submitBody txBuildupCtx body signingKeys invalid
    -- Return the transaction body.
    pure body

toCardanoValue :: Value -> Either String C.Value
toCardanoValue =
  fmap C.valueFromList . traverse toSingleton . flattenValue
  where
    toSingleton (cs, tn, q) =
      toCardanoAssetId (assetClass cs tn) <&> (,C.Quantity q)

toCardanoAssetId :: AssetClass -> Either String C.AssetId
toCardanoAssetId (AssetClass (currencySymbol, tokenName))
  | currencySymbol == adaSymbol && tokenName == adaToken =
      pure C.AdaAssetId
  | otherwise =
      C.AssetId
        <$> toCardanoPolicyId currencySymbol
        <*> toCardanoAssetName tokenName

toCardanoAssetName :: TokenName -> Either String C.AssetName
toCardanoAssetName (TokenName bs) =
  first show $
    deserialiseFromRawBytes C.AsAssetName (fromBuiltin bs)

toCardanoPolicyId :: P.CurrencySymbol -> Either String C.PolicyId
toCardanoPolicyId (P.CurrencySymbol bs) =
  first show $
    deserialiseFromRawBytes C.AsPolicyId (fromBuiltin bs)

toCardanoScriptDataHash :: DatumHash -> Either String (C.Hash C.ScriptData)
toCardanoScriptDataHash (DatumHash bs) =
  first show $
    deserialiseFromRawBytes (C.AsHash C.AsScriptData) (fromBuiltin bs)

toCardanoAddressInEra :: C.NetworkId -> Address -> Either String (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (P.Address addressCredential addressStakingCredential) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)
    <$> ( C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential
        )

toCardanoPaymentCredential :: Credential -> Either String C.PaymentCredential
toCardanoPaymentCredential (PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash pubKeyHash
toCardanoPaymentCredential (ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoPaymentKeyHash :: P.PubKeyHash -> Either String (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (P.PubKeyHash bs) =
  let bsx = fromBuiltin bs
   in first show $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

toCardanoStakeAddressReference :: Maybe P.StakingCredential -> Either String C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (P.StakingHash credential)) =
  C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just P.StakingPtr{}) = Left "StakingPointersNotSupported"

toCardanoStakeCredential :: Credential -> Either String C.StakeCredential
toCardanoStakeCredential (PubKeyCredential pubKeyHash) = CS.StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (ScriptCredential validatorHash) = CS.StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoScriptHash :: P.ScriptHash -> Either String C.ScriptHash
toCardanoScriptHash (P.ScriptHash bs) =
  first show $
    deserialiseFromRawBytes C.AsScriptHash $
      fromBuiltin bs

toCardanoStakeKeyHash :: P.PubKeyHash -> Either String (C.Hash C.StakeKey)
toCardanoStakeKeyHash (P.PubKeyHash bs) =
  first show $
    deserialiseFromRawBytes (C.AsHash C.AsStakeKey) $
      fromBuiltin bs
