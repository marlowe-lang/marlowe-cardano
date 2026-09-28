{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.CLI.Interpret where

import Language.Marlowe.CLI.Transaction (buildPayFromScript, findMarloweScriptsRefs, publishImpl, selectUtxosImpl)
import Language.Marlowe.CLI.Types (
  AUTxO (..),
  CoinSelectionStrategy (CoinSelectionStrategy),
  MarloweScriptsRefs (..),
  MarloweTransaction (..),
  OutputQuery (..),
  OutputQueryResult (..),
  PayFromScript,
  PrintStats (PrintStats),
  PublishingStrategy (PublishAtAddress, PublishPermanently),
  ValidatorInfo (..),
  fromUTxO,
  toQueryContext,
  toSlotRoundedPlutusPOSIXTime,
  unAUTxO,
  validatorInfoScriptOrReference,
 )
import Language.Marlowe.Client qualified as Client
import Language.Marlowe.Core.V1.Semantics qualified as M
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Pretty (pretty)
import PlutusLedgerApi.V1.Value qualified as P

import Cardano.Api (
  IsShelleyBasedEra,
  LocalNodeConnectInfo (LocalNodeConnectInfo),
  StakeAddressReference (NoStakeAddress),
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Contrib.Control.Monad.Except (note)
import Marlowe.Contrib.Foldable (anyFlipped, foldMapFlipped, foldMapMFlipped)
import Contrib.Data.List.Random (combinationWithRepetitions)
import Control.Lens (assign, coerced, modifying, use, view)
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError), liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (untilJust)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bifunctor qualified as Bifunctor
import Data.Coerce (coerce)
import Data.Foldable (fold, for_)
import Data.Function (on)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Traversable (for)
import Language.Marlowe.CLI.Run (
  autoRunTransactionImpl,
  autoWithdrawFundsImpl,
  initializeTransactionImpl,
  initializeTransactionUsingScriptRefsImpl,
  marloweAddressFromCardanoAddress,
  marloweAddressToCardanoAddress,
  prepareTransactionImpl,
  toCardanoAssetName,
 )
import Language.Marlowe.CLI.Sync (classifyOutputs, isMarloweOut)
import Language.Marlowe.CLI.Sync.Types (MarloweOut (ApplicationOut, moTxIn))
import Language.Marlowe.CLI.Test.CLI.Monad (runCli, runLabeledCli)
import Language.Marlowe.CLI.Test.CLI.Types (
  CLIContractInfo (CLIContractInfo, _ciContract, _ciCurrency, _ciPlan, _ciSubmitter, _ciThread, _ciWithdrawalsCheckPoints),
  CLIContracts (CLIContracts),
  CLIOperation (..),
  InterpretMonad,
  MarloweValidators (..),
  anyCLIMarloweThreadInputsApplied,
  connectionL,
  contractsL,
  costModelParamsL,
  eraL,
  getCLIMarloweThreadTransaction,
  getCLIMarloweThreadTxBody,
  printStatsL,
  protocolVersionL,
  publishedScriptsL,
  slotConfigL,
  txBuildupContextL,
 )
import Language.Marlowe.CLI.Test.Contract (ContractNickname (..))
import Language.Marlowe.CLI.Test.Contract.Source (Source (InlineContract, UseTemplate), useTemplate)
import Language.Marlowe.CLI.Test.InterpreterError (rethrowCliError, testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (logStoreLabeledMsg, logStoreMsgWith, throwLabeledError)
import Language.Marlowe.CLI.Test.Wallet.Interpret (
  addPublishingCosts,
  addWalletTransaction,
  decodeContractJSON,
  decodeInputJSON,
  decodeStateJSON,
  fetchWalletValue,
  findWalletByUniqueToken,
  getCurrency,
  getSingletonCurrency,
  getWallet,
  getWalletByAddress,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currency (Currency, ccCurrencySymbol),
  CurrencyNickname,
  Wallet (Wallet, _waAddress, _waSigningKey),
  WalletNickname (WalletNickname),
  ccPolicyId,
  faucetNickname,
 )
import Language.Marlowe.Cardano.Thread (
  anyMarloweThreadCreated,
  foldrMarloweThread,
  marloweThreadTxIn,
  overAnyMarloweThread,
 )
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, State (..))
import Language.Marlowe.Extended.V1 (ada)
import PlutusLedgerApi.V2 qualified as P
import PlutusTx.AssocMap qualified as AM
import PlutusTx.Builtins qualified as P

-- | Build the initial Marlowe state.
initialMarloweState :: AccountId -> Integer -> State
initialMarloweState party minAda =
  State
    { accounts = AM.singleton (party, ada) minAda
    , choices = AM.empty
    , boundValues = AM.empty
    , minTime = 1
    }

findCLIContractInfo
  :: (InterpretMonad env st m lang era)
  => Maybe ContractNickname
  -> m (ContractNickname, CLIContractInfo lang era)
findCLIContractInfo (Just nickname) = do
  CLIContracts contracts <- use contractsL
  note
    ( testExecutionFailed' $
        "[findCLIContractInfo] Marlowe contract structure was not found for a given nickname "
          <> show (coerce nickname :: String)
          <> "."
    )
    ((nickname,) <$> Map.lookup nickname contracts)
findCLIContractInfo Nothing = do
  CLIContracts contracts <- use contractsL
  let fnName :: String
      fnName = "findCLIContractInfo"
  case Map.toList contracts of
    [] -> throwLabeledError fnName (testExecutionFailed' "No contracts found.")
    [pair] -> pure pair
    _ -> throwLabeledError fnName (testExecutionFailed' "Multiple contracts found. Please specify a contract nickname.")

toOneLineJSON :: forall a. (A.ToJSON a) => a -> String
toOneLineJSON = TL.unpack . A.encodeToLazyText . A.toJSON

autoRunTransaction
  :: forall era env m st
   . (IsShelleyBasedEra era)
  => (InterpretMonad env st m C.PlutusScriptV2 era)
  => Maybe CurrencyNickname
  -> WalletNickname
  -> Maybe (MarloweTransaction C.PlutusScriptV2 era, C.TxIn)
  -> MarloweTransaction C.PlutusScriptV2 era
  -> Bool
  -> m (C.TxBody era, Maybe C.TxIn)
autoRunTransaction currency defaultSubmitter prev curr@MarloweTransaction{..} invalid = do
  let label = "autoRunTransaction" :: String
      log' = logStoreLabeledMsg label
      logWith = logStoreMsgWith label

      getInputContentParty = \case
        M.IDeposit _ party _ _ -> Just party
        M.IChoice (M.ChoiceId _ party) _ -> Just party
        M.INotify -> Nothing

      getInputParty = \case
        M.NormalInput inputContent -> getInputContentParty inputContent
        M.MerkleizedInput inputContent _ _ -> getInputContentParty inputContent

      parties = List.nub $ List.sort $ foldMapFlipped mtInputs (maybeToList . getInputParty)

  logWith
    "Applying marlowe inputs: "
    [ ("inputs", A.toJSON mtInputs)
    , ("state", A.toJSON mtState)
    , ("contract", A.toJSON mtContract)
    ]

  txBuildupCtx <- view txBuildupContextL
  era <- view eraL
  (submitterNickname, Wallet address _ skey _ _ _, possiblePayFromOpenRoles) <- do
    (wallets, paysFromOpenRole) <- foldMapMFlipped parties \case
      (M.Address network address) -> do
        addr <- rethrowCliError (marloweAddressToCardanoAddress network address)
        (wn, w) <- getWalletByAddress addr
        pure ([(wn, w)], [])
      (M.Role rn) -> case currency of
        Nothing -> throwError $ testExecutionFailed' "[autoRunTransaction] Contract requires a role currency which was not specified."
        Just cn ->
          findWalletByUniqueToken cn rn >>= \case
            --  We have not found owner of the token. It seems that it should belong to open role validator.
            --  So we need to:
            --    * search for utxo under open role script which contains role token
            --    * if found, use it as an extra input
            --    * extract reference input to the open role script from the context and pass it up
            Right (wn, w) -> do
              pure ([(wn, w)], [])
            Left err -> do
              Currency{ccPolicyId = policyId, ccCurrencySymbol = currencySymbol} <- getCurrency cn
              use publishedScriptsL >>= \case
                Nothing ->
                  throwError $
                    testExecutionFailed' $
                      "[autoRunTransaction] Contract requires role token but open role script was not published yet "
                        <> "and role token is not owned the open role script and was not found between wallets: "
                        <> err
                Just MarloweScriptsRefs{mrOpenRoleValidator = (_, openRoleValidatorInfo)} -> do
                  roleAssetName <-
                    liftEither $ Bifunctor.first (const $ testExecutionFailed' "TokenName conversion error") (toCardanoAssetName rn)
                  let openRoleAddress = viAddress openRoleValidatorInfo
                      roleTokenAssetId = C.AssetId policyId roleAssetName
                      queryCtx = toQueryContext txBuildupCtx
                  runCli era "[AutoRun]" (selectUtxosImpl queryCtx openRoleAddress (AssetOnly roleTokenAssetId)) >>= \case
                    OutputQueryResult{oqrMatching = fromUTxO -> (AUTxO (txIn, _) : _)} -> do
                      let scriptOrReference = validatorInfoScriptOrReference openRoleValidatorInfo
                          redeemer = P.Redeemer $ P.toBuiltinData P.emptyByteString
                          payFromOpenRole :: PayFromScript C.PlutusScriptV2
                          payFromOpenRole = buildPayFromScript scriptOrReference Nothing redeemer txIn

                      pure ([], [payFromOpenRole])
                    _ -> do
                      let bech32 = Text.pack . show . C.serialiseAddress $ openRoleAddress
                      throwError $
                        testExecutionFailed' $
                          "[autoRunTransaction] Contract requires role token but neither open role script or wallet contains it. "
                            <> "Open role script address: "
                            <> show bech32
                            <> ". Role token asset id: "
                            <> show roleTokenAssetId
                            <> " or currency symbol: "
                            <> show currencySymbol
    let compareNicknames = compare `on` fst
        theSameNickname (n1, _) (n2, _) = n1 == n2
        wallets' = List.nubBy theSameNickname . List.sortBy compareNicknames $ wallets
    case wallets' of
      [] -> do
        wallet <- getWallet defaultSubmitter
        pure (defaultSubmitter, wallet, paysFromOpenRole)
      [(wn, w)] -> pure (wn, w, paysFromOpenRole)
      _ -> throwError $ testExecutionFailed' "[autoRunTransaction] Multiple wallets found for parties."

  log' $ "Possible pay from open role: " <> show possiblePayFromOpenRoles
  log' $ "Submitter: " <> case submitterNickname of WalletNickname n -> n

  txBody <-
    runCli era label $
      autoRunTransactionImpl
        txBuildupCtx
        prev
        curr
        possiblePayFromOpenRoles
        address
        [skey]
        C.TxMetadataNone
        True
        invalid

  let C.TxBody C.TxBodyContent{..} = txBody
      mTxId = C.getTxId txBody

  log' $ "TxId:" <> show mTxId
  addWalletTransaction submitterNickname label "" txBody

  let meOuts = classifyOutputs mTxId txOuts
  case filter isMarloweOut meOuts of
    [ApplicationOut{moTxIn}] -> do
      log' $ "Marlowe output:" <> show moTxIn
      pure (txBody, Just moTxIn)
    [] -> pure (txBody, Nothing)
    _ ->
      throwError $ testExecutionFailed' "[AutoRun] Multiple Marlowe outputs detected - unable to handle them yet."

publishCurrentValidators
  :: forall env era st m
   . (C.IsShelleyBasedEra era)
  => (InterpretMonad env st m C.PlutusScriptV2 era)
  => Maybe Bool
  -> Maybe WalletNickname
  -> m (MarloweScriptsRefs C.PlutusScriptV2 era)
publishCurrentValidators publishPermanently possiblePublisher = do
  let walletNickname = fromMaybe faucetNickname possiblePublisher

  Wallet{_waAddress, _waSigningKey} <- getWallet walletNickname
  let label = "publishCurrentValidators" :: String
      publishingStrategy = case publishPermanently of
        Just True -> PublishPermanently NoStakeAddress
        _ -> PublishAtAddress _waAddress
  printStats <- view printStatsL
  era <- view eraL
  txBuildupCtx <- view txBuildupContextL
  let fnName :: String
      fnName = "publishCurrentValidators"
      logTraceMsg' = logStoreLabeledMsg fnName
      queryCtx = toQueryContext txBuildupCtx
  runCli era fnName (findMarloweScriptsRefs @_ queryCtx publishingStrategy printStats) >>= \case
    Just marloweScriptRefs@(MarloweScriptsRefs (AUTxO (mTxIn, _), mv) (AUTxO (pTxIn, _), pv) (AUTxO (orTxIn, _), orv)) -> do
      let logValidatorInfo ValidatorInfo{..} =
            logTraceMsg' $ Text.unpack (C.serialiseAddress viAddress)

      logTraceMsg' "Found already published scripts so using them."
      logTraceMsg' $ "Marlowe reference: " <> show mTxIn
      logValidatorInfo mv
      logTraceMsg' $ "Payout reference: " <> show pTxIn
      logValidatorInfo pv
      logTraceMsg' $ "Open role reference: " <> show orTxIn
      logValidatorInfo orv
      pure marloweScriptRefs
    Nothing -> do
      logStoreLabeledMsg fnName "Scripts not found so publishing them."
      (txBodies, refs) <-
        runCli era fnName $
          publishImpl
            txBuildupCtx
            _waSigningKey
            Nothing
            _waAddress
            publishingStrategy
            (CoinSelectionStrategy False False [])
            (PrintStats True)
      let publisherWalletNickname = fromMaybe faucetNickname possiblePublisher

      for_ txBodies $ addWalletTransaction publisherWalletNickname label ""
      addPublishingCosts walletNickname refs
      pure refs

interpret
  :: forall env era st m
   . (C.IsShelleyBasedEra era)
  => (InterpretMonad env st m C.PlutusScriptV2 era)
  => CLIOperation
  -> m ()
interpret co@Initialize{..} = do
  let submitterNickname = fromMaybe faucetNickname coSubmitter
      merkleize = Just True == coMerkleize

  marloweContract <- case coContractSource of
    InlineContract json -> decodeContractJSON json
    UseTemplate setup -> useTemplate coRoleCurrency setup

  logStoreLabeledMsg co $ "Contract: " <> show (pretty marloweContract)

  address <- _waAddress <$> getWallet submitterNickname
  submitterParty <- uncurry M.Address <$> rethrowCliError (marloweAddressFromCardanoAddress address)

  marloweParams <-
    Client.marloweParams <$> case coRoleCurrency of
      Nothing -> pure P.adaSymbol
      Just nickname -> do
        Currency{ccCurrencySymbol} <- getCurrency nickname
        pure ccCurrencySymbol

  marloweState <- case (coMinLovelace, coInitialState) of
    (Just (Ledger.Coin minAda), Nothing) -> pure $ initialMarloweState submitterParty minAda
    (Nothing, Just parametrizedStateJson) -> do
      decodeStateJSON parametrizedStateJson
    (Just _, Just _) ->
      throwLabeledError co $
        testExecutionFailed' "Both min lovelace and initial state were specified but only one is allowed."
    (Nothing, Nothing) ->
      throwLabeledError co $
        testExecutionFailed' "Neither min lovelace nor initial state were specified so unable to initialize Marlowe contract."

  era <- view eraL
  slotConfig <- view slotConfigL
  costModelParams <- view costModelParamsL
  protocolVersion <- view protocolVersionL
  LocalNodeConnectInfo{localNodeNetworkId} <- view connectionL

  let marloweValidators = fromMaybe (ReferenceCurrentValidators Nothing Nothing) coMarloweValidators
      mkContractNickname = do
        CLIContracts contracts <- use contractsL
        case coContractNickname of
          Nothing -> liftIO $ untilJust do
            suffix <- combinationWithRepetitions 8 ['a' .. 'z']
            let nickname = ContractNickname $ "contract-" <> suffix
            if isJust $ Map.lookup nickname contracts
              then pure Nothing
              else pure $ Just nickname
          Just nickname -> do
            when (isJust . Map.lookup nickname $ contracts) do
              throwLabeledError co $ testExecutionFailed' "Contract with a given nickname already exist."
            pure nickname

  marloweTransaction <- case marloweValidators of
    ReferenceCurrentValidators publishPermanently possiblePublisher -> do
      logStoreLabeledMsg co "Using reference scripts to the current Marlowe validator."
      refs <-
        use publishedScriptsL >>= \case
          Nothing -> do
            logStoreLabeledMsg co "Publishing the scripts."
            marloweScriptRefs <- publishCurrentValidators publishPermanently possiblePublisher
            assign publishedScriptsL (Just marloweScriptRefs)
            pure marloweScriptRefs
          Just refs -> do
            logStoreLabeledMsg co "Scripts were already published so using them."
            pure refs

      runLabeledCli era co $
        initializeTransactionUsingScriptRefsImpl
          marloweParams
          slotConfig
          refs
          NoStakeAddress
          marloweContract
          marloweState
          merkleize
          True
    InTxCurrentValidators -> do
      logStoreLabeledMsg co "Using in Tx scripts embedding strategy to initialize Marlowe contract."
      runLabeledCli era co $
        initializeTransactionImpl
          marloweParams
          slotConfig
          protocolVersion
          costModelParams
          localNodeNetworkId
          NoStakeAddress
          marloweContract
          marloweState
          Nothing
          merkleize
          True
    ReferenceRuntimeValidators ->
      throwLabeledError co $ testExecutionFailed' "Usage of reference runtime scripts is not supported yet."
  contractNickname <- mkContractNickname
  logStoreLabeledMsg co $ "Saving initialized contract " <> show (coerce contractNickname :: String)
  modifying (contractsL . coerced) $
    Map.insert contractNickname $
      CLIContractInfo
        { _ciContract = marloweContract
        , _ciPlan = marloweTransaction :| []
        , _ciThread = Nothing
        , _ciWithdrawalsCheckPoints = mempty
        , _ciCurrency = coRoleCurrency
        , _ciSubmitter = submitterNickname
        }
interpret co@Prepare{..} = do
  (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname
  let curr = List.NonEmpty.head _ciPlan
  inputs <- for coInputs decodeInputJSON
  slotConfig <- view slotConfigL
  minimumTime <- toSlotRoundedPlutusPOSIXTime slotConfig coMinimumTime
  maximumTime <- toSlotRoundedPlutusPOSIXTime slotConfig coMaximumTime
  era <- view eraL
  logStoreLabeledMsg co $ "Inputs: " <> show (pretty inputs)
  new <-
    runLabeledCli era co $
      prepareTransactionImpl
        curr
        inputs
        minimumTime
        maximumTime
        True

  let new' = case coOverrideMarloweState of
        Just customState -> new{mtState = customState}
        Nothing -> new
      plan = new' <| _ciPlan
      marloweContract' = marloweContract{_ciPlan = plan}

  modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
interpret co@Publish{..} = do
  logStoreLabeledMsg co "Using reference scripts to the current Marlowe validator."
  use publishedScriptsL >>= \case
    Nothing -> do
      logStoreLabeledMsg co "Publishing the scripts."
      marloweScriptRefs <- publishCurrentValidators coPublishPermanently coPublisher
      assign publishedScriptsL (Just marloweScriptRefs)
    Just _ -> do
      throwLabeledError co $ testExecutionFailed' "Scripts were already published during this test scenario."
interpret AutoRun{..} = do
  (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname
  let plan = do
        let whole = reverse $ List.NonEmpty.toList _ciPlan
            threadLength = foldrMarloweThread (const (+ 1)) 0
        case _ciThread of
          Just thread -> do
            let l = overAnyMarloweThread threadLength thread
            drop l whole
          Nothing -> whole
      step mTh mt = do
        let prev :: Maybe (MarloweTransaction C.PlutusScriptV2 era, C.TxIn)
            prev = do
              pmt <- overAnyMarloweThread getCLIMarloweThreadTransaction <$> mTh
              txIn <- overAnyMarloweThread marloweThreadTxIn =<< mTh
              pure (pmt, txIn)
            invalid = Just True == coInvalid
            defaultSubmitter = fromMaybe _ciSubmitter coSubmitter

        (txBody, mTxIn) <- autoRunTransaction _ciCurrency defaultSubmitter prev mt invalid
        case (mTh, mTxIn) of
          (Nothing, Nothing) -> throwError $ testExecutionFailed' "[AutoRun] Creation of the Marlowe thread failed."
          (Nothing, Just txIn) -> pure $ Just $ anyMarloweThreadCreated (mt, txBody) txIn
          (Just th, _) -> case anyCLIMarloweThreadInputsApplied (mt, txBody) mTxIn th of
            Just th' -> pure $ Just th'
            Nothing -> throwError $ testExecutionFailed' "[AutoRun] Extending of the Marlowe thread failed."

  thread' <- foldM step _ciThread plan
  let marloweContract' = marloweContract{_ciThread = thread'}
  modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
interpret co@Withdraw{..} = do
  (contractNickname, marloweContract@CLIContractInfo{..}) <- findCLIContractInfo coContractNickname

  marloweThread <- case _ciThread of
    Just marloweThread -> pure marloweThread
    Nothing -> throwLabeledError co $ testExecutionFailed' "Contract is not on the chain yet so there are not payouts as well."
  wallet@Wallet{_waAddress, _waSigningKey} <- getWallet coWalletNickname
  Currency{ccCurrencySymbol} <- maybe (snd <$> getSingletonCurrency) getCurrency _ciCurrency
  onChainValue <- fetchWalletValue wallet
  let roles =
        P.flattenValue onChainValue `foldMapFlipped` \(cs, tn, _) ->
          if cs == ccCurrencySymbol
            then [tn]
            else mempty
  when (roles == mempty) $
    throwLabeledError co $
      testExecutionFailed' $
        fold
          [ "Provided wallet "
          , show coWalletNickname
          , "has no roles associated with the given contract "
          , show coContractNickname
          ]

  txBuildupCtx <- view txBuildupContextL
  txBodies <- foldMapMFlipped roles \role -> do
    let lastWithdrawalCheckPoint = Map.lookup role _ciWithdrawalsCheckPoints
        threadTransactions :: [(MarloweTransaction C.PlutusScriptV2 era, C.TxId)]
        threadTransactions = do
          let step item acc = (getCLIMarloweThreadTransaction item, C.getTxId . getCLIMarloweThreadTxBody $ item) : acc
          overAnyMarloweThread (foldrMarloweThread step []) marloweThread

        possibleWithdrawals = takeWhile ((/=) lastWithdrawalCheckPoint . Just . snd) threadTransactions

        paymentRole (M.Payment _ (M.Party (M.Role r)) _ _) = Just r
        paymentRole _ = Nothing

        -- Sometimes we reuse the same currency across multiple tests (when Faucet is an issuer) so we
        -- need to filter out payouts which are really associated with this particular test
        -- case. We can identify them by matching them against a set of submitted transaction ids.
        filterPayoutsUTxOs utxos = do
          let txIds = map snd possibleWithdrawals
              txInId (C.TxIn txId _) = txId
          filter (flip elem txIds . txInId . fst . unAUTxO) utxos

    let anyWithdrawalsExist =
          possibleWithdrawals `anyFlipped` \(MarloweTransaction{..}, _) ->
            elem role . mapMaybe paymentRole $ mtPayments

    if anyWithdrawalsExist
      then do
        let roleToken = M.Token ccCurrencySymbol role
            MarloweTransaction{mtRoleValidator} :| _ = _ciPlan

        logStoreLabeledMsg co $
          "Withdrawing funds for role " <> show role <> " after application of inputs: " <> do
            let inputs = foldMapFlipped possibleWithdrawals \(MarloweTransaction{mtInputs}, _) -> mtInputs
            show inputs
        era <- view eraL
        txBody <-
          runLabeledCli era co $
            autoWithdrawFundsImpl
              txBuildupCtx
              roleToken
              mtRoleValidator
              Nothing
              _waAddress
              [_waSigningKey]
              (Just filterPayoutsUTxOs)
              C.TxMetadataNone
              (PrintStats True)
              False
        pure [txBody]
      else pure []

  for_ txBodies $ addWalletTransaction coWalletNickname co ""

  let newWithdrawals = foldMapFlipped roles \role ->
        Map.singleton role (C.getTxId . overAnyMarloweThread getCLIMarloweThreadTxBody $ marloweThread)
      marloweContract' = marloweContract{_ciWithdrawalsCheckPoints = newWithdrawals <> _ciWithdrawalsCheckPoints}
  modifying (contractsL . coerced) $ Map.insert contractNickname marloweContract'
