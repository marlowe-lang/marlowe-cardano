-- FIXME: Drop this pragma when porting is finished:
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Language.Marlowe.Runtime.Transaction.Builders where

import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds, UTCTime)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Cardano.Api as C
import qualified PlutusLedgerApi.V2 as PV2
import Log (MonadLog)
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy, MinAdaProvider (MinAdaProvider), initialMarloweState, invalidAddressesError, RolesPolicyId (RolesPolicyId), buildInitConstraints, buildApplyInputsConstraints)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (MarloweV1), MarloweTransactionMetadata, IsMarloweVersion (Contract), ContractId (ContractId), decodeMarloweTransactionMetadataLenient, Inputs, TransactionInputs, TransactionScriptOutput (TransactionScriptOutput, datum), TransactionOutput (TransactionOutput, payouts, scriptOutput), Payout (Payout), fromChainPayoutDatum, ContractWithAdjacency (ContractWithAdjacency), State)
import qualified Language.Marlowe.Runtime.Core.Api as Contract
import Language.Marlowe.Object.Types (ContractHash (ContractHash))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (MarloweScripts, marloweScript, payoutScript), ScriptDetails (ScriptDetails, scriptHash, scriptUTxOs), ReferenceScriptUtxo)
import Language.Marlowe.Runtime.Transaction.Constraints (SolveConstraints, WalletContext (changeAddress), HelpersContext (HelpersContext), MarloweContext (MarloweContext, scriptOutput, marloweAddress, payoutAddress, marloweScriptUTxO, payoutScriptUTxO, marloweScriptHash, payoutScriptHash))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Transaction.Api (RoleTokensConfig, Accounts, InitError(InitEraUnsupported, InitContractNotFound, ProtocolParamNoUTxOCostPerByte, InsufficientMinAdaDeposit, InitLoadMarloweContextFailed, InitToCardanoError, InitLoadHelpersContextFailed, InitSafetyAnalysisError, InitSafetyAnalysisFailed, InitConstraintError, InitTxOutputNotFound), ContractInitialized(ContractInitialized), LoadHelpersContextError, LoadMarloweContextError (MarloweScriptNotPublished, PayoutScriptNotPublished), unAccounts, ContractInitializedInEra (ContractInitializedInEra, contractId , rolesCurrency , metadata , txBody , marloweScriptHash , marloweScriptAddress , payoutScriptHash , payoutScriptAddress , version , datum , assets , safetyErrors), ApplyInputsError (ApplyInputsConstraintError, ApplyInputsEraUnsupported, ApplyInputsLoadHelpersContextFailed, ScriptOutputNotFound, ApplyInputsLoadMarloweContextFailed, ApplyInputsContractContinuationNotFound, ApplyInputsSafetyAnalysisError), InputsApplied(InputsApplied), InputsAppliedInEra (InputsAppliedInEra, metadata, inputs, safetyErrors, version, contractId, input, output, invalidBefore, invalidHereafter, txBody))

import Control.Monad.Trans.Except (runExceptT, ExceptT(ExceptT), throwE, except, withExceptT)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.Marlowe.Runtime.Transaction.Safety (
  mkAdjustMinUTxO, ThreadTokenAssetId (ThreadTokenAssetId), noContinuations, minAdaUpperBound, checkContract, mockLockedRolesContext,
  checkTransactions, mkLockedRolesContext,
  )
import Control.Monad (guard, unless)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first)
import Control.Error (note, MaybeT(MaybeT), runMaybeT)
import Language.Marlowe.Runtime.Cardano.Api (toCardanoPaymentCredential, fromCardanoAddressInEra, toCardanoStakeCredential, fromCardanoTxId, fromCardanoTxOutDatum, fromCardanoTxOutValue)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError (SafetyAnalysisTimeout))
import Control.Exception (SomeException, catch)
import qualified Control.Exception.Base as Exception
import qualified Control.DeepSeq as DeepSeq
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)
import Language.Marlowe.Runtime.ChainSync.Api (fromCardanoTxMetadata, mkTxOutAssets, SlotNo)
import qualified Data.List as List
import qualified Cardano.Ledger.Core as Ledger
import qualified Marlowe.Plutus.Semantics as V1
import Data.Map (Map)
import qualified Marlowe.Plutus.Semantics.Types as V1
import Data.Kind (Type)
import Control.Monad.Trans.Class (lift)
import PlutusTx.Functor ((<&>))
import Data.Traversable (for)
import Marlowe.Contrib.Foldable (foldMapMFlipped)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.These (These(This, That, These))

type GetContractSource v m = ContractHash -> m (Maybe (ContractWithAdjacency v))

type LoadHelpersContext m =
  forall v
   . MarloweVersion v
  -> Either (Chain.PolicyId, RoleTokensConfig) (Maybe ContractId)
  -> m (Either LoadHelpersContextError HelpersContext)

type LoadMarloweContext m =
  forall v
   . MarloweVersion v
  -> ContractId
  -> m (Either LoadMarloweContextError (MarloweContext v))

-- FIXME: Port those stubs
data Connector client (m :: Type -> Type) = Connector

data QueryClient request

data ContractRequest

execInit
  :: forall era m v
   . MonadUnliftIO m
  => C.IsCardanoEra era
  => MonadLog m
  => MkRoleTokenMintingPolicy m
  -> C.CardanoEra era
  -> GetContractSource v m
  -> MarloweScripts
  -> SolveConstraints era v
  -- -> C.LedgerProtocolParameters era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> WalletContext
  -> LoadHelpersContext m
  -> C.NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> Maybe Chain.TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Chain.Lovelace
  -> Accounts
  -> Either (Contract v) ContractHash
  -> NominalDiffTime
  -> m (Either InitError (ContractInitialized v))
execInit
  mkRoleTokenMintingPolicy
  era
  getContractSource
  marloweScripts
  solveConstraints
  protocolParameters
  walletContext
  loadHelpersContext
  networkId
  mStakeCredential
  version
  threadRole
  roleTokens
  metadata
  optMinAda
  accounts
  contract
  analysisTimeout = runExceptT do
  eon <- toBabbageEraOnwards (InitEraUnsupported $ C.AnyCardanoEra era) era
  let
    threadRole' = fromMaybe "" threadRole
    adjustMinUtxo = mkAdjustMinUTxO eon protocolParameters version
  dummyState <- do
    except $
      first invalidAddressesError $
        initialMarloweState
          adjustMinUtxo
          version
          accounts
          (fromMaybe mempty optMinAda)
          (MinAdaProvider walletContext.changeAddress)

  ((contract', continuations) :: (Contract v, Map ContractHash (Contract v))) <- case contract of
    Right hash -> case version of
      MarloweV1 -> do
        lift (getContractSource hash) >>= \case
          Nothing -> throwE $ InitContractNotFound hash
          Just (ContractWithAdjacency{contract = c, closure}) -> do
            continuations <- foldMapMFlipped (Set.delete hash closure) \h -> do
              lift (getContractSource h) >>= \case
                Nothing -> throwE $ InitContractNotFound h
                Just (ContractWithAdjacency{contract = c'}) -> pure $ Map.singleton h c'
            pure (c, continuations)
    Left c -> pure (c, noContinuations version)
  computedMinAdaDeposit <-
    except $
      note ProtocolParamNoUTxOCostPerByte $
        minAdaUpperBound eon protocolParameters version dummyState contract' continuations
  let minAda = fromMaybe computedMinAdaDeposit optMinAda
  unless (minAda >= computedMinAdaDeposit) $ throwE $ InsufficientMinAdaDeposit computedMinAdaDeposit

  ((datum, assets, possibleRolesPolicyId), constraints) <-
    ExceptT $ do
      buildInitConstraints
        mkRoleTokenMintingPolicy
        eon
        version
        walletContext
        roleTokens
        metadata
        minAda
        accounts
        adjustMinUtxo
        contract'
  marloweContext <-
    mkMarloweContext
      networkId
      version
      marloweScripts
      mStakeCredential
  possibleHelpersContext <- for possibleRolesPolicyId \(RolesPolicyId policyId) -> do
    withExceptT InitLoadHelpersContextFailed $
      ExceptT $
        loadHelpersContext version $
          Left (policyId, roleTokens)
  let
    helpersContext = case possibleHelpersContext of
      Nothing -> HelpersContext mempty (Chain.PolicyId "") mempty
      Just hc -> hc
    -- Fast analysis of safety: examines bounds for transactions.
    contractSafetyErrors = checkContract networkId (Just roleTokens) version datum continuations

  transactionSafetyErrors <- do
    -- FIXME: paluh
    -- FIXME: Use here "own policy" when we finally achieve the CIP-69 implementation
    let
      threadTokenAssetId = ThreadTokenAssetId (Chain.AssetId "" threadRole')
      lockedRolesContext = mockLockedRolesContext threadTokenAssetId adjustMinUtxo helpersContext
    ExceptT $
      liftIO $
        fmap (first InitSafetyAnalysisError) . limitAnalysisTime analysisTimeout $
          checkTransactions
            protocolParameters
            eon
            version
            marloweContext
            lockedRolesContext
            walletContext.changeAddress
            datum
            continuations
  let safetyErrors = contractSafetyErrors <> transactionSafetyErrors
  unless (Map.null (unAccounts accounts) || null safetyErrors) do
    throwE $ InitSafetyAnalysisFailed safetyErrors
  txBody <-
    except $
      first InitConstraintError $
        solveConstraints
          eon
          protocolParameters
          version
          (Left marloweContext)
          walletContext
          helpersContext
          constraints
  contractOutput <- except . note InitTxOutputNotFound
    $ findMarloweOutput marloweContext.marloweAddress txBody
  pure $
    ContractInitialized eon $
      ContractInitializedInEra
        { contractId = ContractId contractOutput
        , rolesCurrency = possibleRolesPolicyId <&> \(RolesPolicyId policyId) -> policyId
        , metadata = decodeMarloweTransactionMetadataLenient case txBody of
            C.TxBody C.TxBodyContent{..} -> case txMetadata of
              C.TxMetadataNone -> mempty
              C.TxMetadataInEra _ m -> fromCardanoTxMetadata m
        , txBody
        , marloweScriptHash = marloweContext.marloweScriptHash
        , marloweScriptAddress = marloweContext.marloweAddress
        , payoutScriptHash = marloweContext.payoutScriptHash
        , payoutScriptAddress = marloweContext.payoutAddress
        , version
        , datum
        , assets
        , safetyErrors
        }

toBabbageEraOnwards
  :: (Monad m) => e -> C.CardanoEra era -> ExceptT e m (C.BabbageEraOnwards era)
toBabbageEraOnwards e = C.inEonForEra (throwE e) pure

lookupMarloweScriptUtxo :: C.NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupMarloweScriptUtxo networkId MarloweScripts{marloweScript = ScriptDetails{scriptHash = marloweScriptHash, scriptUTxOs}} =
  note (MarloweScriptNotPublished marloweScriptHash) $
    Map.lookup networkId scriptUTxOs

lookupPayoutScriptUtxo :: C.NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupPayoutScriptUtxo networkId MarloweScripts{payoutScript = ScriptDetails{scriptHash = payoutScriptHash, scriptUTxOs}} =
  note (PayoutScriptNotPublished payoutScriptHash) $
    Map.lookup networkId scriptUTxOs

findMarloweOutput :: forall era. C.IsCardanoEra era => Chain.Address -> C.TxBody era -> Maybe Chain.TxOutRef
findMarloweOutput address = \case
  body@(C.TxBody C.TxBodyContent{..}) ->
    fmap (Chain.TxOutRef (fromCardanoTxId $ C.getTxId body) . fst) $
      List.find (isToCurrentScriptAddress . snd) $
        zip [minBound ..] txOuts
  where
    isToCurrentScriptAddress (C.TxOut address' _ _ _) =
      address == fromCardanoAddressInEra (C.cardanoEra @era) address'

mkMarloweContext
  :: (MonadUnliftIO m)
  => C.NetworkId
  -> MarloweVersion v
  -> MarloweScripts
  -> Maybe Chain.StakeCredential
  -> ExceptT InitError m (MarloweContext v)
mkMarloweContext networkId version marloweScripts mStakeCredential = do
  let
    scripts@MarloweScripts{marloweScript = marloweScriptDetails, payoutScript = payoutScriptDetails} = marloweScripts
    ScriptDetails{scriptHash = marloweScriptHash} = marloweScriptDetails
    ScriptDetails{scriptHash = payoutScriptHash} = payoutScriptDetails
  mCardanoStakeCredential <- except $ traverse (note InitToCardanoError . toCardanoStakeCredential) mStakeCredential
  marlowePaymentCredential <- except . note InitToCardanoError . toCardanoPaymentCredential $ Chain.ScriptCredential marloweScriptHash
  payoutPaymentCredential <- except . note InitToCardanoError . toCardanoPaymentCredential $ Chain.ScriptCredential payoutScriptHash
  let
      stakeReference = maybe C.NoStakeAddress C.StakeAddressByValue mCardanoStakeCredential
      marloweAddress =
        fromCardanoAddressInEra C.BabbageEra $
          C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
            C.makeShelleyAddress
              networkId
              marlowePaymentCredential
              stakeReference
      payoutAddress =
        fromCardanoAddressInEra C.BabbageEra $
          C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
            C.makeShelleyAddress
              networkId
              payoutPaymentCredential
              C.NoStakeAddress
  except $ first InitLoadMarloweContextFailed do
    marloweScriptUTxO <- lookupMarloweScriptUtxo networkId scripts
    payoutScriptUTxO <- lookupPayoutScriptUtxo networkId scripts
    pure
      MarloweContext
        { scriptOutput = Nothing
        , marloweAddress
        , payoutAddress
        , marloweScriptUTxO
        , payoutScriptUTxO
        , marloweScriptHash
        , payoutScriptHash
        }

limitAnalysisTime :: NominalDiffTime -> IO (Either String [SafetyError]) -> IO (Either String [SafetyError])
limitAnalysisTime timeout analysis = do
  let analysis' = runExceptT do
        let go = do
              errs <- analysis >>= (Exception.evaluate . DeepSeq.force)
              pure $ Right errs
        res <-
          ExceptT $
            go `catch` \(e :: SomeException) -> do
              pure $ Left (show e)
        except res
  res <-
    race
      (threadDelay (floor $ nominalDiffTimeToSeconds timeout * 1_000_000))
      analysis'
  case res of
    -- Timeout reached
    Left () -> pure $ Right [SafetyAnalysisTimeout]
    -- Analysis finished with exception or internal error
    Right (Left e) -> pure $ Left e
    -- Analysis finished successfully
    Right (Right res') -> pure $ Right res'

findPayouts
  :: forall era v
   . C.IsCardanoEra era
  => MarloweVersion v
  -> Chain.Address
  -> C.TxBody era
  -> Map Chain.TxOutRef (Payout v)
findPayouts version address body@(C.TxBody C.TxBodyContent{..}) =
  Map.fromDistinctAscList $ mapMaybe (uncurry parsePayout) $ zip [minBound ..] txOuts
  where
    txId = fromCardanoTxId $ C.getTxId body
    parsePayout :: Chain.TxIx -> C.TxOut C.CtxTx era -> Maybe (Chain.TxOutRef, Payout v)
    parsePayout txIx (C.TxOut addr value txOutDatum _) = do
      guard $ fromCardanoAddressInEra (C.cardanoEra @era) addr == address
      datum <- fromCardanoTxOutDatum txOutDatum >>= \case
        This _h -> Nothing
        That d -> Just d
        These _h d -> Just d
      datum' <- fromChainPayoutDatum version datum
      assets <- mkTxOutAssets $ fromCardanoTxOutValue value
      pure (Chain.TxOutRef txId txIx, Payout address assets datum')

-- | Build up continuations closure map for a contract.
-- We don't want to just compute the root hash of the contract and ask the store for the closure because
-- we can have more granular merkleization in the future which sometimes does not merkleize every step
-- in the contract. In other words the root hash could be missing from the store.
getContractContinuations
  :: (Monad m)
  => GetContractSource v m
  -> V1.Contract
  -> m (Maybe (Map ContractHash (Contract v)))
getContractContinuations getContract contract = runMaybeT do
  let getCaseContinuationHashes (V1.MerkleizedCase _ h) = [h]
      getCaseContinuationHashes (V1.Case _ continuation) = getContractContinuationHashes continuation

      getContractContinuationHashes (V1.When cases _ continuation) =
        foldMap getCaseContinuationHashes cases <> getContractContinuationHashes continuation
      getContractContinuationHashes (V1.If _ trueContinuation falseContinuation) =
        getContractContinuationHashes trueContinuation <> getContractContinuationHashes falseContinuation
      getContractContinuationHashes (V1.Pay _ _ _ _ continuation) = getContractContinuationHashes continuation
      getContractContinuationHashes (V1.Let _ _ continuation) = getContractContinuationHashes continuation
      getContractContinuationHashes V1.Close = []
      getContractContinuationHashes (V1.Assert _ continuation) = getContractContinuationHashes continuation

      childrenHashes :: Set PV2.BuiltinByteString
      childrenHashes = Set.fromList $ getContractContinuationHashes contract

      toContractHash = ContractHash . PV2.fromBuiltin

      childrenHashes' :: Set ContractHash
      childrenHashes' = Set.map toContractHash childrenHashes

      getContract' = MaybeT . getContract

  childContracts :: [Contract.ContractWithAdjacency v] <- for (Set.toList childrenHashes') getContract'
  let childrenClosure = flip foldMap childContracts \Contract.ContractWithAdjacency{closure} -> closure

  (closureContracts :: [Contract.ContractWithAdjacency v]) <- do
    let hs = Set.toList $ Set.difference childrenClosure childrenHashes'
    for hs getContract'
  let allContracts = childContracts <> closureContracts
      continuations = Map.fromList $ flip fmap allContracts \Contract.ContractWithAdjacency{contract = c, contractHash = ch} -> (ch, c)
  pure continuations

execApplyInputs
  :: MonadUnliftIO m
  => C.IsCardanoEra era
  => MonadLog m
  => (Contract v -> State v -> TransactionInputs v -> m (Maybe (TransactionInputs v)))
  -> C.CardanoEra era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> GetContractSource v m
  -> m SlotNo
  -> C.SystemStart
  -> C.EraHistory
  -> SolveConstraints era v
  -- Those two were originally loaders:
  -> WalletContext
  -> LoadMarloweContext m
  -> LoadHelpersContext m
  -> C.NetworkId
  -> MarloweVersion v
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> NominalDiffTime
  -> m (Either ApplyInputsError (InputsApplied v))
execApplyInputs
  merkleizeInputs
  era
  protocolParameters
  getContractSource
  getCurrentSlotNo
  systemStart
  eraHistory
  solveConstraints
  walletContext
  loadMarloweContext
  -- marloweContext@MarloweContext{..}
  loadHelpersContext
  networkId
  version@MarloweV1
  contractId
  metadata
  invalidBefore'
  invalidHereafter'
  inputs
  analysisTimeout = runExceptT do
    eon <- toBabbageEraOnwards (ApplyInputsEraUnsupported $ C.AnyCardanoEra era) era
    helpersContext <-
      withExceptT ApplyInputsLoadHelpersContextFailed $ ExceptT $ loadHelpersContext version $ Right $ Just contractId
    marloweContext@MarloweContext{marloweAddress, payoutAddress, scriptOutput} <-
      withExceptT ApplyInputsLoadMarloweContextFailed $
        ExceptT $
          loadMarloweContext version contractId
    tipSlot <- lift getCurrentSlotNo
    scriptOutput'@TransactionScriptOutput{datum = inputDatum} <-
      except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    let (contract, state) = case version of
          MarloweV1 -> case inputDatum of
            V1.MarloweData{..} -> do
              (marloweContract, marloweState)
    ((invalidBefore, invalidHereafter, mAssetsAndDatum, inputs'), constraints) <-
      buildApplyInputsConstraints
        merkleizeInputs
        systemStart
        eraHistory
        version
        scriptOutput'
        tipSlot
        metadata
        invalidBefore'
        invalidHereafter'
        inputs
    txBody <-
      except $
        first ApplyInputsConstraintError $
          solveConstraints eon protocolParameters version (Left marloweContext) walletContext helpersContext constraints

    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output =
          TransactionOutput
            { payouts = findPayouts version payoutAddress txBody
            , scriptOutput = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
            }

    continuations <- case version of
      MarloweV1 -> lift (getContractContinuations getContractSource contract) >>= \case
        Nothing -> throwE ApplyInputsContractContinuationNotFound
        Just c -> pure c

    let -- Fast analysis of safety: examines bounds for transactions.
    -- FIXME: We should verify minting policy here as well:
    --  * we should check if trusted minting policy was used
    --  * we should check the role where minted as NFTs or they were redundant (do we check this in creation?)
    -- Slow analysis of safety: examines all possible transactions.
    safetyErrors <- case mAssetsAndDatum of
      Nothing -> pure []
      Just (_, datum) -> do
        let lockedRolesContext = mkLockedRolesContext helpersContext
            contractSafetyErrors = checkContract networkId Nothing version datum continuations
        transactionSafetyErrors <-
          ExceptT $
            liftIO $
              fmap (first ApplyInputsSafetyAnalysisError) . limitAnalysisTime analysisTimeout $
                checkTransactions
                  protocolParameters
                  eon
                  version
                  marloweContext
                  lockedRolesContext
                  walletContext.changeAddress
                  datum
                  continuations

        pure $ contractSafetyErrors <> transactionSafetyErrors
    pure $
      InputsApplied eon $
        InputsAppliedInEra
          { metadata = decodeMarloweTransactionMetadataLenient case txBody of
              C.TxBody C.TxBodyContent{..} -> case txMetadata of
                C.TxMetadataNone -> mempty
                C.TxMetadataInEra _ m -> fromCardanoTxMetadata m
          , inputs = inputs'
          , safetyErrors
          , version
          , ..
          }

