{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints (
  Accounts,
  AdjustMinUTxO (..),
  ApplyResults,
  InvalidAddresses (..),
  MkRoleTokenMintingPolicy,
  MinAdaProvider (..),
  RolesPolicyId (..),
  ThreadTokenAssetId (..),
  buildApplyInputsConstraints,
  buildInitConstraints,
  buildWithdrawConstraints,
  initialMarloweState,
  initialMarloweDatum,
  invalidAddressesError,
  marloweAccountsAssets,
  safeLovelace,
) where

import Cardano.Api (BabbageEraOnwards(..), EraHistory (..))
import Cardano.Api.Address qualified as C
import Cardano.Api.Plutus qualified as C
import Cardano.Ledger.BaseTypes qualified as CL (Network(..))
import Marlowe.Plutus.Semantics.Types.Address qualified as V1
import Control.Category ((>>>))
import Control.Error (ExceptT, note)
import Control.Error.Util (hush)
import Control.Monad (join, unless, when, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (..), for_, traverse_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe
    ( fromMaybe, listToMaybe, maybeToList, isNothing )
import Data.SOP.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Unsafe.Coerce (unsafeCoerce)
import Marlowe.Plutus.Semantics (TransactionInput(..))
import Marlowe.Plutus.Semantics qualified as V1
import Marlowe.Plutus.Semantics.Types qualified as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoSlotNo,
  plutusScriptHash,
  toCardanoPlutusScript,
  toCardanoSlotNo,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (..),
  AssetId (..),
  Assets (..),
  Lovelace (..),
  Metadata (..),
  PaymentKeyHash (..),
  PolicyId (..),
  Quantity (Quantity),
  SlotNo,
  TokenName (..),
  Tokens (..),
  TransactionMetadata (..),
  TransactionOutput (..),
  TxOutAssets (..),
  TxOutRef,
  UTxO (UTxO),
  mkTxOutAssets,
  toCardanoAddressAny,
  toUTxOsList,
  unInterpreter,
  unQuantity,
  unsafeTxOutAssets, scriptHashToPolicyId,
 )
import Language.Marlowe.Runtime.ChainSync.Api qualified as CS
import Language.Marlowe.Runtime.Core.Api (
  IsMarloweVersion (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (..),
  TransactionScriptOutput (..),
  fromChainPayoutDatum,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..))
import Language.Marlowe.Runtime.Plutus.V2.Api (
  fromPlutusValue,
  toAssetId,
  toPlutusAddress,
  toPlutusCurrencySymbol,
  toPlutusTokenName,
 )
import Language.Marlowe.Runtime.Transaction.Api (
  Account (..),
  Accounts (..),
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  InitBuildupError (
    AddressesDecodingFailed,
    InvalidInitialState,
    MintingScriptDecodingFailed,
    MintingUtxoSelectionFailed
  ),
  InitError (..),
  Destination (..),
  Mint (..),
  MintRole (..),
  RoleTokensConfig (..),
  WithdrawError (..),
  encodeRoleTokenMetadata,
  getTokenQuantities,
  mkAccounts,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  MintingSeed (MintingSeed),
  TxConstraints (..),
  WalletContext (WalletContext),
  mustConsumeMarloweOutput,
  mustDistributeRoleToken,
  mustMintRoleToken,
  mustPayToAddress,
  mustPayToRole,
  mustSendMarloweOutput,
  mustSpendRoleToken,
  requiresMetadata,
  requiresSignature, mustMintThreadToken, mustBurnThreadToken,
 )
import Language.Marlowe.Runtime.Transaction.Constraints qualified as TxConstraints
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime, toRelativeTime)
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraSummary (..),
  PastHorizonException (..),
  Summary (..),
  interpretQuery,
  slotToWallclock,
  wallclockToSlot,
 )
import Ouroboros.Network.Block qualified as O
import PlutusLedgerApi.V2 qualified as PV2
import Marlowe.Plutus.Semantics.Types (AccountId, Token)
import qualified PlutusTx.AssocMap as AM

type TxConstraintsBuilderM err era v m a = WriterT (TxConstraints era v) (ExceptT err m) a

newtype AdjustMinUTxO = AdjustMinUTxO {runAdjustMinUTxO :: TxOutAssets -> TxOutAssets}

runTxConstraintsBuilder
  :: MarloweVersion v
  -> TxConstraintsBuilderM err era v m a
  -> m (Either err (a, TxConstraints era v))
runTxConstraintsBuilder v = runExceptT . runWriterT . withMarloweVersion v

-- | Inits a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildInitConstraints
  :: forall era v m
   . (Monad m)
  => MkRoleTokenMintingPolicy m
  -- ^ A validator creator for the role token minting policy.
  -> BabbageEraOnwards era
  -- ^ The era in which the transaction is being built. Requires reference scripts.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> WalletContext
  -- ^ The wallet used to mint tokens.
  -> RoleTokensConfig
  -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata
  -- ^ Metadata to add to the transaction.
  -> Lovelace
  -- ^ The lower bound on the ada in the initial state.
  -> Accounts
  -- ^ User-defined initial account balances
  -> AdjustMinUTxO
  -- ^ Adjust a value to account for the minimum UTxO ledger rule.
  -> Contract v
  -- ^ The contract being instantiated.
  -> m (Either InitError ((Datum v, TxOutAssets, Maybe RolesPolicyId), TxConstraints era v))
buildInitConstraints mkRoleTokenMintingPolicy era version walletCtx roles metadata minAda accounts adjustMinUtxo contract = case version of
  MarloweV1 ->
    runTxConstraintsBuilder version $
      buildInitConstraintsV1
        mkRoleTokenMintingPolicy
        era
        walletCtx
        roles
        metadata
        minAda
        accounts
        adjustMinUtxo
        contract

type MkRoleTokenMintingPolicy m = MintingSeed -> Map TokenName Integer -> m CS.PlutusScript

newtype ThreadTokenAssetId = ThreadTokenAssetId {unThreadTokenAssetId :: AssetId}
  deriving (Eq, Show)

-- | Inits a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildInitConstraintsV1
  :: forall era m
   . (Monad m)
  => MkRoleTokenMintingPolicy m
  -- ^ A validator creator for the role token minting policy.
  -> BabbageEraOnwards era
  -- ^ The era in which the transaction is being built. Requires reference scripts.
  -> WalletContext
  -- ^ The wallet used to mint tokens.
  -> RoleTokensConfig
  -- ^ The initial distribution of the role tokens.
  -> MarloweTransactionMetadata
  -- ^ Metadata to add to the transaction.
  -> Lovelace
  -- ^ The lower bound on the ada in the initial state.
  -> Accounts
  -- ^ User-defined initial account balances
  -> AdjustMinUTxO
  -- ^ Adjust a value to account for the minimum UTxO ledger rule.
  -> Contract 'V1
  -- ^ The contract being instantiated.
  -> TxConstraintsBuilderM InitError era 'V1 m (Datum 'V1, TxOutAssets, Maybe RolesPolicyId)
buildInitConstraintsV1 mkRoleTokenMintingPolicy era walletCtx roles metadata minAda accounts adjustMinUtxo contract = do
  -- Minting: thread token and role tokens.
  mintingSeed <- getMintingSeed
  tell $ mustMintThreadToken mintingSeed
  possibleRoleCurrency <- buildRoleTokenConstraints

  -- Metadata
  tell . requiresMetadata $ metadata{transactionMetadata = nftsMetadata possibleRoleCurrency <> transactionMetadata metadata}

  -- Marlowe output.
  (datum, assets) <- sendMarloweOutput possibleRoleCurrency

  pure (datum, assets, possibleRoleCurrency)
  where
    getMintingSeed :: TxConstraintsBuilderM InitError era 'V1 m MintingSeed
    getMintingSeed = lift . except $ do
      let
        WalletContext{availableUtxos} = walletCtx
        utxoLovelace (UTxO{transactionOutput = TransactionOutput{assets=CS.TxOutAssets (Assets lovelace _)}}) = lovelace
        possibleInput = listToMaybe . sortBy (compare `on` utxoLovelace) . toUTxOsList $ availableUtxos
      case possibleInput of
        Nothing -> Left $ InitBuildupFailed MintingUtxoSelectionFailed
        Just UTxO{txOutRef} -> pure $ MintingSeed txOutRef

    nftsMetadata Nothing = mempty
    nftsMetadata (Just (RolesPolicyId (PolicyId policyId))) = case roles of
      RoleTokensMint (unMint -> minting) -> do
        let tokensMetadata = flip NEMap.foldMapWithKey minting \tokenName MintRole{..} ->
              flip foldMap roleMetadata \roleTokenMetadata -> do
                let tokenName' = unTokenName tokenName
                -- From CIP-25: In version 2 the the raw bytes of the asset_name are used.
                pure (MetadataBytes tokenName', encodeRoleTokenMetadata roleTokenMetadata)
        case tokensMetadata of
          [] -> mempty
          metadata' -> TransactionMetadata (Map.singleton 721 (MetadataMap [(MetadataBytes policyId, MetadataMap metadata')]))
      _ -> mempty

    liftMaybe err = lift . except . note (InitBuildupFailed err)

    sendMarloweOutput policyId = do
      datum@(V1.MarloweData _ marloweState _) <- do
        let WalletContext{changeAddress} = walletCtx
            minAdaProvider = MinAdaProvider changeAddress
        lift $
          except $
            first invalidAddressesError $
              initialMarloweDatumV1 contract policyId adjustMinUtxo accounts minAda minAdaProvider
      assets <- liftMaybe InvalidInitialState $ totalStateBalance marloweState
      tell $ mustSendMarloweOutput assets datum
      pure (datum, assets)

    -- Role token distribution constraints
    -- Thread token information is provided so the open roles script
    -- can identify the Marlowe thread correctly when releasing the token.
    buildRoleTokenConstraints :: TxConstraintsBuilderM InitError era 'V1 m (Maybe RolesPolicyId)
    buildRoleTokenConstraints = do
      let
        distributeRoleTokens policyId mustDistribute distribution =
          for_ (Map.toList distribution) \(tokenName, dist') ->
            for_ (Map.toList dist') \(destination, quantity) -> do
              let destination' = case destination of
                    ToScript OpenRoleScript -> Right OpenRoleScript
                    ToAddress addr -> Left addr
              tell $ mustDistribute (AssetId policyId tokenName) destination' quantity
      case roles of
        RoleTokensUsePolicy policyId distribution -> do
          distributeRoleTokens policyId mustDistributeRoleToken distribution
          pure $ Just (RolesPolicyId policyId)
        RoleTokensMint mint -> do
          mintingSeed <- getMintingSeed
          let
            tokenQuantities =
              NEMap.toMap
                $ getTokenQuantities mint
          plutusScript <- lift $ lift $ mkRoleTokenMintingPolicy mintingSeed $ unQuantity <$> tokenQuantities

          (script, scriptHash) <- liftMaybe (MintingScriptDecodingFailed plutusScript) do
            script <- toCardanoPlutusScript plutusScript
            scriptHash <- plutusScriptHash plutusScript
            pure (script, scriptHash)
          let
            plutusScriptV2InEra :: C.ScriptLanguageInEra C.PlutusScriptV2 era
            plutusScriptV2InEra = case era of
              BabbageEraOnwardsBabbage -> C.PlutusScriptV2InBabbage
              BabbageEraOnwardsConway -> C.PlutusScriptV2InConway
              BabbageEraOnwardsDijkstra -> unsafeCoerce C.PlutusScriptV2InConway
            witness =
              C.PlutusScriptWitness
                plutusScriptV2InEra
                C.PlutusScriptV2
                (C.PScript script)
                C.NoScriptDatumForMint
                (C.unsafeHashableScriptData $ C.ScriptDataConstructor 0 []) -- This corresponds to the Mint action in the validator.
                (C.ExecutionUnits 0 0)
            policyId = scriptHashToPolicyId scriptHash
            mint' :: Map TokenName (Map Destination Quantity)
            mint' = NEMap.toMap (unMint mint) <&> \(MintRole {roleTokenRecipients}) ->
              NEMap.toMap roleTokenRecipients
          distributeRoleTokens
            policyId
            (mustMintRoleToken mintingSeed witness)  -- <&> (\mintRole -> mintRole.roleTokensRecipients)
            mint'
          pure $ Just (RolesPolicyId policyId) -- , ThreadTokenAssetId . AssetId policyId <$> threadTokenName')
        RoleTokensNone -> do
          pure Nothing

toMarloweNetwork :: Address -> Maybe V1.Network
toMarloweNetwork =
  toCardanoAddressAny >=> \case
    C.AddressByron (C.ByronAddress _) -> Nothing
    C.AddressShelley (C.ShelleyAddress CL.Mainnet _ _) -> Just V1.mainnet
    C.AddressShelley _ -> Just V1.testnet

safeLovelace :: Lovelace
safeLovelace = Lovelace 750_000 -- Enough lovelace to avoid the Cardano.Api minimum-UTxO bug.

newtype InvalidAddresses = InvalidAddresses [Address]
deriving newtype instance Semigroup InvalidAddresses
deriving newtype instance Monoid InvalidAddresses

invalidAddressesError :: InvalidAddresses -> InitError
invalidAddressesError (InvalidAddresses addresses) =
  InitBuildupFailed $ AddressesDecodingFailed addresses

newtype MinAdaProvider = MinAdaProvider Address
  deriving (Eq, Show)

newtype RolesPolicyId = RolesPolicyId PolicyId
  deriving (Eq, Show)

toMarloweParty :: Account -> Either InvalidAddresses V1.Party
toMarloweParty (AddressAccount address) = note (InvalidAddresses [address]) do
  address' <- toPlutusAddress address
  network <- toMarloweNetwork address
  pure $ V1.Address network address'
toMarloweParty (RoleAccount role) =
  pure $ V1.Role $ toPlutusTokenName role

type MarloweAccounts = AM.Map (AccountId, Token) Integer

toMarloweAccounts :: Accounts -> Either InvalidAddresses MarloweAccounts
toMarloweAccounts (Accounts accounts) = do
  let adaToken = V1.Token PV2.adaSymbol PV2.adaToken
  AM.unsafeFromList . join <$> for (Map.toAscList accounts) \(account, CS.TxOutAssets (Assets{..})) ->
    toMarloweParty account <&> \accountId ->
      Map.toAscList
        . (if ada > mempty then Map.insert (accountId, adaToken) $ unLovelace ada else id)
        . Map.mapKeys (\(AssetId cs tn) -> (accountId, V1.Token (toPlutusCurrencySymbol cs) (toPlutusTokenName tn)))
        . fmap unQuantity
        $ unTokens tokens

initialMarloweDatum
  :: Contract v
  -> Maybe RolesPolicyId
  -> AdjustMinUTxO
  -> Accounts
  -> MarloweVersion v
  -> Lovelace
  -> MinAdaProvider
  -> Either InvalidAddresses (Datum v)
initialMarloweDatum contract policyId adjustMinUtxo accounts version minAda minAdaProvider = case version of
  MarloweV1 -> initialMarloweDatumV1 contract policyId adjustMinUtxo accounts minAda minAdaProvider

initialMarloweDatumV1
  :: V1.Contract
  -> Maybe RolesPolicyId
  -> AdjustMinUTxO
  -> Accounts
  -> Lovelace
  -> MinAdaProvider
  -> Either InvalidAddresses (Datum 'V1)
initialMarloweDatumV1 contract possiblePolicyId adjustMinUtxo accounts minAda minAdaProvider = do
  state <- initialMarloweStateV1 adjustMinUtxo accounts minAda minAdaProvider
  let
    policyIdBytes = case possiblePolicyId of
      Just (RolesPolicyId policyId) -> policyId
      Nothing -> ""
    marloweParams = V1.MarloweParams . toPlutusCurrencySymbol $ policyIdBytes
  pure $ V1.MarloweData marloweParams state contract

initialMarloweState
  :: forall v
   . AdjustMinUTxO
  -> MarloweVersion v
  -> Accounts
  -> Lovelace
  -> MinAdaProvider
  -> Either InvalidAddresses V1.State
initialMarloweState adjustMinUtxo version accounts minAda minAdaProvider = case version of
  MarloweV1 -> do
    initialMarloweStateV1 adjustMinUtxo accounts minAda minAdaProvider

initialMarloweStateV1
  :: AdjustMinUTxO
  -> Accounts
  -> Lovelace
  -> MinAdaProvider
  -> Either InvalidAddresses V1.State
initialMarloweStateV1 (AdjustMinUTxO adjustMinUtxo) accounts@(Accounts accountsMap) minAda (MinAdaProvider creatorAddress) = do
  let CS.TxOutAssets (CS.Assets _ accountsTokens) = fold accountsMap
      fakeThreadToken = do
        let
          policyId = BS.pack $ replicate 28 0
          tokenName = BS.pack $ replicate 32 0
        AssetId (PolicyId policyId) (TokenName tokenName)
      allTokens = accountsTokens <> do
        CS.Tokens $ Map.singleton fakeThreadToken (Quantity 1)
      -- The result of computing the min ada upper bound starting with the initial assets
      CS.TxOutAssets (Assets minAda' _) =
        adjustMinUtxo
          . unsafeTxOutAssets
          . Assets (max safeLovelace minAda)
          $ allTokens

      -- FIXME: we should rise here an exception probably
      creatorAccounts = fold do
        creatorAssets <- mkTxOutAssets $ Assets minAda' mempty
        hush $ mkAccounts (Map.singleton (AddressAccount creatorAddress) creatorAssets)

      -- Merge these with the user-defined initial account balances.
      initialAccounts = creatorAccounts <> accounts
  -- Convert it to the format required by the Plutus script datum.
  initialMarloweAccounts <- toMarloweAccounts initialAccounts
  pure $ (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = initialMarloweAccounts}

marloweAccountsAssets :: V1.Accounts -> Maybe TxOutAssets
marloweAccountsAssets accounts = mkTxOutAssets $ fromPlutusValue $ V1.totalBalance accounts

totalStateBalance :: V1.State -> Maybe TxOutAssets
totalStateBalance V1.State{accounts} = marloweAccountsAssets accounts

type ApplyResults v = (UTCTime, UTCTime, Maybe (TxOutAssets, Datum v), Inputs v)

-- applies an input to a contract.
buildApplyInputsConstraints
  :: (Monad m)
  => (Contract v -> State v -> TransactionInputs v -> m (Maybe (TransactionInputs v)))
  -> SystemStart
  -> EraHistory
  -- ^ The era history for converting times to slots.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v
  -- ^ The previous script output for the contract
  -> SlotNo
  -- ^ The current tip slot
  -> MarloweTransactionMetadata
  -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime
  -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime
  -- ^ The maximum bound of the validity interval (exclusive).
  -- If not specified, this is computed from the the timeouts
  -- in the contract.
  -> Inputs v
  -- ^ The inputs to apply to the contract.
  -> ExceptT ApplyInputsError m (ApplyResults v, TxConstraints era v)
buildApplyInputsConstraints merkleizeInputs systemStart eraHistory version marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs =
  case version of
    MarloweV1 ->
      buildApplyInputsConstraintsV1
        merkleizeInputs
        systemStart
        eraHistory
        marloweOutput
        tipSlot
        metadata
        invalidBefore
        invalidHereafter
        inputs

-- | Inits a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraintsV1
  :: forall era m
   . (Monad m)
  => (V1.Contract -> V1.State -> TransactionInput -> m (Maybe TransactionInput))
  -> SystemStart
  -> EraHistory
  -- ^ The era history for converting times to slots.
  -> TransactionScriptOutput 'V1
  -- ^ The previous script output for the contract with raw TxOut.
  -> SlotNo
  -> MarloweTransactionMetadata
  -- ^ Metadata to attach to the transaction
  -> Maybe UTCTime
  -- ^ The minimum bound of the validity interval (inclusive).
  -> Maybe UTCTime
  -- ^ The maximum bound of the validity interval (exclusive).
  -> Inputs 'V1
  -- ^ The inputs to apply to the contract.
  -> ExceptT ApplyInputsError m (ApplyResults 'V1, TxConstraints era 'V1)
buildApplyInputsConstraintsV1 merkleizeInputs systemStart eraHistory marloweOutput tipSlot metadata invalidBefore invalidHereafter inputs = runWriterT do
  let TransactionScriptOutput _ _ _ datum = marloweOutput
      V1.MarloweData params state contract = datum
      V1.MarloweParams currencySymbol = params

      requiredParties = Set.fromList $ maybeToList . marloweInputParty <$> inputs
      roleAssetId = toAssetId currencySymbol

      tipSlot' = toCardanoSlotNo tipSlot

  invalidBefore' <- lift $ maybe (pure tipSlot') utcTimeToSlotNo invalidBefore

  lift $
    unless (invalidBefore' <= tipSlot') $
      throwE $
        ValidityLowerBoundTooHigh tipSlot $
          fromCardanoSlotNo invalidBefore'

  invalidHereafter' <- lift $ case invalidHereafter of
    Nothing -> do
      invalidBefore'' <- slotStart invalidBefore' -- Find the start time of the validity range.
      pure case nextMarloweTimeoutAfter invalidBefore'' contract of -- Find the next timeout after the range start, if any.
        Nothing -> maxSafeSlot -- There is no future timeout.
        Just subsequentTimeout -> case utcTimeToSlotNo' subsequentTimeout of -- There is a future timeout.
          Right slot' -> slot' -- The next timeout is before the safe horizon, but note that this might be an empty interval.
          _ -> maxSafeSlot -- The next timeout is beyond the safe horizon.
    Just t -> utcTimeToSlotNo t

  -- Consume UTXOs containing all necessary role tokens and send them back.
  for_ requiredParties $ traverse_ $ \case
    V1.Role role -> tell $ mustSpendRoleToken $ roleAssetId role
    _ -> pure ()

  -- Require signature of an every party which is authorized through an address.
  for_ requiredParties $ traverse_ $ \case
    V1.Address _ address -> case address of
      PV2.Address (PV2.PubKeyCredential (PV2.PubKeyHash pkh)) _ ->
        tell $ requiresSignature $ PaymentKeyHash $ PV2.fromBuiltin pkh
      _ -> pure ()
    _ -> pure ()

  -- Require transaction metadata for all specified keys
  tell $ requiresMetadata metadata

  -- Apply inputs.
  let slotNoToPOSIXTime = fmap utcToPOSIXTime . slotStart
  txInterval <-
    lift $
      (,)
        <$> slotNoToPOSIXTime invalidBefore'
        -- We subtract 1 here because invalidHereafter' is the first invalid slot.
        -- However, the Marlowe Semantics time interval upper bound is the last
        -- valid millisecond. So we subtract 1 millisecond from the start of the
        -- first invalid slot to get the last millisecond in the last valid slot.
        <*> (subtract 1 <$> slotNoToPOSIXTime invalidHereafter')
  let transactionInput = V1.TransactionInput{txInterval, txInputs = inputs}

  -- Try and auto-merkleize the inputs if possible.
  transactionInput' <- lift $ lift $ fromMaybe transactionInput <$> merkleizeInputs contract state transactionInput

  -- Construct inputs constraints.
  -- Consume UTXOs containing Marlowe script.
  tell $ mustConsumeMarloweOutput @'V1 invalidBefore' invalidHereafter' $ V1.txInputs transactionInput'

  (possibleContinuation, payments) <- case V1.computeTransaction transactionInput' state contract of
    V1.Error err -> lift $ throwE $ ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed err)
    V1.TransactionOutput _ payments _ V1.Close ->
      pure (Nothing, payments)
    V1.TransactionOutput _ payments state' contract' ->
      pure (Just (state', contract'), payments)

  when (isNothing possibleContinuation) $ do
    tell mustBurnThreadToken

  -- Construct outputs constraints.
  -- Require Marlowe output if the contract is not closed.
  output <- for possibleContinuation \(state'@V1.State{accounts}, contract') -> do
    let datum' = V1.MarloweData params state' contract'
    assets <-
      lift . except . note (ApplyInputsConstraintsBuildupFailed InvalidMarloweState) $ marloweAccountsAssets accounts
    tell $ mustSendMarloweOutput assets datum'
    pure (assets, datum')

  -- For every payment require an output either to the role
  -- payout script or directly to the party address.
  for_ payments \(V1.Payment _ payee token quantity) -> do
    let assets = unsafeTxOutAssets $ if token == V1.ada
          then Assets (Lovelace quantity) mempty
          else case token of
            V1.Token cs tn -> do
              let assetId = toAssetId cs tn
                  quantity' = Quantity quantity
              Assets (Lovelace 0) (CS.Tokens $ Map.singleton assetId quantity')
    case payee of
      V1.Party (V1.Address net addr) -> do
        let addr' = Address $ V1.serialiseAddress net addr
        tell $ mustPayToAddress assets addr'
      V1.Party (V1.Role role) ->
        tell $ mustPayToRole assets $ roleAssetId role
      V1.Account _ -> pure ()

  pure
    ( posixTimeToUTCTime $ fst txInterval
    , posixTimeToUTCTime $ snd txInterval + 1 -- Add the millisecond back to convert the upper bound back to an exclusive bound (ledger semantics)
    , output
    , V1.txInputs transactionInput'
    )
  where
    marloweInputContent (V1.NormalInput c) = c
    marloweInputContent (V1.MerkleizedInput c _ _) = c

    marloweInputParty =
      marloweInputContent >>> \case
        V1.IDeposit _ party _ _ -> Just party
        V1.IChoice (V1.ChoiceId _ party) _ -> Just party
        V1.INotify -> Nothing

    EraHistory interpreter = eraHistory

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo :: UTCTime -> ExceptT ApplyInputsError m O.SlotNo
    utcTimeToSlotNo = withExceptT (SlotConversionFailed . show) . except . utcTimeToSlotNo'

    -- Calculate slot number which contains a given timestamp
    utcTimeToSlotNo' :: UTCTime -> Either PastHorizonException O.SlotNo
    utcTimeToSlotNo' time = do
      let relativeTime = toRelativeTime systemStart time
      (slotNo, _, _) <-
        interpretQuery interpreter $
          wallclockToSlot relativeTime
      pure $ O.SlotNo $ O.unSlotNo slotNo

    slotStart :: O.SlotNo -> ExceptT ApplyInputsError m UTCTime
    slotStart (O.SlotNo slotNo) = do
      (relativeTime, _) <-
        except $
          first (SlotConversionFailed . show) $
            interpretQuery interpreter $
              slotToWallclock $
                O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

    utcToPOSIXTime :: UTCTime -> PV2.POSIXTime
    utcToPOSIXTime = PV2.POSIXTime . floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

    posixTimeToUTCTime :: PV2.POSIXTime -> UTCTime
    posixTimeToUTCTime (PV2.POSIXTime t) = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger t / 1000

    nextMarloweTimeoutAfter :: UTCTime -> V1.Contract -> Maybe UTCTime
    nextMarloweTimeoutAfter limit (V1.When _ timeout c)
      | posixTimeToUTCTime timeout > limit = Just $ posixTimeToUTCTime timeout
      | otherwise = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter _ V1.Close = Nothing
    nextMarloweTimeoutAfter limit (V1.Pay _ _ _ _ c) = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter limit (V1.If _ c1 c2) = on min (nextMarloweTimeoutAfter limit) c1 c2
    nextMarloweTimeoutAfter limit (V1.Let _ _ c) = nextMarloweTimeoutAfter limit c
    nextMarloweTimeoutAfter limit (V1.Assert _ c) = nextMarloweTimeoutAfter limit c

    maxSafeSlot :: O.SlotNo
    maxSafeSlot = getMaxSafeSlotFromSummary $ unInterpreter interpreter

    getMaxSafeSlotFromSummary :: Summary xs -> O.SlotNo
    getMaxSafeSlotFromSummary (Summary eras) = case eras of
      NonEmptyOne era -> getMaxSafeSlotFromEraSummary era
      NonEmptyCons _ eras' -> getMaxSafeSlotFromSummary (Summary eras')

    getMaxSafeSlotFromEraSummary :: EraSummary -> O.SlotNo
    getMaxSafeSlotFromEraSummary EraSummary{..} = case eraEnd of
      EraEnd Bound{..} -> O.SlotNo $ O.unSlotNo boundSlot - 1 -- subtract 1 because the era end bound is exclusive
      EraUnbounded -> maxBound

-- | Inits a set of Tx constraints that are used to build a transaction that
-- withdraws payments from a payout validator.
buildWithdrawConstraints
  :: forall m era v
   . (Monad m)
  => TxConstraints.PayoutContext
  -- ^ The payout context for the current transaction.
  -> MarloweVersion v
  -- ^ The Marlowe version to build the transaction for.
  -> Set TxOutRef
  -- ^ The payouts to withdraw
  -> ExceptT WithdrawError m (Map TxOutRef (Payout v), TxConstraints era v)
buildWithdrawConstraints TxConstraints.PayoutContext{..} = \case
  MarloweV1 -> buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1
      :: Set TxOutRef -> ExceptT WithdrawError m (Map TxOutRef (Payout 'V1), TxConstraints era 'V1)
    buildWithdrawConstraintsV1 payouts = runWriterT do
      when (Set.null payouts) $ lift $ throwE EmptyPayouts
      let payoutsList = Set.toAscList payouts
      traverse_ (tell . TxConstraints.mustConsumePayout) payoutsList
      Map.fromDistinctAscList <$> for payoutsList \payoutRef -> do
        let notFoundError = WithdrawConstraintError $ TxConstraints.PayoutNotFound payoutRef
        TransactionOutput{..} <- lift $ except $ note notFoundError $ Map.lookup payoutRef payoutOutputs
        let invalidError = WithdrawConstraintError $ TxConstraints.InvalidPayoutDatum payoutRef datum
        roleToken <- lift $ except $ note invalidError $ fromChainPayoutDatum MarloweV1 =<< datum
        tell $ mustSpendRoleToken roleToken
        pure (payoutRef, Payout address assets roleToken)
