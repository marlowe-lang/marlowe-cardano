module Marlowe.Indexer.MarloweChainFollower.Extractor where

import Cardano.Api qualified as C
import Control.Error (note)
import Control.Monad (guard, mfilter, unless, join, void, forM_ )
import Control.Monad.Except (MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (except, ExceptT (ExceptT))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (WriterT, execWriterT, Writer)
import Control.Monad.Writer.Class (MonadWriter, listens, tell)
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (Bifunctor(bimap, first))
import Data.ByteString.Lazy qualified as BSL
import Data.Either (partitionEithers)
import Data.Foldable (for_, find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, listToMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import Language.Marlowe.Runtime.ChainSync.Api ( BlockHeader(..), Credential(..), Transaction(..), TransactionOutput(..), TxOutRef(..), paymentCredential, AssetId(AssetId), PolicyId(PolicyId), Quantity(Quantity), Tokens(Tokens), TransactionMetadata, TxId, Address, ScriptHash (ScriptHash), TxOutAssets (TxOutAssets), Assets (Assets), TxIx(TxIx))
import Language.Marlowe.Runtime.ChainSync.Api qualified as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId (..), SomeMarloweVersion (SomeMarloweVersion), fromChainDatum, TransactionScriptOutput(TransactionScriptOutput), decodeMarloweTransactionMetadataLenient, MarloweVersion (MarloweV1), fromChainPayoutDatum)
import Language.Marlowe.Runtime.Core.Api qualified as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (ScriptRegistry, ReleaseScriptHashes(ReleaseScriptHashes, payoutScriptHash), getMarloweVersion)
import Language.Marlowe.Runtime.History.Api ( ExtractMarloweTransactionError (..), MarloweApplyInputsTransaction (..), MarloweScriptHashes (..), MarloweWithdrawTransaction (..), UnspentContractOutput (..), createStepToUnspentContractOutput, SomeCreateStep (SomeCreateStep), CreateStep (CreateStep))
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweUTxO (..), MarloweBlock (..), MarloweTransaction (..), ExtractCreationError (..), MarloweCreateTransaction (..), MarloweInvalidCreateTransaction(..))
import Marlowe.Plutus.Scripts.Types qualified as V1
import Marlowe.Plutus.Semantics.Types qualified as V1
import Ouroboros.Consensus.BlockchainTime (fromRelativeTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, slotToWallclock)
import Ouroboros.Network.Block qualified as O
import PlutusLedgerApi.V3 qualified as PV3
import Witherable (Witherable, wither)

type ExtractM = WriterT [MarloweTransaction] (StateT MarloweUTxO (Writer [Text]))

logMsg :: Text -> ExtractM ()
logMsg = lift . tell . pure

logJson :: ToJSON a => a -> ExtractM ()
logJson = logMsg . T.decodeUtf8 . BSL.toStrict . encodePretty . toJSON

-- Extracts a MarloweBlock from Cardano Block information. Returns an updated MarloweUTxO.
extractMarloweBlock
  :: C.SystemStart
  -> C.EraHistory
  -> MarloweScriptHashes
  -- ^ All known Marlowe script hashes.
  -> ScriptRegistry
  -- ^ Script registry.
  -> BlockHeader
  -- ^ The BlockHeader of the block.
  -> [Transaction]
  -- ^ The transactions in the block.
  -> StateT MarloweUTxO (Writer [Text]) (Maybe MarloweBlock)
extractMarloweBlock systemStart eraHistory marloweScriptHashes registry blockHeader txs = do
  transactions <- execWriterT $ do
    let
      BlockHeader{blockNo} = blockHeader
    logMsg $ "Processing block: " <> T.pack (show blockNo)
    -- FIXME: I've left this for a reference but I don't believe we need any retries.
    -- Seems like a ugly work around for some internal bug.
    -- retrySilentUntilAllSilent (Set.toList txs) \tx -> do
    forM_ txs \tx -> do
      extractCreateTx registry marloweScriptHashes tx
      extractApplyInputsTx systemStart eraHistory blockHeader tx
      extractWithdrawTx tx
  pure case transactions of
    [] -> Nothing
    x : xs -> Just MarloweBlock{blockHeader, transactions = x :| xs}

-- Runs the given writer action for each element in a structure. If any actions
-- produce output, the silent elements
-- (the ones that did not produce any output) will be retried
retrySilentUntilAllSilent :: (Witherable t, MonadWriter [w] m) => t a -> (a -> m ()) -> m ()
retrySilentUntilAllSilent as f = do
  (as', allSilent) <- listens null $ flip wither as \a -> do
    (_, isSilent) <- listens null $ f a
    pure $ a <$ guard isSilent
  if allSilent then pure () else retrySilentUntilAllSilent as' f


-- | Extracts a ContractId from a transaction output if it is a Marlowe contract output.
extractContractId
  :: MarloweScriptHashes
  -- ^ All known Marlowe script hashes.
  -> TxOutRef
  -- ^ The txOutRef of the transaction output.
  -> TransactionOutput
  -- ^ The transaction output.
  -> Maybe ContractId
extractContractId (MarloweScriptHashes marloweScriptHashes) txOutRef TransactionOutput{..} = do
  -- Extract the payment credential from the address.
  credential <- paymentCredential address

  -- The output is a Marlowe output if the credential is a script credential whose script hash is one of the known Marlowe script hashes.
  case credential of
    ScriptCredential hash -> ContractId txOutRef <$ guard (Set.member hash marloweScriptHashes)
    _ -> Nothing

extractMintedThreadTokens
  :: MarloweScriptHashes
  -- ^ All known Marlowe script hashes.
  -> Tokens
  -- ^ The tokens minted by the transaction.
  -> MintedThreadTokens
extractMintedThreadTokens (MarloweScriptHashes marloweScriptHashes) (Tokens mintedTokens) = do
  let
    isThreadToken (AssetId (PolicyId policyIdHash) _, Quantity q) = Set.member (ScriptHash policyIdHash) marloweScriptHashes && q == 1
  MintedThreadTokens . Set.fromList $ fst <$> filter isThreadToken (Map.toList mintedTokens)

extractCreation
  :: ScriptRegistry
  -> TransactionMetadata
  -> TxId
  -> TxIx
  -> TransactionOutput
  -> Either ExtractCreationError SomeCreateStep
extractCreation scriptRegistry txMetadata txId txIx txOut = do
  let
    getScriptHash :: Address -> Either ExtractCreationError ScriptHash
    getScriptHash address = do
      credential <- note ByronAddress $ Chain.paymentCredential address
      case credential of
        ScriptCredential scriptHash -> pure scriptHash
        _ -> throwError NonScriptAddress

  marloweScriptHash <- getScriptHash txOut.address
  (SomeMarloweVersion version, ReleaseScriptHashes{payoutScriptHash}) <- note InvalidScriptHash $ getMarloweVersion scriptRegistry marloweScriptHash

  txDatum <- maybe (throwError NoInitDatum) pure txOut.datum
  marloweDatum <- note InvalidInitDatum $ fromChainDatum version txDatum

  let
    txOutRef = TxOutRef txId txIx
    createOutput = TransactionScriptOutput txOut.address txOut.assets txOutRef marloweDatum
    metadata = decodeMarloweTransactionMetadataLenient txMetadata
    createStep = CreateStep
      createOutput
      metadata
      payoutScriptHash
  pure $ SomeCreateStep version createStep

newtype MintedThreadTokens = MintedThreadTokens (Set AssetId)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Semigroup, Monoid)

extractThreadToken
  :: MintedThreadTokens
  -> TransactionOutput
  -> Maybe AssetId
extractThreadToken (MintedThreadTokens mintedThreadTokens) txOut = do
  let
    TxOutAssets (Assets _ (Tokens (Map.keys -> assetsIds))) = txOut.assets
  find (`Set.member` mintedThreadTokens) assetsIds

-- | Extracts a MarloweCreateTransaction from a Chain transaction. A single
-- transaction can create multiple Marlowe contracts, and this function returns
-- a map of outputs that it failed to extract as well as the map of contracts
-- it successfully extracted.
extractCreateTx
  :: ScriptRegistry
  -> MarloweScriptHashes
  -- ^ All known Marlowe spending validator script hashes.
  -> Transaction
  -> ExtractM ()
extractCreateTx registry marloweScriptHashes Transaction{..} = do
  let
    mintedThreadTokens = extractMintedThreadTokens marloweScriptHashes mintedTokens
    extracted :: [Either (TxIx, ExtractCreationError) (TxIx, SomeCreateStep)]
    extracted = flip mapMaybe (zip [0 ..] outputs) \(txIxRaw, output) -> do
      let
        txIx = TxIx txIxRaw
      -- Guard for the Marlowe output
      void $ extractThreadToken mintedThreadTokens output
      pure $ bimap (txIx,) (txIx,) $ extractCreation registry metadata txId txIx output
    (failed, successful) = partitionEithers extracted

  unless (null successful) do
    -- Add the new contract outputs to the MarloweUTxO
    let newUnspentContractOutputs = createStepToUnspentContractOutput <$> Map.fromList (first (Core.ContractId . TxOutRef txId) <$> successful)
    modify \utxo -> utxo{unspentContractOutputs = unspentContractOutputs utxo <> newUnspentContractOutputs}
    tell [CreateTransaction MarloweCreateTransaction{txId, newContracts = Map.fromList successful}]

  unless (null failed) do
    let errors = Map.fromList failed
    tell [InvalidCreateTransaction MarloweInvalidCreateTransaction{txId, errors}]

isToScriptHash :: Chain.ScriptHash -> Chain.TransactionOutput -> Bool
isToScriptHash toScriptHash Chain.TransactionOutput{..} = case Chain.paymentCredential address of
  Just (Chain.ScriptCredential hash) -> hash == toScriptHash
  _ -> False

isToAddress :: Chain.Address -> Chain.TransactionOutput -> Bool
isToAddress toAddress Chain.TransactionOutput{..} = address == toAddress

extractMarloweTransaction
  :: MarloweVersion v
  -> C.SystemStart
  -> C.EraHistory
  -> ContractId
  -> Chain.Address
  -> Chain.ScriptHash
  -> (Chain.TxOutRef, Maybe Chain.Redeemer)
  -> BlockHeader
  -> Transaction
  -> Either ExtractMarloweTransactionError (Core.Transaction v)
extractMarloweTransaction version systemStart eraHistory contractId scriptAddress payoutValidatorHash (consumedTxOutRef, possibleRedeemer) blockHeader Chain.Transaction{..} = do
  let
    transactionId = txId
  unless (elem consumedTxOutRef . Map.keys $ inputs) $
    Left TxInNotFound

  marloweInputs <- case version of
    MarloweV1 -> do
      redeemer <- do
        rawRedeemer <- note NoRedeemer possibleRedeemer
        note InvalidRedeemer $ Chain.fromRedeemer rawRedeemer
      for redeemer \case
        V1.Input content -> pure $ V1.NormalInput content
        V1.MerkleizedTxInput content continuationHash -> do
          datum <- note MissingDatumHash $ listToMaybe $
            flip mapMaybe outputs \Chain.TransactionOutput{..} -> do
              guard $ datumHash == Just (Chain.DatumHash $ PV3.fromBuiltin continuationHash)
              datum
          contract <- note InvalidContinuation $ Chain.fromDatum datum
          pure $ V1.MerkleizedInput content continuationHash contract
  (minSlot, maxSlot) <- case validityRange of
    Chain.MinMaxBound minSlot maxSlot -> pure (minSlot, maxSlot)
    _ -> Left InvalidValidityRange
  validityLowerBound <- slotStartTime minSlot
  validityUpperBound <- slotStartTime maxSlot
  scriptOutput <- runMaybeT do
    (ix, Chain.TransactionOutput{assets, datum = mDatum}) <-
      hoistMaybe $ find (isToAddress scriptAddress . snd) $ zip [0 ..] outputs
    lift do
      rawDatum <- note NoTransactionDatum mDatum
      datum <- note InvalidTransactionDatum $ fromChainDatum version rawDatum
      let txIx = Chain.TxIx ix
      let utxo = Chain.TxOutRef{..}
      let address = scriptAddress
      pure TransactionScriptOutput{..}
  let payoutOutputs =
        Map.filter (isToScriptHash payoutValidatorHash) $
          Map.fromList $
            (\(txIx, output) -> (Chain.TxOutRef{txIx = Chain.TxIx txIx, ..}, output)) <$> zip [0 ..] outputs
  payouts <- flip Map.traverseWithKey payoutOutputs \txOut Chain.TransactionOutput{address, datum = mPayoutDatum, assets} -> do
    rawPayoutDatum <- note (NoPayoutDatum txOut) mPayoutDatum
    payoutDatum <- note (InvalidPayoutDatum txOut) $ fromChainPayoutDatum version rawPayoutDatum
    pure $ Core.Payout address assets payoutDatum
  let output = Core.TransactionOutput{..}
  pure
    Core.Transaction
      { transactionId
      , contractId
      , metadata = decodeMarloweTransactionMetadataLenient metadata
      , blockHeader
      , validityLowerBound
      , validityUpperBound
      , inputs = marloweInputs
      , output
      }
  where
    C.EraHistory interpreter = eraHistory
    slotStartTime (Chain.SlotNo slotNo) = do
      (relativeTime, _) <-
        first (const SlotConversionFailed) $
          interpretQuery interpreter $
            slotToWallclock $
              O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

-- | Extracts an apply inputs transaction from a chain transaction. Returns
-- nothing if the transaction does not apply an input to any unspent contract
-- output.
extractApplyInputsTx
  :: C.SystemStart
  -> C.EraHistory
  -> BlockHeader
  -> Transaction
  -- ^ The transaction to extract an apply inputs tx from.
  -> ExtractM ()
extractApplyInputsTx systemStart eraHistory blockHeader tx@Transaction{inputs, txId = txId'} = do
  -- MaybeT $ ExceptT $ WriterT $ StateT $ WriterT $ Identity
  mTransaction <- runMaybeT $ runExceptT $ withExceptT (txId',) do
    -- Get the unspentContractOutputs from  the MarloweUTxO
    contractsUTxOs <- gets unspentContractOutputs
    -- Find an unspent contract output that the transaction spends.
    (contractId, marloweInput@UnspentContractOutput{..}) <- do
      let
        inputs' = Set.fromList . Map.keys $ inputs
        matchingMarloweInputs = filter (flip Set.member inputs' . txOutRef . snd) $ Map.toList contractsUTxOs
      let contractIds = Set.fromList $ fst <$> matchingMarloweInputs
      -- Update the MarloweUTxO to remove the unspent contract outputs.
      modify \utxo -> utxo{unspentContractOutputs = Map.withoutKeys (unspentContractOutputs utxo) contractIds}
      case matchingMarloweInputs of
        [] -> do
          lift $ lift $ logMsg "No matching Marlowe inputs found for transaction"
          ExceptT $ MaybeT $ pure Nothing
        [x] -> pure x
        _ -> do
          let matchingRefs = Set.fromList $ txOutRef . snd <$> matchingMarloweInputs
          throwError (matchingRefs, MultipleContractInputs matchingRefs)

    -- Extract a Marlowe transaction of the correct version.
    case marloweVersion of
      Core.SomeMarloweVersion v -> do
        let
          possibleRedeemer = join $ Map.lookup txOutRef inputs

        lift . lift $ do
          logMsg "Extracted Redeemer:"
          logJson possibleRedeemer
          logMsg "Inputs provided to Marlowe transaction:"
          logJson inputs
          logMsg "Input txOutRef:"
          logJson txOutRef

        marloweTransaction <-
          withExceptT (Set.singleton txOutRef,) $
            except $
              extractMarloweTransaction
                v
                systemStart
                eraHistory
                contractId
                marloweAddress
                payoutValidatorHash
                (txOutRef, possibleRedeemer)
                blockHeader
                tx

        -- Add new payouts to the unspentPayoutOutputs and update the MarloweUTxO to add the new unspent contract output if one was produced.
        modify \MarloweUTxO{..} ->
          MarloweUTxO
            { unspentPayoutOutputs =
                Map.unionWith (<>) unspentPayoutOutputs $
                  Map.filter (not . Set.null) $
                    Map.singleton contractId $
                      Map.keysSet $
                        Core.payouts $
                          Core.output marloweTransaction
            , unspentContractOutputs = case Core.scriptOutput $ Core.output marloweTransaction of
                Nothing -> unspentContractOutputs
                Just scriptOutput ->
                  let newOutput =
                        UnspentContractOutput
                          { marloweVersion = Core.SomeMarloweVersion v
                          , txOutRef = Core.utxo scriptOutput
                          , marloweAddress
                          , payoutValidatorHash
                          }
                   in Map.insert contractId newOutput unspentContractOutputs
            }

        pure
          MarloweApplyInputsTransaction
            { marloweVersion = v
            , marloweInput
            , marloweTransaction
            }

  for_ mTransaction \case
    Left (txId, (txOutRefs, err)) -> tell [InvalidApplyInputsTransaction txId txOutRefs err]
    Right transaction -> tell [ApplyInputsTransaction transaction]

-- | Extracts a withdraw transaction from a chain transaction. Returns nothing
-- if the transaction does not withdraw contract payouts. Removes payouts from
-- the Marlowe UTxO.
extractWithdrawTx
  :: Transaction
  -> ExtractM ()
extractWithdrawTx Transaction{inputs, txId = consumingTx} = do
  -- Get the unspentPayoutOutputs fro the MarloweUTxO
  unspentPayoutOutputs <- gets unspentPayoutOutputs

  -- Find unspent payouts that the transaction spends.
  let
    inputs' = Set.fromList . Map.keys $ inputs
    consumedPayouts = Map.filter (not . Set.null) $ Set.intersection inputs' <$> unspentPayoutOutputs
  for_ (Map.toList consumedPayouts) \(contractId, payoutsForContract) -> do
    -- Update the MarloweUTxO to remove the unspentPayoutOutputs.
    modify \utxo -> utxo{unspentPayoutOutputs = Map.alter (>>= removePayouts payoutsForContract) contractId unspentPayoutOutputs}

  unless (Map.null consumedPayouts) $ tell [WithdrawTransaction MarloweWithdrawTransaction{..}]
  where
    -- Remove the consumed payouts for the contract and remove the contractId from the
    -- map if there are no more payouts left afterward.
    removePayouts consumedPayouts = mfilter (not . Set.null) . Just . (`Set.difference` consumedPayouts)

hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
