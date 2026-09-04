module Marlowe.Indexer.MarloweChainFollower.Extractor where

import qualified Cardano.Api as C
import qualified Data.Text.Encoding as T
import Control.Applicative (empty)
import Control.Monad (guard, mfilter, unless, join)
import Control.Monad.Except (MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer (WriterT, execWriterT, Writer)
import Control.Monad.Writer.Class (MonadWriter, listens, tell)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api
    ( BlockHeader(..),
      Credential(..),
      ScriptHash,
      Transaction(..),
      TransactionOutput(..),
      TxIx(TxIx),
      TxOutRef(..),
      paymentCredential
    )
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.History.Api (
  ExtractCreationError (NotCreationTransaction),
  ExtractMarloweTransactionError (MultipleContractInputs),
  MarloweApplyInputsTransaction (..),
  MarloweCreateTransaction (..),
  MarloweWithdrawTransaction (..),
  UnspentContractOutput (..),
  createStepToUnspentContractOutput,
  extractCreation,
  extractMarloweTransaction,
 )
import Witherable (Witherable, wither)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweUTxO (..), MarloweBlock (..), MarloweTransaction (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, toJSON)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson.Encode.Pretty (encodePretty)
import Debug.Trace (traceM)

type ExtractM = WriterT [MarloweTransaction] (StateT MarloweUTxO (Writer [Text]))

logMsg :: Text -> ExtractM ()
logMsg = lift . tell . pure

logJson :: ToJSON a => a -> ExtractM ()
logJson = logMsg . T.decodeUtf8 . BSL.toStrict . encodePretty . toJSON

-- Extracts a MarloweBlock from Cardano Block information. Returns an updated MarloweUTxO.
extractMarloweBlock
  :: C.SystemStart
  -> C.EraHistory
  -> Set ScriptHash
  -- ^ All known Marlowe script hashes.
  -> BlockHeader
  -- ^ The BlockHeader of the block.
  -> Set Transaction
  -- ^ The current MarloweUTxO
  -> StateT MarloweUTxO (Writer [Text]) (Maybe MarloweBlock)
extractMarloweBlock systemStart eraHistory marloweScriptHashes blockHeader txs = do
  transactions <- execWriterT $ do
    let
      BlockHeader{blockNo} = blockHeader
    logMsg $ "Processing block: " <> T.pack (show blockNo)
    retrySilentUntilAllSilent (Set.toList txs) \tx -> do
      logMsg "Extracting transaction: "
      logJson tx
      extractCreateTx marloweScriptHashes tx
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

-- | Extracts a MarloweCreateTransaction from a Chain transaction. A single
-- transaction can create multiple Marlowe contracts, and this function returns
-- a map of outputs that it failed to extract as well as the map of contracts
-- it successfully extracted.
extractCreateTx
  :: Set ScriptHash
  -- ^ All known Marlowe script hashes.
  -> Transaction
  -> ExtractM ()
extractCreateTx marloweScriptHashes Transaction{..} = do
  -- Creation transactions cannot consume outputs from other Marlowe contracts.
  let -- Find all outputs that create a new Marlowe contract
    contractIds =
      mapMaybe (uncurry $ extractContractId marloweScriptHashes) $
        zip (TxOutRef txId . TxIx <$> [0 ..]) outputs
  logMsg $ "Found " <> T.pack (show (length contractIds))
  logJson contractIds
  existingContracts <- gets $ Map.keysSet . unspentContractOutputs

  -- Try to extract a creation step for each prospective contract ID, reporting
  -- any errors found.
  newContracts <-
    Map.fromList <$> flip wither contractIds \contractId ->
      if Set.member contractId existingContracts
        then do
          tell [InvalidCreateTransaction contractId NotCreationTransaction]
          pure Nothing
        else case extractCreation contractId Transaction{..} of
          Left err -> do
            tell [InvalidCreateTransaction contractId err]
            pure Nothing
          Right creationStep -> pure $ Just (case Core.unContractId contractId of TxOutRef{txIx} -> txIx, creationStep)

  -- Prevent the creation of empty create transactions.
  unless (null newContracts) do
    -- Add the new contract outputs to the MarloweUTxO
    let newUnspentContractOutputs = Map.mapKeys (Core.ContractId . TxOutRef txId) $ createStepToUnspentContractOutput <$> newContracts
    modify \utxo -> utxo{unspentContractOutputs = unspentContractOutputs utxo <> newUnspentContractOutputs}

    tell [CreateTransaction MarloweCreateTransaction{..}]
  where

    --   , outputs :: [TransactionOutput]
    -- -- | An output of a transaction.
    -- data TransactionOutput = TransactionOutput
    --   { address :: Address
    --   -- ^ The address that receives the assets of this output.
    --   , assets :: TxOutAssets
    --   -- ^ The assets this output produces.
    --   , datumHash :: Maybe DatumHash
    --   -- ^ FIXME: I'm guessing - The hash of the script non-inlined datum associated with this output.
    --   , datum :: Maybe Datum
    --   -- ^ FIXME: I'm guessing - The script inlined-datum associated with this output.
    --   }
    --   deriving stock (Show, Eq, Ord, Generic)
    --   deriving anyclass (Binary, ToJSON, Variations)
    --
    --   , mintedTokens :: Tokens
    -- -- | A collection of token quantities by their asset ID.
    -- newtype Tokens = Tokens {unTokens :: Map AssetId Quantity}
    --
    -- newtype TxOutAssets = TxOutAssets {unTxOutAssets :: Assets}
    -- data Assets = Assets
    --   { ada :: Lovelace
    --   -- ^ The ADA sent by the tx output.
    --   , tokens :: Tokens
    --   -- ^ Additional tokens sent by the tx output.
    --   }
    --   deriving stock (Show, Eq, Generic)
    --   deriving anyclass (Binary, ToJSON, Variations)
    -- isNewMarloweOutput
    --   :: Chain.Tokens
    --   -> Chain.TransactionOutput
    --   -> Bool
    -- isNewMarloweOutput txMintedTokens output = isJust $ extractThreadTokenPolicyId txMintedTokens output

-- | Extracts a ContractId from a transaction output if it is a Marlowe contract output.
extractContractId
  :: Set ScriptHash
  -- ^ All known Marlowe script hashes.
  -> TxOutRef
  -- ^ The txOutRef of the transaction output.
  -> TransactionOutput
  -- ^ The transaction output.
  -> Maybe ContractId
extractContractId marloweScriptHashes txOutRef TransactionOutput{..} = do
  -- Extract the payment credential from the address.
  credential <- paymentCredential address

  -- The output is a Marlowe output if the credential is a script credential whose script hash is one of the known Marlowe script hashes.
  case credential of
    ScriptCredential hash -> ContractId txOutRef <$ guard (Set.member hash marloweScriptHashes)
    _ -> Nothing

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
  mTransaction <- runMaybeT $ runExceptT $ withExceptT (txId',) do
    -- Get the unspentContractOutputs from  the MarloweUTxO
    contractUTxO <- gets unspentContractOutputs
    -- Find an unspent contract output that the transaction spends.
    (contractId, marloweInput@UnspentContractOutput{..}) <- do
      let
        inputs' = Set.fromList . Map.keys $ inputs
        matchingMarloweInputs = filter (flip Set.member inputs' . txOutRef . snd) $ Map.toList contractUTxO
      let contractIds = Set.fromList $ fst <$> matchingMarloweInputs
      -- Update the MarloweUTxO to remove the unspent contract outputs.
      modify \utxo -> utxo{unspentContractOutputs = Map.withoutKeys (unspentContractOutputs utxo) contractIds}
      case matchingMarloweInputs of
        [] -> do
          lift . lift $ do
            logMsg "No matching Marlowe inputs found for transaction"
            logJson contractUTxO
          lift empty
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
