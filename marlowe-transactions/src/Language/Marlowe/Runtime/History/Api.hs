module Language.Marlowe.Runtime.History.Api where

import Cardano.Api (EraHistory (EraHistory))
import Control.Error (listToMaybe, note, runMaybeT)
import Control.Error.Util (hoistMaybe)
import Control.Monad (guard, join, unless)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Bifunctor (first)
import Data.Binary (Binary, get, put)
import Data.Foldable (find)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Traversable (for)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import qualified Marlowe.Plutus.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  BlockHeader,
  ScriptHash,
  TxError,
  TxId,
  TxIx,
  TxOutRef (..),
  UTxOError, paymentCredential,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api hiding (marloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (getMarloweVersion, MarloweScriptHashes (..), ScriptRegistry)
import qualified Marlowe.Plutus.Scripts.Types as V1
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, slotToWallclock)
import qualified Ouroboros.Network.Block as O
import qualified PlutusLedgerApi.V2 as PV2
import Data.Variations (Variations (..), varyAp)
import qualified Data.Set as Set

data ContractHistoryError
  = HansdshakeFailed
  | FindTxFailed TxError
  | ExtractContractFailed ExtractCreationError
  | FollowScriptUTxOFailed UTxOError
  | FollowPayoutUTxOsFailed (Map Chain.TxOutRef UTxOError)
  | ExtractMarloweTransactionFailed ExtractMarloweTransactionError
  | PayoutUTxONotFound Chain.TxOutRef
  | CreateTxRolledBack
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoInitDatum
  | InvalidInitDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | NoTransactionDatum
  | InvalidTransactionDatum
  | NoPayoutDatum TxOutRef
  | InvalidPayoutDatum TxOutRef
  | InvalidValidityRange
  | SlotConversionFailed
  | MultipleContractInputs (Set TxOutRef)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data CreateStep v = CreateStep
  { createOutput :: TransactionScriptOutput v
  , metadata :: MarloweTransactionMetadata
  , payoutValidatorHash :: ScriptHash
  }
  deriving (Generic)

deriving instance Show (CreateStep 'V1)
deriving instance Eq (CreateStep 'V1)
instance ToJSON (CreateStep 'V1)
instance Variations (CreateStep 'V1)

data SomeCreateStep = forall v. SomeCreateStep (MarloweVersion v) (CreateStep v)

data MarloweCreateTransaction = MarloweCreateTransaction
  { txId :: TxId
  , newContracts :: Map TxIx SomeCreateStep
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

-- | Information about an unspent contract transaction output.
data UnspentContractOutput = UnspentContractOutput
  { marloweVersion :: SomeMarloweVersion
  -- ^ The version of the contract.
  , txOutRef :: TxOutRef
  -- ^ The unspent output.
  , marloweAddress :: Address
  -- ^ The address of the marlowe validator.
  , payoutValidatorHash :: ScriptHash
  -- ^ The hash of the payout validator.
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary, Variations)

instance ToJSON UnspentContractOutput

data MarloweApplyInputsTransaction = forall v.
  MarloweApplyInputsTransaction
  { marloweVersion :: MarloweVersion v
  , marloweInput :: UnspentContractOutput
  , marloweTransaction :: Transaction v
  }

instance Eq MarloweApplyInputsTransaction where
  MarloweApplyInputsTransaction MarloweV1 inpA txA == MarloweApplyInputsTransaction MarloweV1 inpB txB = txA == txB && inpA == inpB

instance Show MarloweApplyInputsTransaction where
  showsPrec p (MarloweApplyInputsTransaction MarloweV1 inp tx) =
    showParen
      (p >= 11)
      ( showString "MarloweApplyInputsTransaction"
          . showSpace
          . showsPrec 11 MarloweV1
          . showSpace
          . showsPrec 11 inp
          . showSpace
          . showsPrec 11 tx
      )

instance ToJSON MarloweApplyInputsTransaction where
  toJSON (MarloweApplyInputsTransaction MarloweV1 input tx) =
    object
      [ "marloweVersion" .= SomeMarloweVersion MarloweV1
      , "marloweInput" .= input
      , "marloweTransaction" .= tx
      ]

instance Variations MarloweApplyInputsTransaction where
  variations = MarloweApplyInputsTransaction MarloweV1 <$> variations `varyAp` variations

instance Binary MarloweApplyInputsTransaction where
  put (MarloweApplyInputsTransaction MarloweV1 input tx) = do
    put $ SomeMarloweVersion MarloweV1
    put input
    put tx
  get =
    get >>= \case
      SomeMarloweVersion MarloweV1 ->
        MarloweApplyInputsTransaction MarloweV1 <$> get <*> get

data MarloweWithdrawTransaction = MarloweWithdrawTransaction
  { consumedPayouts :: Map ContractId (Set TxOutRef)
  , consumingTx :: TxId
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , createTransactions :: [MarloweCreateTransaction]
  , applyInputsTransactions :: [MarloweApplyInputsTransaction]
  , withdrawTransactions :: [MarloweWithdrawTransaction]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

instance Eq SomeCreateStep where
  SomeCreateStep MarloweV1 a == SomeCreateStep MarloweV1 b = a == b

instance Show SomeCreateStep where
  show (SomeCreateStep MarloweV1 a) = show a

instance ToJSON SomeCreateStep where
  toJSON (SomeCreateStep MarloweV1 createStep) =
    object
      [ "version" .= MarloweV1
      , "createStep" .= createStep
      ]

instance Variations SomeCreateStep where
  variations =
    join $
      NE.fromList
        [ SomeCreateStep MarloweV1 <$> variations
        ]

instance Binary SomeCreateStep where
  put (SomeCreateStep MarloweV1 createStep) = do
    put $ SomeMarloweVersion MarloweV1
    put createStep
  get =
    get >>= \case
      SomeMarloweVersion MarloweV1 ->
        SomeCreateStep MarloweV1 <$> get

instance Binary (CreateStep 'V1) where
  put CreateStep{..} = do
    put createOutput
    put metadata
    put payoutValidatorHash
  get = CreateStep <$> get <*> get <*> get

data RedeemStep v = RedeemStep
  { utxo :: TxOutRef
  , redeemingTx :: TxId
  , datum :: PayoutDatum v
  }
  deriving (Generic)

deriving instance Show (RedeemStep 'V1)
deriving instance Eq (RedeemStep 'V1)
instance ToJSON (RedeemStep 'V1)
instance Variations (RedeemStep 'V1)

instance Binary (RedeemStep 'V1) where
  put RedeemStep{..} = do
    put utxo
    put redeemingTx
    putPayoutDatum MarloweV1 datum
  get = RedeemStep <$> get <*> get <*> getPayoutDatum MarloweV1

data ContractStep v
  = ApplyTransaction (Transaction v)
  | RedeemPayout (RedeemStep v)
  deriving (Generic)

deriving instance Show (ContractStep 'V1)
deriving instance Eq (ContractStep 'V1)
instance Binary (ContractStep 'V1)
instance ToJSON (ContractStep 'V1)
instance Variations (ContractStep 'V1)

data SomeContractStep = forall v. SomeContractStep (MarloweVersion v) (ContractStep v)

instance Eq SomeContractStep where
  SomeContractStep MarloweV1 a == SomeContractStep MarloweV1 b = a == b

instance Show SomeContractStep where
  show (SomeContractStep MarloweV1 contractStep) = show contractStep

instance ToJSON SomeContractStep where
  toJSON (SomeContractStep MarloweV1 contractStep) =
    object
      [ "version" .= MarloweV1
      , "contractStep" .= contractStep
      ]

instance Variations SomeContractStep where
  variations =
    join $
      NE.fromList
        [ SomeContractStep MarloweV1 <$> variations
        ]

instance Binary SomeContractStep where
  put (SomeContractStep MarloweV1 contractStep) = do
    put $ SomeMarloweVersion MarloweV1
    put contractStep
  get =
    get >>= \case
      SomeMarloweVersion MarloweV1 ->
        SomeContractStep MarloweV1 <$> get


extractThreadToken
  :: Chain.PolicyId
  -> [Chain.AssetId]
  -> Maybe Chain.TokenName
extractThreadToken ownPolicyId mintedAssets = do
  case [ tokenName | Chain.AssetId policyId tokenName <- mintedAssets, policyId == ownPolicyId ] of
    [threadTokenName] -> Just threadTokenName
    _ -> Nothing

extractThreadTokenPolicyId
  :: Chain.Tokens
  -> Chain.TransactionOutput
  -> Set Chain.ScriptHash
  -> Maybe Chain.PolicyId
extractThreadTokenPolicyId (Chain.Tokens (Map.keys -> mintedAssets)) Chain.TransactionOutput{address} marloweScriptHashes = do
  (Chain.ScriptCredential scriptHash) <- paymentCredential address
  guard (Set.member scriptHash marloweScriptHashes)
  let ownPolicyId = Chain.scriptHashToPolicyId scriptHash
  threadTokenName <- extractThreadToken ownPolicyId mintedAssets
  let threadTokenAssetId = Chain.AssetId ownPolicyId threadTokenName
  guard (threadTokenAssetId `elem` mintedAssets)
  pure ownPolicyId

extractCreation :: ScriptRegistry -> ContractId -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
extractCreation scriptRegistry contractId tx@Chain.Transaction{metadata = txMetadata} = do
  Chain.TransactionOutput{assets, address = scriptAddress, datum = mdatum} <-
    getOutput (txIx $ unContractId contractId) tx
  marloweScriptHash <- getScriptHash scriptAddress
  (SomeMarloweVersion version, MarloweScriptHashes{..}) <- note InvalidScriptHash $ getMarloweVersion scriptRegistry marloweScriptHash
  let payoutValidatorHash = payoutScript
  -- for_ inputs \Chain.TransactionInput{..} ->
  --   when (isScriptAddress marloweScriptHash address) $ Left NotCreationTransaction
  txDatum <- note NoInitDatum mdatum
  datum <- note InvalidInitDatum $ fromChainDatum version txDatum
  let createOutput = TransactionScriptOutput scriptAddress assets (unContractId contractId) datum
  let metadata = decodeMarloweTransactionMetadataLenient txMetadata
  pure $ SomeCreateStep version CreateStep{..}

getScriptHash :: Chain.Address -> Either ExtractCreationError ScriptHash
getScriptHash address = do
  credential <- note ByronAddress $ Chain.paymentCredential address
  case credential of
    Chain.ScriptCredential scriptHash -> pure scriptHash
    _ -> Left NonScriptAddress

isScriptAddress :: ScriptHash -> Chain.Address -> Bool
isScriptAddress scriptHash address = getScriptHash address == Right scriptHash

getOutput :: Chain.TxIx -> Chain.Transaction -> Either ExtractCreationError Chain.TransactionOutput
getOutput (Chain.TxIx i) Chain.Transaction{..} = go i outputs
  where
    go _ [] = Left TxIxNotFound
    go 0 (x : _) = Right x
    go i' (_ : xs) = go (i' - 1) xs

extractMarloweTransaction
  :: MarloweVersion v
  -> SystemStart
  -> EraHistory
  -> ContractId
  -> Chain.Address
  -> Chain.ScriptHash
  -> (Chain.TxOutRef, Maybe Chain.Redeemer)
  -> BlockHeader
  -> Chain.Transaction
  -> Either ExtractMarloweTransactionError (Transaction v)
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
        V1.MerkleizedTxInput content continuationHash ->
          fmap (V1.MerkleizedInput content continuationHash) $
            note InvalidRedeemer $
              listToMaybe $
                flip mapMaybe outputs \Chain.TransactionOutput{..} -> do
                  guard $ datumHash == Just (Chain.DatumHash $ PV2.fromBuiltin continuationHash)
                  Chain.fromDatum =<< datum
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
    pure $ Payout address assets payoutDatum
  let output = TransactionOutput{..}
  pure
    Transaction
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
    EraHistory interpreter = eraHistory
    slotStartTime (Chain.SlotNo slotNo) = do
      (relativeTime, _) <-
        first (const SlotConversionFailed) $
          interpretQuery interpreter $
            slotToWallclock $
              O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

isToScriptHash :: Chain.ScriptHash -> Chain.TransactionOutput -> Bool
isToScriptHash toScriptHash Chain.TransactionOutput{..} = case Chain.paymentCredential address of
  Just (Chain.ScriptCredential hash) -> hash == toScriptHash
  _ -> False

isToAddress :: Chain.Address -> Chain.TransactionOutput -> Bool
isToAddress toAddress Chain.TransactionOutput{..} = address == toAddress

consumesUTxO :: TxOutRef -> Chain.TxOutRef -> Bool
consumesUTxO TxOutRef{..} Chain.TxOutRef{txId = txInId, txIx = txInIx} =
  txId == txInId && txIx == txInIx

createStepToUnspentContractOutput :: SomeCreateStep -> UnspentContractOutput
createStepToUnspentContractOutput (SomeCreateStep version CreateStep{..}) =
  let TransactionScriptOutput{..} = createOutput
      txOutRef = utxo
      marloweAddress = address
      marloweVersion = SomeMarloweVersion version
   in UnspentContractOutput{..}
