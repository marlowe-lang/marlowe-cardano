module Language.Marlowe.Runtime.History.Api where

import Data.Aeson (ToJSON, object, toJSON, (.=), FromJSON)
import Data.Binary (Binary, get, put)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  ScriptHash,
  TxId,
  TxOutRef (..),
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api hiding (marloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(marloweScript), ScriptDetails(scriptHash), ScriptRegistry, pattern ScriptRegistry)
import Data.Variations (Variations (..), varyAp)
import qualified Data.Set as Set
import Marlowe.Contrib.Foldable (foldMapFlipped)
import qualified Data.List.NonEmpty as NEList
import qualified Data.Map.NonEmpty as NEMap
import Control.Monad (join)

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | MissingDatumHash
  | InvalidContinuation
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

newtype MarloweScriptHashes = MarloweScriptHashes {hashes :: Set ScriptHash}
  deriving stock (Show, Eq, Ord)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, Variations)

getRegistryMarloweScriptHashes :: ScriptRegistry -> MarloweScriptHashes
getRegistryMarloweScriptHashes (ScriptRegistry _ allReleases) = do
  foldMapFlipped (NEList.toList . NEMap.elems $ allReleases) \release -> do
    MarloweScriptHashes $ Set.singleton release.marloweScript.scriptHash

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
