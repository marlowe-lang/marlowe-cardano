module Language.Marlowe.Runtime.Transaction.Constraints (
  ConstraintError (..),
  Distribution (..),
  HelperScriptInfo (..),
  HelperScriptState (..),
  HelpersContext (..),
  MarloweContext (..),
  MarloweInputConstraints (..),
  MarloweOutputConstraints (..),
  MintingSeed (..),
  PayoutContext (..),
  RoleTokenConstraints (RoleTokenConstraintsNone, MintRoleTokens, DistributeRoleTokens, SpendRoleTokens),
  SolveConstraints,
  ThreadTokenConstraints (..),
  TxConstraints (..),
  WalletContext (..),
  adjustTxForMinUtxo,
  balanceTx,
  ensureMinUtxo,
  findMinUtxo,
  mkTxOutValue,
  mustBurnThreadToken,
  mustConsumeMarloweOutput,
  mustConsumePayout,
  mustDistributeRoleToken,
  mustMintRoleToken,
  mustMintThreadToken,
  mustPayToAddress,
  mustPayToRole,
  mustSendMarloweOutput,
  mustSpendRoleToken,
  requiresMetadata,
  requiresSignature,
  selectCoins,
  solveConstraints,
  solveInitialTxBodyContent,
) where

import Cardano.Api (
  SystemStart,
  TxBodyContent (..),
  unsafeHashableScriptData,
  )
import qualified Cardano.Api as C
import GHC.Exts (fromList, toList)
import Cardano.Api.Ledger (ppPricesL)
import qualified Cardano.Api.Ledger as Ledger
import Cardano.Api.Plutus (fromAlonzoExUnits)
import Cardano.Api.ProtocolParameters (fromAlonzoPrices)
import Cardano.Ledger.Alonzo.PParams (
  ppMaxTxExUnitsL,
 )
import Cardano.Ledger.Core (ppMaxTxSizeL, ppTxFeePerByteL, ppTxFeeFixedL)
import Cardano.Ledger.Coin (CoinPerByte(unCoinPerByte), Coin(Coin))
import Cardano.Ledger.Compactible (fromCompact)
import Control.Applicative ((<|>))
import Control.Error (hoistMaybe, note, noteT, runExceptT, ExceptT(ExceptT))
import Control.Monad (forM, unless, when, (<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Aeson (ToJSON (toJSON), (.=))
import qualified Data.Aeson as A
import Data.Foldable (Foldable (fold))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (delete, find, minimumBy, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as SMap (fromList, toList)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Monoid (First (..), getFirst)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Traversable (for)
import GHC.Generics (Generic)
import qualified Marlowe.Plutus.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (
  babbageEraOnwardsToAlonzoEraOnwards,
  babbageEraOnwardsToMaryEraOnwards,
  babbageEraOnwardsToShelleyBasedEra,
  fromCardanoAddressInEra,
  fromCardanoDatumHash,
  fromCardanoTxIn,
  toCardanoAddressInEra,
  toCardanoPaymentKeyHash,
  toCardanoScriptData,
  toCardanoTxIn,
  toCardanoTxOut,
  toCardanoTxOut',
  toCardanoTxOutDatum',
  toCardanoTxOutValue,
  tokensFromCardanoValue,
  tokensToCardanoValue,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  lookupUTxO,
  toCardanoMetadata,
  toUTxOTuple,
  toUTxOsList,
  unsafeTxOutAssets,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (
  MarloweTransactionMetadata (..),
  MarloweVersionTag (..),
  TransactionScriptOutput (assets, utxo),
  emptyMarloweTransactionMetadata,
  encodeMarloweTransactionMetadata,
  fromChainPayoutDatum,
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript(OpenRoleScript), ReferenceScriptUtxo (..), toCardanoScriptInAnyLang, hashScriptInPlutus)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.Transaction.Api (CoinSelectionError (..), ConstraintError (..), Destination (..))
import qualified Marlowe.Plutus.Scripts.Types as V1
import qualified Marlowe.Plutus.Scripts as V1
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V3 as P

-- | Describes a set of Marlowe-specific conditions that a transaction must satisfy.
data TxConstraints era v = TxConstraints
  { marloweInputConstraints :: MarloweInputConstraints v
  , payoutInputConstraints :: Set Chain.TxOutRef
  , roleTokenConstraints :: RoleTokenConstraints era
  , payToAddresses :: Map Chain.Address Chain.TxOutAssets
  , payToRoles :: Map (Core.PayoutDatum v) Chain.TxOutAssets
  , marloweOutputConstraints :: MarloweOutputConstraints v
  , signatureConstraints :: Set Chain.PaymentKeyHash
  , metadataConstraints :: Core.MarloweTransactionMetadata
  , threadTokenConstraints :: ThreadTokenConstraints era
  }

deriving instance Show (TxConstraints era 'V1)
deriving instance Eq (TxConstraints era 'V1)
deriving instance Generic (TxConstraints era 'V1)
deriving instance ToJSON (TxConstraints era 'V1)

newtype MintingSeed = MintingSeed {unMintingSeed :: Chain.TxOutRef}
  deriving (Eq, Show)
deriving newtype instance ToJSON MintingSeed

data ThreadTokenConstraints era
  = BurnThreadToken
  | MintThreadToken MintingSeed
  | PassThreadToken
  deriving (Eq, Show)

instance ToJSON (ThreadTokenConstraints era) where
  toJSON = \case
    BurnThreadToken -> toJSON ("burn" :: String)
    MintThreadToken out ->
      A.object
        [ "mint" .= out
        ]
    PassThreadToken -> toJSON ("carry" :: String)

instance Semigroup (ThreadTokenConstraints era) where
  a <> PassThreadToken = a
  _ <> b = b

mustBurnThreadToken :: (Core.IsMarloweVersion v) => TxConstraints era v
mustBurnThreadToken = mempty{threadTokenConstraints = BurnThreadToken}

mustMintThreadToken :: (Core.IsMarloweVersion v) => MintingSeed -> TxConstraints era v
mustMintThreadToken seed = mempty{threadTokenConstraints = MintThreadToken seed}

-- | Constraints related to role tokens.
data RoleTokenConstraints era
  = RoleTokenConstraintsNone
  | NEMintRoleTokens MintingSeed (C.ScriptWitness C.WitCtxMint era) NEDistribution
  | NEDistributeRoleTokens NEDistribution
  | NESpendRoleTokens (NESet Chain.AssetId)
  deriving (Eq, Show)

instance ToJSON (RoleTokenConstraints era) where
  toJSON = \case
    RoleTokenConstraintsNone -> A.Null
    NEMintRoleTokens out _ dist ->
      A.object
        [ "mint"
            .= A.object
              [ "out" .= out
              , "distribution" .= dist
              ]
        ]
    NEDistributeRoleTokens dist ->
      A.object
        [ "distribute" .= dist
        ]
    NESpendRoleTokens roles ->
      A.object
        [ "spend" .= roles
        ]

{-# COMPLETE RoleTokenConstraintsNone, MintRoleTokens, DistributeRoleTokens, SpendRoleTokens #-}

pattern DistributeRoleTokens :: Distribution -> RoleTokenConstraints era
pattern DistributeRoleTokens dist <- NEDistributeRoleTokens (toDistribution -> dist)
  where
    DistributeRoleTokens dist =
      maybe RoleTokenConstraintsNone NEDistributeRoleTokens $ nonEmptyDistribution dist

pattern MintRoleTokens
  :: MintingSeed
  -> C.ScriptWitness C.WitCtxMint era
  -> Distribution
  -> RoleTokenConstraints era
pattern MintRoleTokens out wit dist <- NEMintRoleTokens out wit (toDistribution -> dist)
  where
    MintRoleTokens out wit dist =
      maybe RoleTokenConstraintsNone (NEMintRoleTokens out wit) $ nonEmptyDistribution dist

pattern SpendRoleTokens :: Set Chain.AssetId -> RoleTokenConstraints era
pattern SpendRoleTokens roles <- NESpendRoleTokens (NESet.toSet -> roles)
  where
    SpendRoleTokens roles =
      maybe RoleTokenConstraintsNone NESpendRoleTokens $ NESet.nonEmptySet roles

instance Semigroup (RoleTokenConstraints era) where
  a <> RoleTokenConstraintsNone = a
  MintRoleTokens _ _ a <> MintRoleTokens ref witness b = MintRoleTokens ref witness $ a <> b
  DistributeRoleTokens a <> DistributeRoleTokens b = DistributeRoleTokens $ a <> b
  SpendRoleTokens a <> SpendRoleTokens b = SpendRoleTokens $ a <> b
  _ <> b = b

instance Monoid (RoleTokenConstraints era) where
  mempty = RoleTokenConstraintsNone

data NEDistribution
  = NESendToAddresses (NEMap Chain.AssetId (NEMap Chain.Address Chain.Quantity))
  | NESendToScripts (NEMap Chain.AssetId (NEMap Destination Chain.Quantity))
  deriving (Eq, Show, Generic, ToJSON)

nonEmptyDistribution :: Distribution -> Maybe NEDistribution
nonEmptyDistribution = \case
  SendToAddresses dist ->
    fmap NESendToAddresses
      . NEMap.nonEmptyMap
      . Map.mapMaybe (NEMap.nonEmptyMap . Map.filter (> mempty))
      $ dist
  SendToScripts dist ->
    fmap NESendToScripts
      . NEMap.nonEmptyMap
      . Map.mapMaybe (NEMap.nonEmptyMap . Map.filter (> mempty))
      $ dist

toDistribution :: NEDistribution -> Distribution
toDistribution = \case
  NESendToAddresses dist -> SendToAddresses $ NEMap.toMap $ NEMap.toMap <$> dist
  NESendToScripts dist -> SendToScripts $ NEMap.toMap $ NEMap.toMap <$> dist

data Distribution
  = SendToAddresses (Map Chain.AssetId (Map Chain.Address Chain.Quantity))
  | SendToScripts (Map Chain.AssetId (Map Destination Chain.Quantity))
  deriving (Eq, Show)

instance Semigroup Distribution where
  SendToAddresses dist <> SendToAddresses dist' =
    SendToAddresses $ Map.unionWith (Map.unionWith (<>)) dist dist'
  SendToScripts dist <> SendToScripts dist' =
    SendToScripts $ Map.unionWith (Map.unionWith (<>)) dist dist'
  SendToScripts dist <> SendToAddresses dist' =
    SendToScripts $ Map.unionWith (Map.unionWith (<>)) dist (Map.mapKeysMonotonic ToAddress <$> dist')
  SendToAddresses dist <> SendToScripts dist' =
    SendToScripts $ Map.unionWith (Map.unionWith (<>)) (Map.mapKeysMonotonic ToAddress <$> dist) dist'

-- | Require the transaction to mint the specified number of role tokens with the
-- specified assetId and send them to the given destinations. Additionally, require that
-- the given UTXO is consumed. Additionally, mint one of the given thread tokens if specified.
--
-- Requires that:
--   1. The transaction mints at least n tokens with the given assetId.
--   2. The transaction sends n tokens with the given assetId to the given destination.
--   3. The output in rule 2 does not contain any other tokens aside from ADA.
--   4. The transaction consumes the given TxOutRef.
--   5. The transaction mints 1 of thread token with the specified name if specified.
mustMintRoleToken
  :: (Core.IsMarloweVersion v)
  => MintingSeed
  -> C.ScriptWitness C.WitCtxMint era
  -> Chain.AssetId
  -> Either Chain.Address HelperScript
  -> Chain.Quantity
  -> TxConstraints era v
mustMintRoleToken seed witness assetId destination quantity =
  mempty
    { roleTokenConstraints =
        MintRoleTokens seed witness case destination of
          Left addr -> SendToAddresses $ Map.singleton assetId $ Map.singleton addr quantity
          Right helperScript -> SendToScripts $ Map.singleton assetId $ Map.singleton (ToScript helperScript) quantity
    }

-- | Require the transaction to send the specified number of role tokens with the
-- specified assetId to the given destinations.
--
-- Requires that:
--   1. The transaction sends n tokens with the given assetId to the given destination.
mustDistributeRoleToken
  :: (Core.IsMarloweVersion v)
  => Chain.AssetId
  -> Either Chain.Address HelperScript
  -> Chain.Quantity
  -> TxConstraints era v
mustDistributeRoleToken assetId destination quantity =
  mempty
    { roleTokenConstraints = DistributeRoleTokens case destination of
        Left addr -> SendToAddresses $ Map.singleton assetId $ Map.singleton addr quantity
        Right script -> SendToScripts $ Map.singleton assetId $ Map.singleton (ToScript script) quantity
    }

-- | Require the transaction to spend a UTXO with 1 role token of the specified
-- assetID. It also needs to send an identical output (same assets) to the
-- address that held the spent UTXO.
--
-- Requires that:
--   1. The transaction consumes a UTXO that contains the necessary role
--      token.
--   2. The transaction produces an output that is identical to the output from
--      rule 1.
mustSpendRoleToken :: (Core.IsMarloweVersion v) => Chain.AssetId -> TxConstraints era v
mustSpendRoleToken assetId = mempty{roleTokenConstraints = SpendRoleTokens $ Set.singleton assetId}

data MarloweOutputConstraints v
  = MarloweOutputConstraintsNone
  | MarloweOutput Chain.TxOutAssets (Core.Datum v)
  -- | ^ This does not include thread token information in the assets by design.
  -- | We do not want to deal with the script hashes etc.

deriving instance Show (MarloweOutputConstraints 'V1)
deriving instance Eq (MarloweOutputConstraints 'V1)
deriving instance Generic (MarloweOutputConstraints 'V1)
deriving instance ToJSON (MarloweOutputConstraints 'V1)

instance Semigroup (MarloweOutputConstraints v) where
  a <> MarloweOutputConstraintsNone = a
  _ <> b = b

instance Monoid (MarloweOutputConstraints v) where
  mempty = MarloweOutputConstraintsNone

-- | Require the transaction to send the specified assets to the address.
--
-- Requires that:
--   postulate:
--     total :: Address -> TxBody era -> TxOutValue era
--     subValue :: TxOutValue era -> TxOutValue era -> TxOutValue era
--   given:
--     constraints :: TxConstraints v
--     assets :: Assets
--     address :: Address
--   define:
--     payConstraint = mustPayToAddress assets address
--     Right txBody = solveConstraints $ constraints <> payConstraint
--     Right txBody' = solveConstraints constraints
--   1. fromCardano (total address txBody `subValue` total address txBody') == assets
mustPayToAddress :: (Core.IsMarloweVersion v) => Chain.TxOutAssets -> Chain.Address -> TxConstraints era v
mustPayToAddress assets address = mempty{payToAddresses = Map.singleton address assets}

-- | Require the transaction to send an output to the marlowe script address
-- with the given assets and the given datum.
--
-- Requires that:
--   1. The transaction sends an output with the given assets and datum to a
--      script address.
--   2. The script address in rule 1 is in the script address set for the
--      corresponding Marlowe version.
mustSendMarloweOutput :: (Core.IsMarloweVersion v) => Chain.TxOutAssets -> Core.Datum v -> TxConstraints era v
mustSendMarloweOutput assets datum =
  mempty{marloweOutputConstraints = MarloweOutput assets datum}

-- | Require the transaction to send an output to the payout script address
-- with the given assets and the given datum.
--
-- Requires that:
--   postulate:
--     total :: PayoutDatum v -> TxBody era -> TxOutValue era
--     subValue :: TxOutValue era -> TxOutValue era -> TxOutValue era
--   given:
--     constraints :: TxConstraints v
--     assets :: Assets
--     role :: PayoutDatum v
--   define:
--     payConstraint = mustPayToRole assets role
--     Right txBody = solveConstraints $ constraints <> payConstraint
--     Right txBody' = solveConstraints constraints
--   1. fromCardano (total role txBody `subValue` total role txBody') == assets
--   2. The transaction sends an output to a script address.
--   3. The datum of the output in rule 2 is equal to the role.
mustPayToRole :: (Core.IsMarloweVersion v) => Chain.TxOutAssets -> Core.PayoutDatum v -> TxConstraints era v
mustPayToRole assets datum =
  mempty{payToRoles = Map.singleton datum assets}

data MarloweInputConstraints v
  = MarloweInputConstraintsNone
  | MarloweInput C.SlotNo C.SlotNo (Core.Inputs v)

deriving instance Show (MarloweInputConstraints 'V1)
deriving instance Eq (MarloweInputConstraints 'V1)
deriving instance Generic (MarloweInputConstraints 'V1)
deriving instance ToJSON (MarloweInputConstraints 'V1)

instance Semigroup (MarloweInputConstraints v) where
  a <> MarloweInputConstraintsNone = a
  _ <> b = b

instance Monoid (MarloweInputConstraints v) where
  mempty = MarloweInputConstraintsNone

-- | Require the transaction to consume the UTXO for the current contract from
-- the Marlowe script with the given validity interval and redeemer (input).
-- Used for apply-inputs.
--
-- Requires that:
--   1. The input at the Marlowe Script Address for the contract is consumed.
--   2. The input in rule 1 includes the given redeemer.
--   3. The validity range of the transaction matches the given min and max
--      validity bounds.
--   4. All other inputs do not come from a script address.
mustConsumeMarloweOutput :: (Core.IsMarloweVersion v) => C.SlotNo -> C.SlotNo -> Core.Inputs v -> TxConstraints era v
mustConsumeMarloweOutput invalidBefore invalidHereafter inputs =
  mempty{marloweInputConstraints = MarloweInput invalidBefore invalidHereafter inputs}

-- | Require the transaction to consume the given payout transaction output.
--
-- Requires that:
--   1. The output is consumed.
--   2. The witness for the output is a script witness
--   3. The script witness specifies a valid payout datum for the correct version
--   4. The script witness specifies a reference script
mustConsumePayout :: (Core.IsMarloweVersion v) => Chain.TxOutRef -> TxConstraints era v
mustConsumePayout output = mempty{payoutInputConstraints = Set.singleton output}

-- | Require the transaction to hold a signature for the given payment key
-- hash.
--
-- Requires that:
--   1. If none of the inputs are from an address withe a matching payment key,
--      then the txExtraKeyWits of the body content contain the given hash.
--   2. If theExtraKeyWits of the body content does not contain the given hash,
--      then at least one input must be from an address with a matching payment
--      key.
requiresSignature :: (Core.IsMarloweVersion v) => Chain.PaymentKeyHash -> TxConstraints era v
requiresSignature pkh = mempty{signatureConstraints = Set.singleton pkh}

-- | Require the transaction include the given metadata.
--
-- Requires that:
--   1. The given metadata map is present in the transaction.
requiresMetadata :: (Core.IsMarloweVersion v) => Core.MarloweTransactionMetadata -> TxConstraints era v
requiresMetadata metadataConstraints = mempty{metadataConstraints}

instance (Core.IsMarloweVersion v) => Semigroup (TxConstraints era v) where
  a <> b = case Core.marloweVersion @v of
    Core.MarloweV1 ->
      TxConstraints
        { marloweInputConstraints = on (<>) marloweInputConstraints a b
        , payoutInputConstraints = on Set.union payoutInputConstraints a b
        , roleTokenConstraints = on (<>) roleTokenConstraints a b
        , payToAddresses = on (Map.unionWith (<>)) payToAddresses a b
        , payToRoles = on (Map.unionWith (<>)) payToRoles a b
        , marloweOutputConstraints = on (<>) marloweOutputConstraints a b
        , signatureConstraints = on (<>) signatureConstraints a b
        , metadataConstraints =
            MarloweTransactionMetadata
              { marloweMetadata = on (<|>) (marloweMetadata . metadataConstraints) a b
              , transactionMetadata = on (<>) (transactionMetadata . metadataConstraints) a b
              }
        , threadTokenConstraints = on (<>) threadTokenConstraints a b
        }

instance (Core.IsMarloweVersion v) => Monoid (TxConstraints era v) where
  mempty = case Core.marloweVersion @v of
    Core.MarloweV1 ->
      TxConstraints
        { marloweInputConstraints = mempty
        , payoutInputConstraints = mempty
        , roleTokenConstraints = mempty
        , payToAddresses = mempty
        , payToRoles = mempty
        , marloweOutputConstraints = mempty
        , signatureConstraints = mempty
        , metadataConstraints = emptyMarloweTransactionMetadata
        , threadTokenConstraints = PassThreadToken
        }

-- | Data from a wallet needed to solve the constraints.
data WalletContext = WalletContext
  { availableUtxos :: Chain.UTxOs
  -- ^ The UTXO set of the wallet that can be used for coin selection and
  -- satisfying constraints.
  , collateralUtxos :: Set Chain.TxOutRef
  -- ^ The subset of keys in 'availableUtxos' that may be used for collateral.
  , changeAddress :: Chain.Address
  -- ^ The change address of the wallet.
  }
  deriving (Show, Generic, ToJSON)

-- | Data from Marlowe Script needed to solve the constraints.
data MarloweContext v = MarloweContext
  { scriptOutput :: Maybe (Core.TransactionScriptOutput v)
  -- ^ The UTXO at the script address, if any.
  , marloweAddress :: Chain.Address
  , payoutAddress :: Chain.Address
  , marloweScriptUTxO :: ReferenceScriptUtxo
  , payoutScriptUTxO :: ReferenceScriptUtxo
  , marloweScriptHash :: Chain.ScriptHash
  , payoutScriptHash :: Chain.ScriptHash
  }
  deriving (Generic)

deriving instance Show (MarloweContext 'V1)

-- | Data from Payout Scripts needed to solve the constraints.
data PayoutContext = PayoutContext
  { payoutOutputs :: Map Chain.TxOutRef Chain.TransactionOutput
  -- ^ The unspent payout outputs.
  , payoutScriptOutputs :: Map Chain.ScriptHash ReferenceScriptUtxo
  -- ^ The unspent payout reference script outputs indexed by script hash.
  }
  deriving (Generic, Show, Eq)

-- Data from Helper Scripts needed to solve the constraints.
data HelpersContext = HelpersContext
  { currentHelperScripts :: Map HelperScript HelperScriptInfo
  -- ^ The current version of the helper scripts.
  , helperPolicyId :: Chain.PolicyId
  -- ^ The roles currency for the contract.
  , helperScriptStates :: Map Chain.TokenName HelperScriptState
  -- ^ Map of which scripts play which role.
  }
  deriving (Generic)

deriving instance Show HelpersContext
-- | 'ToJSON HelpersContext' is provided by
-- 'Language.Marlowe.Runtime.Core.ScriptRegistry.JSON' because it transitively
-- uses a 'ReferenceScriptUtxo' which has a custom 'ToJSON' instance.

data HelperScriptState = HelperScriptState
  { helperScriptInfo :: HelperScriptInfo
  , helperUTxO :: Maybe (Chain.TxOutRef, Chain.TransactionOutput)
  }
  deriving (Generic)

deriving instance Show HelperScriptState
-- | 'ToJSON HelperScriptState' is provided by
-- 'Language.Marlowe.Runtime.Core.ScriptRegistry.JSON'.

data HelperScriptInfo = HelperScriptInfo
  { helperScript :: HelperScript
  , helperAddress :: Chain.Address
  , helperScriptUTxO :: ReferenceScriptUtxo
  , helperScriptHash :: Chain.ScriptHash
  }
  deriving (Generic)

deriving instance Show HelperScriptInfo
-- | 'ToJSON HelperScriptInfo' lives here (not in 'ScriptRegistry') so the
-- 'Constraints' module can derive 'ToJSON PayoutContext' without importing
-- 'ScriptRegistry' (which itself imports 'Constraints'). The custom instance
-- is needed because 'HelperScriptInfo' contains a 'ReferenceScriptUtxo' whose
-- own 'ToJSON' instance is also defined in this same transitive set.
instance ToJSON HelperScriptInfo where
  toJSON HelperScriptInfo{helperScript, helperAddress, helperScriptUTxO, helperScriptHash} =
    A.object
      [ "helperScript" .= helperScript
      , "helperAddress" .= helperAddress
      , "helperScriptUTxO" .= helperScriptUTxO
      , "helperScriptHash" .= helperScriptHash
      ]

type SolveConstraints era v =
  C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> Core.MarloweVersion v
  -> Either (MarloweContext v) PayoutContext
  -> WalletContext
  -> HelpersContext
  -> TxConstraints era v
  -> Either ConstraintError (C.TxBody era)

-- | Given a set of constraints and the context of a wallet, produces a
-- balanced, unsigned transaction that satisfies the constraints.
solveConstraints
  :: forall era v
   . SystemStart
  -> C.LedgerEpochInfo
  -> SolveConstraints era v
solveConstraints start history era protocol version scriptCtx walletCtx helpersCtx constraints = do
  solveInitialTxBodyContent era protocol version scriptCtx walletCtx helpersCtx constraints
    >>= adjustTxForMinUtxo era protocol (either (Just . marloweAddress) (const Nothing) scriptCtx)
    >>= selectCoins era protocol version scriptCtx walletCtx helpersCtx
    >>= balanceTx era start history protocol version scriptCtx walletCtx helpersCtx

-- | 2022-08 This function was written to compensate for a bug in Cardano's
--   calculateMinimumUTxO. It's called by adjustOutputForMinUTxO below. We will
--   eventually be able to remove it.
ensureAtLeastHalfAnAda :: C.Value -> C.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> C.lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = C.selectLovelace origValue
    minLovelace = Ledger.Coin 500_000

-- | Compute the `minAda` and adjust the lovelace in a single output to conform
--   to the minimum ADA requirement.
adjustOutputForMinUtxo
  :: forall era
   . C.MaryEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> C.TxOut C.CtxTx era
  -> Either ConstraintError (C.TxOut C.CtxTx era)
adjustOutputForMinUtxo era protocol (C.TxOut address txOrigValue datum script) = do
  let origValue = C.txOutValueToValue txOrigValue
      adjustedForCalculateMin = ensureAtLeastHalfAnAda origValue
      txOut' = C.TxOut address (mkTxOutValue era adjustedForCalculateMin) datum script
  let minLovelace = C.calculateMinimumUTxO (C.convert era) protocol txOut'
  let deficit =
        if minLovelace > C.selectLovelace origValue
          then minLovelace <> (negate $ C.selectLovelace origValue)
          else mempty
      newValue = origValue <> C.lovelaceToValue deficit
  pure $ C.TxOut address (mkTxOutValue era newValue) datum script

-- Adjusts all the TxOuts as necessary to comply with Minimum UTXO
-- requirements. Additionally, ensures that the Value of the marlowe output
-- does not change (fails with an error if it does).
adjustTxForMinUtxo
  :: forall era
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> Maybe Chain.Address
  -> C.TxBodyContent C.BuildTx era
  -> Either ConstraintError (C.TxBodyContent C.BuildTx era)
adjustTxForMinUtxo era protocol mMarloweAddress txBodyContent = do
  let getMarloweOutputValue :: [C.TxOut C.CtxTx era] -> Maybe (C.TxOutValue era)
      getMarloweOutputValue outputs = do
        marloweAddress <- mMarloweAddress
        getFirst
          . mconcat
          . map
            ( First
                . ( \(C.TxOut addressInEra txOutValue _ _) ->
                      if fromCardanoAddressInEra (C.toCardanoEra era) addressInEra == marloweAddress
                        then Just txOutValue
                        else Nothing
                  )
            )
          $ outputs

      origTxOuts = C.txOuts txBodyContent
      origMarloweValue = getMarloweOutputValue origTxOuts

      maryEraOnwards :: C.MaryEraOnwards era
      maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

  adjustedTxOuts <- traverse (adjustOutputForMinUtxo maryEraOnwards protocol) origTxOuts

  if origMarloweValue == getMarloweOutputValue adjustedTxOuts
    then Right $ txBodyContent{C.txOuts = adjustedTxOuts}
    else
      Left $
        CalculateMinUtxoFailed $
          "Marlowe output value changed during output adjustment:"
            <> show origMarloweValue
            <> " /= "
            <> show (getMarloweOutputValue adjustedTxOuts)

-- | Compute the maximum fee for any transaction.
maximumFee :: C.BabbageEraOnwards era -> Ledger.PParams (C.ShelleyLedgerEra era) -> Ledger.Coin
maximumFee era protocolParameters = C.babbageEraOnwardsConstraints era $ do
  let txFeeFixed = protocolParameters ^. ppTxFeeFixedL
      txFeePerByte = protocolParameters ^. ppTxFeePerByteL
      maxTxSize = protocolParameters ^. ppMaxTxSizeL
      C.ExecutionUnitPrices{..} = fromAlonzoPrices $ protocolParameters ^. ppPricesL
      C.ExecutionUnits{..} = fromAlonzoExUnits $ protocolParameters ^. ppMaxTxExUnitsL
      txFee = txFeeFixed + fromCompact txFeePerByte.unCoinPerByte * Coin (fromIntegral maxTxSize)
      executionFee = priceExecutionSteps * fromIntegral executionSteps + priceExecutionMemory * fromIntegral executionMemory
   in txFee + ceiling executionFee

-- | Calculate the minimum UTxO requirement for a value.
--   We must pack the address, datum and value into a dummy TxOut along with
--   the "none" reference script in order to use it with C.calculateMinimumUTxO
--   in a subsequent call
findMinUtxo
  :: forall era
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> (Chain.Address, Maybe Chain.Datum, C.Value)
  -> Either ConstraintError C.Value
findMinUtxo era protocol (chAddress, mbDatum, origValue) =
  do
    let atLeastHalfAnAda :: C.Value

        maryEraOnwards :: C.MaryEraOnwards era
        maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

        alonzoEraOnwards :: C.AlonzoEraOnwards era
        alonzoEraOnwards = babbageEraOnwardsToAlonzoEraOnwards era

        -- FIXME Found what looks like the same value being added and then subtracted from this computation
        -- atLeastHalfAnAda = origValue <> C.lovelaceToValue (maximum [500_000, C.selectLovelace origValue] - C.selectLovelace origValue)
        atLeastHalfAnAda = C.lovelaceToValue (max 500_000 (C.selectLovelace origValue))
        revisedValue = origValue <> C.negateValue (C.lovelaceToValue $ C.selectLovelace origValue) <> atLeastHalfAnAda
        datum =
          maybe
            C.TxOutDatumNone
            (C.TxOutSupplementalDatum alonzoEraOnwards . C.unsafeHashableScriptData . toCardanoScriptData)
            mbDatum

    dummyTxOut <- makeTxOut maryEraOnwards chAddress datum revisedValue C.ReferenceScriptNone
    let minLovelace = C.calculateMinimumUTxO (babbageEraOnwardsToShelleyBasedEra era) protocol dummyTxOut
    pure $ C.lovelaceToValue minLovelace

-- | Ensure that the minimum UTxO requirement is satisfied for outputs.
ensureMinUtxo
  :: forall era
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> (Chain.Address, C.Value)
  -> Either ConstraintError (Chain.Address, C.Value)
ensureMinUtxo era protocol (chAddress, origValue) =
  case findMinUtxo era protocol (chAddress, Nothing, origValue) of
    Right minValue ->
      pure
        ( chAddress
        , origValue
            <> (C.lovelaceToValue $ max (C.selectLovelace minValue) (C.selectLovelace origValue) - C.selectLovelace origValue)
        )
    Left e -> Left e

-- | Compute transaction output for building a transaction.
makeTxOut
  :: C.MaryEraOnwards era
  -> Chain.Address
  -> C.TxOutDatum C.CtxTx era
  -> C.Value
  -> C.ReferenceScript era
  -> Either ConstraintError (C.TxOut C.CtxTx era)
makeTxOut era address datum value referenceScript = do
  cardanoAddress <-
    note
      (CalculateMinUtxoFailed $ "Unable to convert address: " <> show address)
      $ C.anyAddressInShelleyBasedEra (C.convert era) <$> Chain.toCardanoAddressAny address
  Right $
    C.TxOut
      cardanoAddress
      (mkTxOutValue era value)
      datum
      referenceScript

-- Test whether a value only contains lovelace.
onlyLovelace :: C.Value -> Bool
onlyLovelace value = C.lovelaceToValue (C.selectLovelace value) == value

-- Selects enough additional inputs to cover the excess balance of the
-- transaction (total outputs - total inputs).
selectCoins
  :: forall era v
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> Core.MarloweVersion v
  -> Either (MarloweContext v) PayoutContext
  -> WalletContext
  -> HelpersContext
  -> C.TxBodyContent C.BuildTx era
  -> Either ConstraintError (C.TxBodyContent C.BuildTx era)
selectCoins era protocol marloweVersion scriptCtx walletCtx@WalletContext{..} helpersCtx txBodyContent = do
  let -- Extract the value of a UTxO
      txOutToValue :: C.TxOut C.CtxTx era -> C.Value
      txOutToValue (C.TxOut _ value _ _) = C.txOutValueToValue value

      -- All utxos that are spendable from either the Marlowe context or wallet context
      -- False means not including the reference utxo
      utxos :: [(C.TxIn, C.TxOut C.CtxTx era)]
      utxos = allUtxos era marloweVersion scriptCtx walletCtx helpersCtx False

      -- Compute the value of all available UTxOs
      universe :: C.Value
      universe = foldMap (txOutToValue . snd) utxos

      -- FIXME: Use protocolParamCollateralPercent from ProtocolParameters instead of this hard-coded '2'. We will automatically pick up future chain values this way.
      fee :: C.Value
      fee = C.lovelaceToValue $ 2 * maximumFee era protocol

      maryEraOnwards :: C.MaryEraOnwards era
      maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

      alonzoEraOnwards :: C.AlonzoEraOnwards era
      alonzoEraOnwards = babbageEraOnwardsToAlonzoEraOnwards era

  collateral <-
    let candidateCollateral =
          if Set.null collateralUtxos
            then utxos -- Use any UTxO if the wallet did not constrain collateral.
            else -- The filter below is safe because, by the definition of `WalletContext`,
            -- the `collateralUtxos` are an improper subset of `availableUtxos`. Also
            -- note that the definition of `WalletContext` does not *require* that
            -- every UTxO specified as collateral be used: it just states that those
            -- UTxOs are *available* for use as collateral.
              filter (flip Set.member collateralUtxos . fromCardanoTxIn . fst) utxos
        isPlutusScriptWitness C.PlutusScriptWitness{} = True
        isPlutusScriptWitness _ = False
        hasPlutusScriptWitness :: (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)) -> Bool
        hasPlutusScriptWitness (_, C.BuildTxWith (C.ScriptWitness _ witness)) = isPlutusScriptWitness witness
        hasPlutusScriptWitness _ = False
        hasPlutusMinting = any isPlutusScriptWitness' $ C.indexTxMintValue $ C.txMintValue txBodyContent
        isPlutusScriptWitness' :: (C.ScriptWitnessIndex, C.PolicyId, C.PolicyAssets, C.BuildTxWith C.BuildTx (C.ScriptWitness C.WitCtxMint era)) -> Bool
        isPlutusScriptWitness' (_, _, _, C.BuildTxWith sw) = case sw of
          C.PlutusScriptWitness{} -> True
          _ -> False
     in -- TODO: Support Babbage-style collateral, where multiple UTxOs are used and change is made.
        if hasPlutusMinting || any hasPlutusScriptWitness (C.txIns txBodyContent)
          then case filter
            ( \candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value && C.selectLovelace value >= C.selectLovelace fee
            )
            candidateCollateral of
            (txIn, _) : _ -> pure $ C.TxInsCollateral alonzoEraOnwards [txIn]
            [] -> do
              Left . CoinSelectionFailed $ NoCollateralFound (Set.fromList $ map (fromCardanoTxIn . fst) utxos)
          else pure C.TxInsCollateralNone -- No collateral is needed unless a Plutus script is being executed.

  -- Bound the lovelace that must be included with change
  -- Worst case scenario of how much ADA would be added to the native and non-native change outputs
  minUtxo <-
    (<>)
      <$> findMinUtxo era protocol (changeAddress, Nothing, universe) -- Output to native tokens.
      <*> findMinUtxo era protocol (changeAddress, Nothing, mempty) -- Pure lovelace to change address.
  let -- Compute the value of the outputs.
      outputsFromBody :: C.Value
      outputsFromBody = foldMap txOutToValue $ C.txOuts txBodyContent

      -- Sum of value that's been placed in the tx body
      -- example: spending the value at the Marlowe script address, this will be here as an input when we get the txBodyContent
      inputTxIns = map fst $ C.txIns txBodyContent
      inputsFromBody = foldMap (txOutToValue . snd) $ filter (flip elem inputTxIns . fst) utxos

      mintValue = C.txMintValueToValue $ C.txMintValue txBodyContent

      -- Find the net additional input that is needed; the extra input we need to find, worst case
      targetSelectionValue :: C.Value
      targetSelectionValue = outputsFromBody <> fee <> minUtxo <> (C.negateValue inputsFromBody <> C.negateValue mintValue)

      -- Remove the lovelace from a value.
      deleteLovelace :: C.Value -> C.Value
      deleteLovelace value = value <> C.negateValue (C.lovelaceToValue $ C.selectLovelace value)

      -- Compute the excess and missing tokens in a value.
      matchingCoins :: C.Value -> C.Value -> (Int, Int)
      matchingCoins required candidate =
        let delta :: [C.Quantity]
            delta =
              fmap snd
                . toList
                . deleteLovelace
                $ candidate <> C.negateValue required
            excess :: Int
            excess = length $ filter (> 0) delta
            deficit :: Int
            deficit = length $ filter (< 0) delta
         in (excess, deficit)

  -- Ensure that coin selection for tokens is possible.
  unless (snd (matchingCoins targetSelectionValue universe) == 0)
    . Left
    . CoinSelectionFailed
    $ do
      let missingValues = toList $ targetSelectionValue <> C.negateValue universe
          missingTokens = tokensFromCardanoValue $ fromList $ filter ((> C.Quantity 0) . snd) missingValues
      InsufficientTokens missingTokens

  -- Ensure that coin selection for lovelace is possible.
  -- unless (C.selectLovelace targetSelectionValue <= C.selectLovelace universe)
  unless (C.selectLovelace targetSelectionValue <= C.selectLovelace universe)
    . Left
    . CoinSelectionFailed
    $ do
      let Ledger.Coin required = C.selectLovelace targetSelectionValue
          Ledger.Coin available = C.selectLovelace universe
      InsufficientLovelace required available

  -- Satisfy the native-token requirements.
  let -- Sort the UTxOs by their deficit, excess, and lovelace in priority order.
      priority :: C.Value -> (C.TxIn, C.TxOut C.CtxTx era) -> (Int, Int, Bool, Bool)
      priority required candidate =
        let candidate' :: C.Value
            candidate' = txOutToValue $ snd candidate
            excess :: Int
            deficit :: Int
            (excess, deficit) = matchingCoins required candidate'
            notOnlyLovelace :: Bool
            notOnlyLovelace = not $ onlyLovelace candidate'
            insufficientLovelace :: Bool
            insufficientLovelace = C.selectLovelace candidate' < C.selectLovelace required
         in ( deficit -- It's most important to not miss any required coins,
            , excess -- but we don't want extra coins;
            , notOnlyLovelace -- prefer lovelace-only UTxOs if there is no deficit,
            , insufficientLovelace -- and prefer UTxOs with sufficient lovelace.
            )

      -- Use a simple greedy algorithm to select coins.
      select
        :: C.Value
        -> [(C.TxIn, C.TxOut C.CtxTx era)]
        -> [(C.TxIn, C.TxOut C.CtxTx era)]
      select _ [] = []
      select required candidates =
        let -- Choose the best UTxO from the candidates.
            next :: (C.TxIn, C.TxOut C.CtxTx era)
            -- next = minimumBy (compare `on` priority required) candidates
            next = minimumBy (compare `on` priority required) candidates
            -- Determine the remaining candidates.
            candidates' :: [(C.TxIn, C.TxOut C.CtxTx era)]
            -- candidates' = delete next candidates
            candidates' = delete next candidates
            -- Ignore negative quantities.
            filterPositive :: C.Value -> C.Value
            filterPositive = fromList . filter ((> 0) . snd) . toList
            -- Compute the remaining requirement.
            required' :: C.Value
            required' = filterPositive $ required <> C.negateValue (txOutToValue $ snd next)
         in -- Decide whether to continue.
            if required' == mempty
              then -- The requirements have been met.
                pure next
              else -- The requirements have not been met.
                next : select required' candidates'

      -- Select the coins.
      selection :: [(C.TxIn, C.TxOut C.CtxTx era)]
      selection = select targetSelectionValue $ filter (flip notElem inputTxIns . fst) utxos

      -- Compute the *native token* change, if any.
      change :: C.Value
      change =
        -- This is the change required to balance native tokens.
        deleteLovelace $ -- The lovelace are irrelevant because pure-lovelace change is handled during the final balancing.
          (mconcat $ txOutToValue . snd <$> selection) -- The inputs selected by the algorithm for spending many include native tokens that weren't in the required `outputs`.
            <> C.negateValue targetSelectionValue -- The tokens required by `outputs` (as represented in the `targetSelectionValue` requirement) shouldn't be included as change.
            -- Compute the change that contains native tokens used for balancing, omitting ones explicitly specified in the outputs.
      outputs = C.txOuts txBodyContent

  output <-
    if change == mempty
      then pure []
      else do
        (addr, val) <- ensureMinUtxo era protocol (changeAddress, change)
        (: []) <$> makeTxOut maryEraOnwards addr C.TxOutDatumNone val C.ReferenceScriptNone

  let -- FIXME Generalize to include script witnesses
      addWitness
        :: (C.TxIn, C.TxOut C.CtxTx era)
        -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
      addWitness (txIn, _txOut) = (txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)

  -- Return the transaction with coin selection added
  pure $
    txBodyContent
      { C.txInsCollateral = collateral
      , C.txIns = C.txIns txBodyContent <> fmap addWitness selection
      , C.txOuts = outputs <> (output :: [C.TxOut C.CtxTx era])
      }

-- FIXME: There are pathological failures that could happen if there are very many native tokens.

-- Ensure the fee is computed and covered, and that the excess input balance is
-- returned to the change address.
balanceTx
  :: forall era v
   . C.BabbageEraOnwards era
  -> SystemStart
  -> C.LedgerEpochInfo
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> Core.MarloweVersion v
  -> Either (MarloweContext v) PayoutContext
  -> WalletContext
  -> HelpersContext
  -> C.TxBodyContent C.BuildTx era
  -> Either ConstraintError (C.TxBody era)
balanceTx era systemStart eraHistory protocol marloweVersion scriptCtx walletCtx@WalletContext{..} helpersCtx C.TxBodyContent{..} = do
  changeAddress' <-
    maybe
      (Left $ BalancingError "Failed to convert change address.")
      Right
      $ toCardanoAddressInEra (C.toCardanoEra era) changeAddress

  let -- Extract the value of a UTxO
      txOutToValue :: C.TxOut ctx era -> C.Value
      txOutToValue (C.TxOut _ value _ _) = C.txOutValueToValue value

      maryEraOnwards :: C.MaryEraOnwards era
      maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

      -- Make the change output.
      mkChangeTxOut value = do
        let txOutValue = mkTxOutValue maryEraOnwards value
        C.TxOut changeAddress' txOutValue C.TxOutDatumNone C.ReferenceScriptNone

      -- Repeatedly try to balance the transaction.
      balancingLoop
        :: Integer
        -> C.Value
        -> Either ConstraintError (C.TxBodyContent C.BuildTx era, C.BalancedTxBody era)
      balancingLoop counter changeValue = do
        when (counter == 0) $
          Left . BalancingError $
            "Unsuccessful transaction balancing: "
              <> C.shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) (show C.TxBodyContent{..})
        let ledgerProtocolParams = C.LedgerProtocolParameters protocol
            -- Recompute execution units with full set of UTxOs, including change.
            buildTxBodyContent = C.TxBodyContent{..}{C.txOuts = mkChangeTxOut changeValue : txOuts}
            dummyTxOut =
              C.makeTransactionBodyAutoBalance
                (babbageEraOnwardsToShelleyBasedEra era)
                systemStart
                eraHistory
                ledgerProtocolParams
                mempty
                mempty
                mempty
                utxos
                buildTxBodyContent
                changeAddress'
                Nothing
        case dummyTxOut of
          -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
          Left (C.TxBodyErrorBalanceNegative delta _) -> do
            balancingLoop (counter - 1) (C.lovelaceToValue delta <> changeValue)
          Left err -> Left . BalancingError $ show err
          Right balanced@(C.BalancedTxBody C.TxBodyContent{txFee = fee} _ _ _) -> do
            pure (buildTxBodyContent{C.txFee = fee}, balanced)

      -- The available UTxOs.
      -- FIXME: This only needs to be the subset of available UTxOs that are actually `TxIns`, but including extras should be harmless.
      -- This time we call allUtxos we need to know the total cost, so we do want the reference script (passing True)
      utxos :: C.UTxO era
      utxos = C.UTxO . SMap.fromList $ allUtxos era marloweVersion scriptCtx walletCtx helpersCtx True

      -- Compute net of inputs and outputs, accounting for minting.
      totalIn =
        foldMap (txOutToValue . snd)
          -- 5. turn the right-side TxOutS that remain into ValueS
          . filter (flip elem (fst <$> txIns) . fst)
          -- 4. filter for membership of the TxIn in the ones from txIns
          . SMap.toList -- 3. turn that Map into a [(TxIn, (TxOut CtxUTxO era))]
          . C.unUTxO -- 2. extract the Map inside C.UTxO
          $ utxos -- 1. :: C.UTxO C.BabbageEra
      totalOut = foldMap txOutToValue txOuts
      totalMint = C.txMintValueToValue txMintValue

      -- Initial setup is `fee = 0` - we output all the difference as a change and expect balancing error ;-)
      initialChange = totalIn <> totalMint <> C.negateValue totalOut

  unless (onlyLovelace initialChange) $
    Left (BalancingError "balanceTx: Change must be pure lovelace")

  -- Return the transaction body.
  (_, C.BalancedTxBody _ txBody _ _) <- balancingLoop 10 initialChange
  pure txBody

-- The available UTxOs.
allUtxos
  :: forall era v ctx
   . C.BabbageEraOnwards era
  -> Core.MarloweVersion v
  -> Either (MarloweContext v) PayoutContext
  -> WalletContext
  -> HelpersContext
  -> Bool -- False if we do not want to include the script reference
  -> [(C.TxIn, C.TxOut ctx era)]
allUtxos era marloweVersion scriptCtx WalletContext{..} HelpersContext{..} includeReferences =
  let -- Convert chain UTxOs to Cardano API ones.
      maryEraOnwards :: C.MaryEraOnwards era
      maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

      era' :: C.CardanoEra era
      era' = C.toCardanoEra era

      convertUtxo :: (Chain.TxOutRef, Chain.TransactionOutput) -> Maybe (C.TxIn, C.TxOut ctx era)
      convertUtxo (txOutRef, transactionOutput) =
        (,) <$> toCardanoTxIn txOutRef <*> toCardanoTxOut' maryEraOnwards transactionOutput Nothing

      -- Extra UTxO for the input from the script.
      mkMarloweUtxo :: Core.TransactionScriptOutput v -> Maybe (C.TxIn, C.TxOut ctx era)
      mkMarloweUtxo Core.TransactionScriptOutput{..} =
        (,)
          <$> toCardanoTxIn utxo
          <*> ( C.TxOut
                  <$> toCardanoAddressInEra era' address
                  <*> toCardanoTxOutValue maryEraOnwards assets
                  <*> toCardanoTxOutDatum'
                    era'
                    ( Just
                        . fromCardanoDatumHash
                        . C.hashScriptDataBytes
                        . unsafeHashableScriptData
                        $ toCardanoScriptData
                        $ Core.toChainDatum marloweVersion datum
                    )
                  <*> pure C.ReferenceScriptNone
              )

      -- Extra UTxOs for reference scripts.
      mkReferenceUtxo :: ReferenceScriptUtxo -> Maybe (C.TxIn, C.TxOut ctx era)
      mkReferenceUtxo ReferenceScriptUtxo{..} = do
        let
          scriptInAnyLang = toCardanoScriptInAnyLang script
        (,)
          <$> toCardanoTxIn txOutRef
          <*> toCardanoTxOut' maryEraOnwards txOut (Just scriptInAnyLang)

      -- UTxOs for helper scripts.
      helperUTxO HelperScriptState{helperUTxO = Just (helperTxOutRef, helperTransactionOutput)} =
        maybe mempty pure $
          (,)
            <$> toCardanoTxIn helperTxOutRef
            <*> toCardanoTxOut' maryEraOnwards helperTransactionOutput Nothing
      helperUTxO _ = mempty
   in mapMaybe convertUtxo (SMap.toList availableUtxos.utxosMap)
        <> either
          (maybe mempty pure . (mkMarloweUtxo <=< scriptOutput))
          (mapMaybe convertUtxo . Map.toList . payoutOutputs)
          scriptCtx
        <> foldMap helperUTxO helperScriptStates
        <> mapMaybe
          mkReferenceUtxo
          ( filter (const includeReferences) $
              either (pure . marloweScriptUTxO) (Map.elems . payoutScriptOutputs) scriptCtx
                <> foldMap (pure . helperScriptUTxO . helperScriptInfo) helperScriptStates
          )

altEither :: Either e a -> Either e a -> Either e a
altEither (Left _) r = r
altEither l _ = l

solveInitialTxBodyContent
  :: forall era v
   . C.BabbageEraOnwards era
  -> Ledger.PParams (C.ShelleyLedgerEra era)
  -> Core.MarloweVersion v
  -> Either (MarloweContext v) PayoutContext
  -> WalletContext
  -> HelpersContext
  -> TxConstraints era v
  -> Either ConstraintError (C.TxBodyContent C.BuildTx era)
solveInitialTxBodyContent era protocol marloweVersion scriptCtx WalletContext{..} HelpersContext{..} TxConstraints{..} = do
  (txIns, requiredPayoutScriptHashes, helperTxInReferences, helperRoles) <- solveTxIns
  txInsReference <- solveTxInsReference requiredPayoutScriptHashes helperTxInReferences
  txOuts <- solveTxOuts helperRoles
  (txValidityLowerBound, txValidityUpperBound) <- solveTxValidityRange
  txExtraKeyWits <- solveTxExtraKeyWits
  txMintValue <- solveTxMintValue
  let ledgerProtocolParams = C.LedgerProtocolParameters protocol
  pure
    (C.defaultTxBodyContent shelleyEra)
      { txIns
      , txInsReference
      , txOuts
      , txValidityLowerBound
      , txValidityUpperBound
      , txMetadata
      , txExtraKeyWits
      , txProtocolParams = C.BuildTxWith $ Just ledgerProtocolParams
      , txMintValue
      }
  where
    shelleyEra :: C.ShelleyBasedEra era
    shelleyEra = babbageEraOnwardsToShelleyBasedEra era

    allegraEraOnwards :: C.AllegraEraOnwards era
    allegraEraOnwards = case era of
      C.BabbageEraOnwardsBabbage -> C.AllegraEraOnwardsBabbage
      C.BabbageEraOnwardsConway -> C.AllegraEraOnwardsConway
      C.BabbageEraOnwardsDijkstra -> C.AllegraEraOnwardsDijkstra

    maryEraOnwards :: C.MaryEraOnwards era
    maryEraOnwards = babbageEraOnwardsToMaryEraOnwards era

    alonzoEraOnwards :: C.AlonzoEraOnwards era
    alonzoEraOnwards = babbageEraOnwardsToAlonzoEraOnwards era

    getWalletInputs
      :: Set Chain.AssetId -> Either ConstraintError [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
    getWalletInputs helperRoles = do
      roleTokenInputs <- case roleTokenConstraints of
        RoleTokenConstraintsNone -> pure []
        DistributeRoleTokens _ -> pure [] -- Coin selection will handle these inputs.
        MintRoleTokens (MintingSeed txOutRef) _ _ -> do
          txIn <- note ToCardanoError $ toCardanoTxIn txOutRef
          _ <- note (MintingUtxoNotFound txOutRef) $ lookupUTxO txOutRef availableUtxos
          pure [(txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
        SpendRoleTokens roleTokens -> do
          let availTuples = map toUTxOTuple . toUTxOsList $ availableUtxos
              roleTokens' = roleTokens Set.\\ helperRoles
          txIns <-
            -- Filter out Ada because we don't need to specifically select an input for an Ada role token.
            nub <$> forM (filter (not . isAda) $ Set.toList roleTokens') \token -> do
              -- Find an element from availTuples where 'token' is in the assets.
              let containsToken :: Chain.TransactionOutput -> Bool
                  containsToken = Map.member token . Chain.unTokens . Chain.tokens . Chain.unTxOutAssets . Chain.assets
              (txOutRef, _) <- note (RoleTokenNotFound token) $ find (containsToken . snd) availTuples
              note ToCardanoError $ toCardanoTxIn txOutRef
          pure $ (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) <$> txIns
      threadTokenMintingInputs <- case threadTokenConstraints of
        PassThreadToken -> pure []
        BurnThreadToken -> pure []
        MintThreadToken (MintingSeed txOutRef) -> do
          seedTxIn <- note ToCardanoError $ toCardanoTxIn txOutRef
          if (not . any (\(txIn, _) -> txIn == seedTxIn)) roleTokenInputs
            then do
              _ <- note (MintingUtxoNotFound txOutRef) $ lookupUTxO txOutRef availableUtxos
              pure [(seedTxIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
            else pure []
      pure $ roleTokenInputs <> threadTokenMintingInputs

    getMarloweInput :: Either ConstraintError (Maybe (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)))
    getMarloweInput = case (marloweInputConstraints, scriptCtx) of
      (MarloweInputConstraintsNone, _) -> pure Nothing
      (MarloweInput _ _ inputs, Left MarloweContext{marloweScriptUTxO, scriptOutput}) -> fmap Just $ do
        Core.TransactionScriptOutput{datum, utxo} <- note MissingMarloweInput scriptOutput
        txIn <- note ToCardanoError $ toCardanoTxIn utxo
        plutusScriptOrRefInput <-
          note ToCardanoError $
            C.PReferenceScript
              <$> toCardanoTxIn (txOutRef marloweScriptUTxO)
        (plutusScriptV3InEra :: C.ScriptLanguageInEra C.PlutusScriptV3 era) <- case era of
            C.BabbageEraOnwardsBabbage -> Left ToCardanoError
            C.BabbageEraOnwardsConway -> pure C.PlutusScriptV3InConway
            C.BabbageEraOnwardsDijkstra -> pure C.PlutusScriptV3InDijkstra
        let

            scriptWitness =
              C.PlutusScriptWitness
                plutusScriptV3InEra
                C.PlutusScriptV3
                plutusScriptOrRefInput
                (C.ScriptDatumForTxIn $ Just . unsafeHashableScriptData $ toCardanoScriptData $ Core.toChainDatum marloweVersion datum)
                ( unsafeHashableScriptData $ toCardanoScriptData case marloweVersion of
                    Core.MarloweV1 ->
                      Chain.toDatum $
                        inputs <&> \case
                          V1.NormalInput content -> V1.Input content
                          V1.MerkleizedInput content hash _ -> V1.MerkleizedTxInput content hash
                )
                (C.ExecutionUnits 0 0)
        pure (txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending scriptWitness)
      (MarloweInput{}, Right _) -> Left MarloweInputInWithdraw

    getPayoutInputs
      :: Either ConstraintError [((C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)), Chain.ScriptHash)]
    getPayoutInputs
      | Set.null payoutInputConstraints = pure []
      | otherwise = case scriptCtx of
          Right PayoutContext{..} -> for (Set.toList payoutInputConstraints) \payoutRef -> do
            Chain.TransactionOutput{..} <- note (PayoutNotFound payoutRef) $ Map.lookup payoutRef payoutOutputs
            scriptHash <- note (InvalidPayoutScriptAddress payoutRef address) do
              credential <- Chain.paymentCredential address
              case credential of
                Chain.ScriptCredential hash -> pure hash
                _ -> Nothing
            payoutDatum <- note (InvalidPayoutDatum payoutRef datum) $ fromChainPayoutDatum marloweVersion =<< datum
            txIn <- note ToCardanoError $ toCardanoTxIn payoutRef
            referenceScriptOutput <-
              note (UnknownPayoutScript scriptHash) $ Map.lookup scriptHash payoutScriptOutputs
            referenceScriptTxIn <- note ToCardanoError $ toCardanoTxIn $ ScriptRegistry.txOutRef referenceScriptOutput
            (plutusScriptV3InEra :: C.ScriptLanguageInEra C.PlutusScriptV3 era) <- case era of
                C.BabbageEraOnwardsBabbage -> Left ToCardanoError
                C.BabbageEraOnwardsConway -> pure C.PlutusScriptV3InConway
                C.BabbageEraOnwardsDijkstra -> pure C.PlutusScriptV3InDijkstra

            let plutusScriptOrRefInput = C.PReferenceScript referenceScriptTxIn
                scriptWitness =
                  C.PlutusScriptWitness
                    plutusScriptV3InEra
                    C.PlutusScriptV3
                    plutusScriptOrRefInput
                    ( C.ScriptDatumForTxIn
                        $ Just
                          . C.unsafeHashableScriptData
                        $ toCardanoScriptData
                        $ Core.toChainPayoutDatum marloweVersion payoutDatum
                    )
                    (C.unsafeHashableScriptData $ C.ScriptDataConstructor 0 [])
                    (C.ExecutionUnits 0 0)
            pure ((txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending scriptWitness), scriptHash)
          Left _ -> Left PayoutInputInInitOrApply

    solveTxIns = do
      helperInputs <- getHelperInputs
      let helperRoles = Set.fromList $ fst . snd <$> helperInputs
      walletInputs <- getWalletInputs helperRoles
      marloweInputs <- maybeToList <$> getMarloweInput
      payoutInputs <- getPayoutInputs
      pure
        ( walletInputs <> marloweInputs <> (fst <$> payoutInputs) <> (fst <$> helperInputs)
        , Set.fromList $ snd <$> payoutInputs
        , Set.fromList $ snd . snd <$> helperInputs
        , helperRoles
        )

    solveTxInsReference :: Set Chain.ScriptHash -> Set C.TxIn -> Either ConstraintError (C.TxInsReference C.BuildTx era)
    solveTxInsReference requiredPayoutScriptHashes helperTxInReferences = do
      marloweScriptTxIn <- do
        marloweScriptUTxO <- getMarloweScriptUTxO
        note ToCardanoError $ toCardanoTxIn (txOutRef marloweScriptUTxO)
      payoutTxIns <- payoutTxInReferences requiredPayoutScriptHashes
      let
        txIns =
          marloweScriptTxIn
          : (payoutTxIns <> Set.toList helperTxInReferences)
      pure if null txIns
        then C.TxInsReferenceNone
        else C.TxInsReference era txIns $ C.BuildTxWith mempty

    getMarloweScriptUTxO :: Either ConstraintError ReferenceScriptUtxo
    getMarloweScriptUTxO = case scriptCtx of
      Left MarloweContext{marloweScriptUTxO} -> pure marloweScriptUTxO
      Right _ -> Left MarloweScriptUTxOInWithdraw

    -- Only include the payout reference scripts if we are consuming any payout
    -- inputs.
    payoutTxInReferences :: Set Chain.ScriptHash -> Either ConstraintError [C.TxIn]
    payoutTxInReferences requiredScriptHashes
      | Set.null requiredScriptHashes = pure []
      | otherwise = case scriptCtx of
          Right PayoutContext{..} ->
            for (Set.toList requiredScriptHashes) \payoutScriptHash -> do
              ScriptRegistry.ReferenceScriptUtxo{..} <-
                note (UnknownPayoutScript payoutScriptHash) $ Map.lookup payoutScriptHash payoutScriptOutputs
              note ToCardanoError $ toCardanoTxIn txOutRef
          Left _ -> Left PayoutInputInInitOrApply

    getMarloweOutput :: Either ConstraintError (Maybe Chain.TransactionOutput)
    getMarloweOutput = case (marloweOutputConstraints, scriptCtx) of
      (MarloweOutputConstraintsNone, _) -> Right Nothing
      (MarloweOutput contractAssets datum, Left MarloweContext{marloweAddress}) -> do
        threadToken <- getMintedThreadToken `altEither` getExistingThreadToken
        let
          outputAssets = contractAssets <> do
            unsafeTxOutAssets . Chain.Assets mempty . Chain.Tokens . Map.singleton threadToken $ Chain.Quantity 1
        Right $
          Just $
            Chain.TransactionOutput marloweAddress outputAssets Nothing $
              Just $
                Core.toChainDatum marloweVersion datum
      _ -> Left MarloweOutputInWithdraw

    getHelperInputs
      :: Either ConstraintError [((C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)), (Chain.AssetId, C.TxIn))]
    getHelperInputs = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure []
      MintRoleTokens{} -> pure []
      DistributeRoleTokens{} -> pure []
      SpendRoleTokens roleTokens -> catMaybes <$> mapM getHelperInput (Set.toList roleTokens)

    getHelperInput
      :: Chain.AssetId
      -> Either ConstraintError (Maybe ((C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)), (Chain.AssetId, C.TxIn)))
    getHelperInput role@(Chain.AssetId rolePolicy roleToken) =
      case (rolePolicy == helperPolicyId, roleToken `Map.lookup` helperScriptStates) of
        (True, Just HelperScriptState{helperScriptInfo, helperUTxO = Just (helperTxOutRef, helperTransactionOutput)}) ->
          do
            let HelperScriptInfo{..} = helperScriptInfo
                Chain.TransactionOutput{datum} = helperTransactionOutput
            txIn <- note ToCardanoError $ toCardanoTxIn helperTxOutRef
            helperDatum <- note (InvalidHelperDatum helperTxOutRef datum) $ toCardanoScriptData <$> datum
            referenceScriptTxIn <- note ToCardanoError $ toCardanoTxIn $ ScriptRegistry.txOutRef helperScriptUTxO
            (plutusScriptV3InEra :: C.ScriptLanguageInEra C.PlutusScriptV3 era) <- case era of
                C.BabbageEraOnwardsBabbage -> Left ToCardanoError
                C.BabbageEraOnwardsConway -> pure C.PlutusScriptV3InConway
                C.BabbageEraOnwardsDijkstra -> pure C.PlutusScriptV3InDijkstra
            let plutusScriptOrRefInput = C.PReferenceScript referenceScriptTxIn
                scriptWitness =
                  C.PlutusScriptWitness
                    plutusScriptV3InEra
                    C.PlutusScriptV3
                    plutusScriptOrRefInput
                    (C.ScriptDatumForTxIn . Just $ C.unsafeHashableScriptData helperDatum)
                    (C.unsafeHashableScriptData $ C.ScriptDataConstructor 0 []) -- FIXME: In the future, some helpers may require a redeemer, but open roles does not.
                    (C.ExecutionUnits 0 0)
            pure $
              pure ((txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending scriptWitness), (role, referenceScriptTxIn))
        _ -> pure Nothing

    getMerkleizedContinuationOutputs :: [Chain.TransactionOutput]
    getMerkleizedContinuationOutputs = case marloweInputConstraints of
      MarloweInput _ _ inputs -> case marloweVersion of
        Core.MarloweV1 -> flip mapMaybe inputs \case
          V1.MerkleizedInput _ _ continuation ->
            Just $
              Chain.TransactionOutput changeAddress mempty Nothing $
                Just $
                  Chain.toDatum continuation
          _ -> Nothing
      _ -> []

    getRoleTokenOutputs
      :: Set Chain.AssetId
      -> Either ConstraintError [Chain.TransactionOutput]
    getRoleTokenOutputs helperRoles = go roleTokenConstraints
      where
        go = \case
          RoleTokenConstraintsNone -> pure []
          MintRoleTokens _ _ dist -> go $ DistributeRoleTokens dist
          DistributeRoleTokens (SendToAddresses distribution) ->
            go . DistributeRoleTokens . SendToScripts $ Map.mapKeysMonotonic ToAddress <$> distribution
          DistributeRoleTokens (SendToScripts dist) -> do
            sequence $ runExceptT do
              (assetId, dist') <- lift $ Map.toList dist
              (dest, q) <- lift $ Map.toList dist'
              address <- case dest of
                ToAddress a -> pure a
                ToScript script ->
                  noteT (HelperScriptNotFound $ Chain.tokenName assetId)
                    . hoistMaybe
                    . Map.lookup script
                    $ helperAddress <$> currentHelperScripts
              -- In the current setup this will never happen because we guard this invariant on the distribution level.
              let assets = unsafeTxOutAssets (Chain.Assets mempty $ Chain.Tokens $ Map.singleton assetId q)
              datum <- case dest of
                ToAddress _ -> pure Nothing
                ToScript OpenRoleScript -> do
                  Chain.TokenName threadTokenNameBytes <- ExceptT . pure $ getMintedThreadTokenName
                  pure . Just . Chain.B $ threadTokenNameBytes
              pure
                Chain.TransactionOutput
                  { address
                  , assets = assets
                  , datumHash = Nothing
                  , datum
                  }
          SpendRoleTokens roleTokens -> do
            let availTuples = map toUTxOTuple . toUTxOsList $ availableUtxos
                roleTokens' = roleTokens Set.\\ helperRoles
            -- Ignore ada role tokens because we don't specifically select an input for it, and balancing will refund all
            -- spent Ada.
            nub <$> forM (filter (not . isAda) $ Set.toList roleTokens') \token -> do
              -- Find an element from availTuples where 'token' is in the assets.
              let containsToken :: Chain.TransactionOutput -> Bool
                  containsToken = Map.member token . Chain.unTokens . Chain.tokens . Chain.unTxOutAssets . Chain.assets
              note (RoleTokenNotFound token) $ snd <$> find (containsToken . snd) availTuples

    getPayoutOutputs :: Either ConstraintError [Chain.TransactionOutput]
    getPayoutOutputs = traverse (uncurry getPayoutOutput) $ Map.toList payToRoles

    getPayoutOutput :: Core.PayoutDatum v -> Chain.TxOutAssets -> Either ConstraintError Chain.TransactionOutput
    getPayoutOutput payoutDatum assets = case scriptCtx of
      Left MarloweContext{..} -> Right $ Chain.TransactionOutput payoutAddress assets Nothing $ Just datum
      _ -> Left PayoutOutputInWithdraw
      where
        datum = Core.toChainPayoutDatum marloweVersion payoutDatum

    getAddressOutputs :: [Chain.TransactionOutput]
    getAddressOutputs = uncurry getAddressOutput <$> Map.toList payToAddresses

    getAddressOutput :: Chain.Address -> Chain.TxOutAssets -> Chain.TransactionOutput
    getAddressOutput address assets = Chain.TransactionOutput address assets Nothing Nothing

    solveTxOuts helperRoles =
      note ToCardanoError . traverse (toCardanoTxOut maryEraOnwards) =<< do
        roleTokenOutputs <- getRoleTokenOutputs helperRoles
        marloweOutput <- getMarloweOutput
        payoutOutputs <- getPayoutOutputs
        pure $
          concat
            [ maybeToList marloweOutput
            , getMerkleizedContinuationOutputs
            , roleTokenOutputs
            , payoutOutputs
            , getAddressOutputs
            ]

    solveTxValidityRange = case marloweInputConstraints of
      MarloweInputConstraintsNone ->
        pure
          ( C.TxValidityNoLowerBound
          , C.TxValidityUpperBound shelleyEra Nothing
          )
      MarloweInput minSlotNo maxSlotNo _ -> do
        pure
          ( C.TxValidityLowerBound allegraEraOnwards minSlotNo
          , C.TxValidityUpperBound shelleyEra $ Just maxSlotNo
          )

    Chain.TransactionMetadata encodedMetadata = encodeMarloweTransactionMetadata metadataConstraints

    txMetadata :: C.TxMetadataInEra era
    txMetadata
      | Map.null encodedMetadata = C.TxMetadataNone
      | otherwise = C.TxMetadataInEra shelleyEra $ C.TxMetadata $ toCardanoMetadata <$> encodedMetadata

    solveTxExtraKeyWits :: Either ConstraintError (C.TxExtraKeyWitnesses era)
    solveTxExtraKeyWits
      | Set.null signatureConstraints = pure C.TxExtraKeyWitnessesNone
      | otherwise =
          note ToCardanoError $
            C.TxExtraKeyWitnesses alonzoEraOnwards
              <$> traverse toCardanoPaymentKeyHash (Set.toList signatureConstraints)

    getExistingThreadToken :: Either ConstraintError Chain.AssetId
    getExistingThreadToken = do
      case scriptCtx of
        -- Extract the thread token
        Left (MarloweContext { scriptOutput=Just (Core.TransactionScriptOutput {assets})}) -> do
          marlowePolicyId <- getMarlowePolicyId
          let
            possibleThreadToken = do
              let
                tokens = Map.toList . Chain.unTokens . Chain.tokens . Chain.unTxOutAssets $ assets
                isMarlowePolicyToken (Chain.AssetId policyId _tokenName, _quantity) = policyId == marlowePolicyId
              filter isMarlowePolicyToken tokens
          case possibleThreadToken of
            [(threadToken, _quantity)] -> pure threadToken
            _ -> Left ThreadTokenNotFound
        _ -> Left ThreadTokenNotFound

    getMintedThreadTokenName :: Either ConstraintError Chain.TokenName
    getMintedThreadTokenName = case threadTokenConstraints of
      MintThreadToken (MintingSeed (Chain.TxOutRef { txId = Chain.TxId txId, txIx = Chain.TxIx txIx })) -> do
        let
          seed = P.TxOutRef (P.TxId $ P.toBuiltin txId) (fromIntegral txIx)
          -- We hard code the Marlowe output index to be `0` because at the moment
          -- our tx builder handles only single contract creation / closure per tx.
          V1.TokenName (P.fromBuiltin -> threadTokenNameBytes) = V1.mkThreadTokenName seed 0
        pure $ Chain.TokenName threadTokenNameBytes
      BurnThreadToken -> Left ThreadTokenNotMinted
      PassThreadToken -> Left ThreadTokenNotMinted

    getMintedThreadToken :: Either ConstraintError Chain.AssetId
    getMintedThreadToken = do
      marlowePolicyId <- getMarlowePolicyId
      threadTokenName <- getMintedThreadTokenName
      pure $ Chain.AssetId marlowePolicyId threadTokenName

    getMarlowePolicyId :: Either ConstraintError Chain.PolicyId
    getMarlowePolicyId = do
      marloweScriptUTxO <- getMarloweScriptUTxO
      pure . Chain.scriptHashToPolicyId . hashScriptInPlutus $ marloweScriptUTxO.script

    getThreadTokenMintValue :: Either ConstraintError (C.TxMintValue C.BuildTx era)
    getThreadTokenMintValue = do
      mintingCtx :: Maybe (Chain.Redeemer, Chain.TokenName, Chain.Quantity) <- case threadTokenConstraints of
        PassThreadToken -> pure Nothing
        MintThreadToken (MintingSeed (Chain.TxOutRef { txId = Chain.TxId txId, txIx = Chain.TxIx txIx })) -> do
          let
            seed = P.TxOutRef (P.TxId $ P.toBuiltin txId) (fromIntegral txIx)
            redeemer = Chain.toRedeemer $ V1.Minting seed
          tokenName <- getMintedThreadTokenName
          pure $ Just (redeemer, tokenName, Chain.Quantity 1)
        BurnThreadToken -> do
          let
            redeemer = Chain.toRedeemer V1.Burning
          threadToken <- getExistingThreadToken
          let Chain.AssetId _ tokenName = threadToken
          pure $ Just (redeemer, tokenName, Chain.Quantity (-1))
      case mintingCtx of
        Nothing -> pure C.TxMintNone
        Just (Chain.Redeemer redeemer, tokeName, quantity) -> do
          marloweScriptUTxO <- getMarloweScriptUTxO
          (plutusScriptV3InEra :: C.ScriptLanguageInEra C.PlutusScriptV3 era) <- case era of
            C.BabbageEraOnwardsBabbage -> Left ToCardanoError
            C.BabbageEraOnwardsConway -> pure C.PlutusScriptV3InConway
            C.BabbageEraOnwardsDijkstra -> pure C.PlutusScriptV3InDijkstra
          plutusScriptOrRefInput <-
            note ToCardanoError $
              C.PReferenceScript
                <$> toCardanoTxIn marloweScriptUTxO.txOutRef
          marlowePolicyId <- getMarlowePolicyId
          let
            witness =
              C.PlutusScriptWitness
                plutusScriptV3InEra
                C.PlutusScriptV3
                plutusScriptOrRefInput
                C.NoScriptDatumForMint
                (C.unsafeHashableScriptData $ toCardanoScriptData redeemer)
                (C.ExecutionUnits 0 0)
            marloweAssetId = Chain.AssetId marlowePolicyId tokeName
            tokens = Chain.Tokens $ Map.singleton marloweAssetId quantity
          value <- note ToCardanoError $ tokensToCardanoValue tokens
          let
            policyAssetsMap = C.valueToPolicyAssets value
            mintValues = [(pid, assets, C.BuildTxWith witness) | (pid, assets) <- Map.toList policyAssetsMap]
          pure $ C.mkTxMintValue maryEraOnwards mintValues

    getRoleTokenMintValue :: Either ConstraintError (C.TxMintValue C.BuildTx era)
    getRoleTokenMintValue = case roleTokenConstraints of
      MintRoleTokens _ witness (SendToScripts distribution) -> go witness $ case traverse demoteDestination distribution of
        Nothing -> distribution
        Just distribution' -> Map.mapKeysMonotonic ToAddress <$> distribution'
      MintRoleTokens _ witness (SendToAddresses distribution) -> go witness (Map.mapKeysMonotonic ToAddress <$> distribution)
      _ -> pure C.TxMintNone
      where
        go witness distribution = do
          let tokens = fold <$> distribution
          value <- note ToCardanoError $ tokensToCardanoValue $ Chain.Tokens tokens
          let
            policyAssetsMap = C.valueToPolicyAssets value
            mintValues = [(pid, assets, C.BuildTxWith witness) | (pid, assets) <- Map.toList policyAssetsMap]
          pure $ C.mkTxMintValue maryEraOnwards mintValues

    solveTxMintValue :: Either ConstraintError (C.TxMintValue C.BuildTx era)
    solveTxMintValue = do
      threadTokenMintValue <- getThreadTokenMintValue
      roleTokenMintValue <- getRoleTokenMintValue
      pure $ threadTokenMintValue <> roleTokenMintValue

demoteDestination :: Map Destination Chain.Quantity -> Maybe (Map Chain.Address Chain.Quantity)
demoteDestination dist =
  Map.fromDistinctAscList <$> for (Map.toAscList dist) \case
    (ToAddress addr, q) -> Just (addr, q)
    _ -> Nothing

isAda :: Chain.AssetId -> Bool
isAda (Chain.AssetId "" "") = True
isAda _ = False

mkTxOutValue :: C.MaryEraOnwards era -> C.Value -> C.TxOutValue era
mkTxOutValue C.MaryEraOnwardsMary = C.TxOutValueShelleyBased C.ShelleyBasedEraMary . C.toLedgerValue C.MaryEraOnwardsMary
mkTxOutValue C.MaryEraOnwardsAlonzo = C.TxOutValueShelleyBased C.ShelleyBasedEraAlonzo . C.toLedgerValue C.MaryEraOnwardsAlonzo
mkTxOutValue C.MaryEraOnwardsBabbage = C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage . C.toLedgerValue C.MaryEraOnwardsBabbage
mkTxOutValue C.MaryEraOnwardsConway = C.TxOutValueShelleyBased C.ShelleyBasedEraConway . C.toLedgerValue C.MaryEraOnwardsConway
mkTxOutValue C.MaryEraOnwardsDijkstra = C.TxOutValueShelleyBased C.ShelleyBasedEraDijkstra . C.toLedgerValue C.MaryEraOnwardsDijkstra
