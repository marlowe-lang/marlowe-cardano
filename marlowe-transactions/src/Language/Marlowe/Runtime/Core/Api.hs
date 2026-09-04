{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Marlowe.Runtime.Core.Api where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Control.Monad (join, zipWithM, (<=<))
import Data.Aeson (
  FromJSON (..),
  FromJSONKey,
  Result (..),
  ToJSON (..),
  ToJSONKey (toJSONKey),
  Value (..),
  eitherDecode,
  encode,
 )
import Data.Aeson.Types (Parser, parse, parseFail, toJSONKeyText)
import Data.Bifunctor (first)
import Data.Binary (Binary (..), Get, Put, getWord8, putWord8)
import Data.Binary.Get (getWord32be)
import Data.Binary.Put (putWord32be)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16Untyped, encodeBase16)
import Data.Either (fromRight)
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Object.Types (ContractHash)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import GHC.Generics (Generic, to)
import qualified Marlowe.Plutus.Analysis.Safety.Types as V1
import qualified Marlowe.Plutus.Semantics as V1
import qualified Marlowe.Plutus.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader,
  TokenName (..),
  TxId (..),
  TxOutRef (..),
  parseTxOutRef,
  renderTxOutRef,
  unPolicyId,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Data.Variations (GVariations (gVariations), Variations (..), varyAp)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as Plutus
import qualified PlutusLedgerApi.V2 as PV2
import Data.Base16.Types (extractBase16)
import qualified PlutusTx.AssocMap as AM
import qualified Cardano.Api as C
import Data.Data (Proxy(..))
import qualified Data.Aeson as A
import Data.Set (Set)

-- | The ID of a contract is the TxId and TxIx of the UTxO that first created
-- the contract.
newtype ContractId = ContractId {unContractId :: TxOutRef}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving anyclass (Binary, Variations)

instance ToJSON ContractId where
  toJSON = String . renderContractId

instance ToJSONKey ContractId where
  toJSONKey = toJSONKeyText renderContractId

instance FromJSON ContractId where
  parseJSON json = do
    t <- parseJSON json
    case parseContractId t of
      Nothing -> fail $ "Invalid ContractId JSON: " <> t
      Just cid -> pure cid

parseContractId :: String -> Maybe ContractId
parseContractId = fmap ContractId . parseTxOutRef . T.pack

renderContractId :: ContractId -> Text
renderContractId = renderTxOutRef . unContractId

data MarloweVersionTag
  = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

instance TestEquality MarloweVersion where
  testEquality MarloweV1 MarloweV1 = Just Refl

assertVersionsEqual :: MarloweVersion v -> MarloweVersion v' -> v :~: v'
assertVersionsEqual v1 v2 =
  fromMaybe
    (error $ "getNextSteps: Marlowe version mismatch. Expected " <> show v1 <> ", got " <> show v2)
    (testEquality v1 v2)

deriving instance Show (MarloweVersion v)
deriving instance Eq (MarloweVersion v)
deriving instance Ord (MarloweVersion v)

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: Type
  type TransactionError v :: Type
  type Datum v :: Type
  type State v :: Type
  type Inputs (v :: MarloweVersionTag) :: Type
  type TransactionInputs (v :: MarloweVersionTag) :: Type
  type PayoutDatum v :: Type
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type TransactionError 'V1 = V1.TransactionError
  type Datum 'V1 = V1.MarloweData
  type State 'V1 = V1.State
  type Inputs 'V1 = [V1.Input]
  type TransactionInputs 'V1 = V1.TransactionInput
  type PayoutDatum 'V1 = Chain.AssetId
  marloweVersion = MarloweV1

withMarloweVersion :: MarloweVersion v -> ((IsMarloweVersion v) => a) -> a
withMarloweVersion = \case
  MarloweV1 -> id

instance (IsMarloweVersion v) => Variations (MarloweVersion v) where
  variations = pure $ marloweVersion @v

instance ToJSON (MarloweVersion v) where
  toJSON =
    String . \case
      MarloweV1 -> "v1"

newtype MarloweMetadataTag = MarloweMetadataTag {getMarloweMetadataTag :: Text}
  deriving newtype (Show, Eq, Ord, IsString, FromJSON, ToJSON, Binary, ToJSONKey, FromJSONKey, Variations)

-- | The standard metadata structure for Marlowe Contracts (metadata index 1564).
-- | New versions should get new constructors here.
data MarloweMetadata = MarloweMetadata
  { tags :: Map MarloweMetadataTag (Maybe Chain.Metadata)
  -- ^ custom metadata. The keys are indexed and searchable.
  , continuations :: Maybe Text
  -- ^ A URI that points to the continuation map for this contract.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A wrapper around `Chain.TransactionMetadata` that has had the
-- MarloweMetadata extracted.
data MarloweTransactionMetadata = MarloweTransactionMetadata
  { marloweMetadata :: Maybe MarloweMetadata
  -- ^ The Marlowe metadata (key 1564), if present.
  , transactionMetadata :: Chain.TransactionMetadata
  -- ^ The raw underlying transaction metadata.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, Variations)

instance Binary MarloweTransactionMetadata where
  put = put . encodeMarloweTransactionMetadata
  get = decodeMarloweTransactionMetadataLenient <$> get

emptyMarloweTransactionMetadata :: MarloweTransactionMetadata
emptyMarloweTransactionMetadata = MarloweTransactionMetadata Nothing mempty

data MetadataDecodeError
  = ExpectedMap
  | ExpectedList
  | ExpectedNumber
  | ExpectedText
  | ExpectedChunkedText
  | ExpectedBytes
  | ErrorInMap Chain.Metadata MetadataDecodeError
  | ErrorInList Natural MetadataDecodeError
  | MapIndexNotFound Natural
  | ListIndexNotFound Natural
  | InvalidValue Text
  | OneOf MetadataDecodeError MetadataDecodeError
  deriving (Eq, Ord, Show)

encodeMarloweTransactionMetadata :: MarloweTransactionMetadata -> Chain.TransactionMetadata
encodeMarloweTransactionMetadata MarloweTransactionMetadata{..} = case marloweMetadata of
  Nothing -> transactionMetadata
  Just m ->
    Chain.TransactionMetadata $
      Map.insert 1564 (encodeMarloweMetadata m) $
        Chain.unTransactionMetadata transactionMetadata

decodeMarloweTransactionMetadataLenient :: Chain.TransactionMetadata -> MarloweTransactionMetadata
decodeMarloweTransactionMetadataLenient metadata =
  fromRight (MarloweTransactionMetadata Nothing metadata) $ decodeMarloweTransactionMetadata metadata

decodeMarloweTransactionMetadata :: Chain.TransactionMetadata -> Either MetadataDecodeError MarloweTransactionMetadata
decodeMarloweTransactionMetadata metadata =
  MarloweTransactionMetadata
    <$> traverse decodeMarloweMetadata (Map.lookup 1564 $ Chain.unTransactionMetadata metadata)
    <*> pure metadata

encodeMarloweMetadata :: MarloweMetadata -> Chain.Metadata
encodeMarloweMetadata MarloweMetadata{..} =
  Chain.MetadataList $
    catMaybes
      [ Just $ Chain.MetadataNumber 2 -- version
      , Just $ Chain.MetadataList $ uncurry encodeMetadataTag <$> Map.toList tags -- tags
      , Chain.MetadataText <$> continuations -- continuations
      ]

encodeMetadataTag :: MarloweMetadataTag -> Maybe Chain.Metadata -> Chain.Metadata
encodeMetadataTag (MarloweMetadataTag tag) = \case
  Nothing -> Chain.MetadataList [tagEncoded]
  Just payload -> Chain.MetadataList [tagEncoded, payload]
  where
    tagEncoded = case T.chunksOf 64 tag of
      [] -> Chain.MetadataText ""
      [x] -> Chain.MetadataText x
      xs -> Chain.MetadataList $ Chain.MetadataText <$> xs

decodeMarloweMetadata :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadata m = do
  decoder <- flip (withMetadataListIx 0) m $ withMetadataNumber \case
    1 -> pure decodeMarloweMetadataV1
    2 -> pure decodeMarloweMetadataV2
    v -> Left $ InvalidValue $ T.pack $ "Unknown contract metadata version " <> show v
  decoder m

decodeMarloweMetadataV1 :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV1 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV1 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMarloweMetadataV2 :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV2 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV2 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMetadataTagsV1
  :: Chain.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe Chain.Metadata))
decodeMetadataTagsV1 =
  fmap Map.fromList . withMetadataList \case
    Chain.MetadataList ms ->
      withMetadataTuple
        (withMetadataText $ pure . MarloweMetadataTag)
        (pure . Just)
        (Chain.MetadataList ms)
    Chain.MetadataText tag -> pure (MarloweMetadataTag tag, Nothing)
    _ -> Left $ OneOf ExpectedList ExpectedText

decodeMetadataTagsV2
  :: Chain.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe Chain.Metadata))
decodeMetadataTagsV2 =
  fmap Map.fromList . withMetadataList \m ->
    (,)
      <$> withMetadataListIx 0 decodeMetadataTag m
      <*> withMetadataListIxOptional 1 pure m

decodeMetadataTag :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadataTag
decodeMetadataTag = withMetadataChunkedText $ pure . MarloweMetadataTag

withMetadataNumber :: (Integer -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataNumber f = \case
  Chain.MetadataNumber i -> f i
  _ -> Left ExpectedNumber

withMetadataChunkedText :: (Text -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataChunkedText f = \case
  Chain.MetadataList xs -> f . T.concat =<< withMetadataList (withMetadataText pure) (Chain.MetadataList xs)
  Chain.MetadataText i -> f i
  _ -> Left ExpectedChunkedText

withMetadataText :: (Text -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataText f = \case
  Chain.MetadataText i -> f i
  _ -> Left ExpectedText

withMetadataList :: (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError [a]
withMetadataList f = \case
  Chain.MetadataList ms -> zipWithM (\i -> first (ErrorInList i) . f) [0 ..] ms
  _ -> Left ExpectedList

withMetadataListIx
  :: Natural -> (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataListIx ix f = maybe (Left $ ListIndexNotFound ix) pure <=< withMetadataListIxOptional ix f

withMetadataListIxOptional
  :: Natural -> (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError (Maybe a)
withMetadataListIxOptional ix f = \case
  Chain.MetadataList ms -> case drop (fromIntegral ix) ms of
    [] -> pure Nothing
    m : _ -> first (ErrorInList ix) $ Just <$> f m
  _ -> Left ExpectedList

withMetadataTuple
  :: (Chain.Metadata -> Either MetadataDecodeError a)
  -> (Chain.Metadata -> Either MetadataDecodeError b)
  -> Chain.Metadata
  -> Either MetadataDecodeError (a, b)
withMetadataTuple f g m = (,) <$> withMetadataListIx 0 f m <*> withMetadataListIx 1 g m

data Transaction v = Transaction
  { transactionId :: TxId
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , blockHeader :: BlockHeader
  , validityLowerBound :: UTCTime
  , validityUpperBound :: UTCTime
  , inputs :: Inputs v
  , output :: TransactionOutput v
  }
  deriving (Generic)

deriving instance Show (Transaction 'V1)
deriving instance Eq (Transaction 'V1)
instance ToJSON (Transaction 'V1)
instance Variations (Transaction 'V1)

instance Binary (Transaction 'V1) where
  put Transaction{..} = do
    put transactionId
    put contractId
    put metadata
    put blockHeader
    put validityLowerBound
    put validityUpperBound
    putInputs MarloweV1 inputs
    put output
  get =
    Transaction
      <$> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> getInputs MarloweV1
      <*> get

data TransactionOutput v = TransactionOutput
  { payouts :: Map Chain.TxOutRef (Payout v)
  , scriptOutput :: Maybe (TransactionScriptOutput v)
  }
  deriving (Generic)

deriving instance Show (TransactionOutput 'V1)
deriving instance Eq (TransactionOutput 'V1)
instance ToJSON (TransactionOutput 'V1)
instance Variations (TransactionOutput 'V1)

instance Binary (TransactionOutput 'V1) where
  put TransactionOutput{..} = do
    put payouts
    put scriptOutput
  get = TransactionOutput <$> get <*> get

data Payout v = Payout
  { address :: Chain.Address
  , assets :: Chain.TxOutAssets
  , datum :: PayoutDatum v
  }
  deriving (Generic)

deriving instance Show (Payout 'V1)
deriving instance Eq (Payout 'V1)
instance ToJSON (Payout 'V1)
instance Variations (Payout 'V1)

instance Binary (Payout 'V1) where
  put Payout{..} = do
    put address
    put assets
    put datum
  get = Payout <$> get <*> get <*> get

data TransactionScriptOutput v = TransactionScriptOutput
  { address :: Chain.Address
  , assets :: Chain.TxOutAssets
  , utxo :: TxOutRef
  , datum :: Datum v
  }
  deriving (Generic)

deriving instance Show (TransactionScriptOutput 'V1)
deriving instance Eq (TransactionScriptOutput 'V1)
instance ToJSON (TransactionScriptOutput 'V1)
instance Variations (TransactionScriptOutput 'V1)

instance Binary (TransactionScriptOutput 'V1) where
  put TransactionScriptOutput{..} = do
    put address
    put assets
    put utxo
    putDatum MarloweV1 datum
  get = TransactionScriptOutput <$> get <*> get <*> get <*> getDatum MarloweV1

data SomeTransactionScriptOutput = forall v. SomeTransactionScriptOutput (MarloweVersion v) (TransactionScriptOutput v)

instance Show SomeTransactionScriptOutput where
  show (SomeTransactionScriptOutput version output) = case version of
    MarloweV1 -> "SomeTransactionScriptOutputV1 " <> show output

instance Eq SomeTransactionScriptOutput where
  SomeTransactionScriptOutput MarloweV1 output1 == SomeTransactionScriptOutput MarloweV1 output2 = output1 == output2

instance ToJSON SomeTransactionScriptOutput where
  toJSON (SomeTransactionScriptOutput version output) = case version of
    MarloweV1 ->
      A.object
        [ "version" A..= MarloweV1
        , "output" A..= output
        ]

instance Variations SomeTransactionScriptOutput where
  variations =
    join $
      NE.fromList
        [ SomeTransactionScriptOutput MarloweV1 <$> variations
        ]

instance Binary SomeTransactionScriptOutput where
  put (SomeTransactionScriptOutput version output) = case version of
    MarloweV1 -> do
      put $ SomeMarloweVersion MarloweV1
      put output
  get =
    get >>= \case
      SomeMarloweVersion MarloweV1 -> SomeTransactionScriptOutput MarloweV1 <$> get

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

instance Eq SomeMarloweVersion where
  SomeMarloweVersion MarloweV1 == SomeMarloweVersion MarloweV1 = True

instance Ord SomeMarloweVersion where
  compare (SomeMarloweVersion MarloweV1) (SomeMarloweVersion MarloweV1) = EQ

instance Bounded SomeMarloweVersion where
  minBound = SomeMarloweVersion MarloweV1
  maxBound = SomeMarloweVersion MarloweV1

instance Enum SomeMarloweVersion where
  toEnum = \case
    0 -> SomeMarloweVersion MarloweV1
    _ -> error "toEnum: value out of range of Marlowe versions"
  fromEnum (SomeMarloweVersion version) = case version of
    MarloweV1 -> 0

instance Show SomeMarloweVersion where
  showsPrec p (SomeMarloweVersion version) =
    showParen (p >= 11) $
      showString "SomeMarloweVersion " . showsPrec 11 version

withSomeMarloweVersion :: (forall v. MarloweVersion v -> r) -> SomeMarloweVersion -> r
withSomeMarloweVersion f (SomeMarloweVersion v) = f v

instance ToJSON SomeMarloweVersion where
  toJSON (SomeMarloweVersion v) = toJSON v

instance Variations SomeMarloweVersion where
  variations =
    NE.fromList
      [ SomeMarloweVersion MarloweV1
      ]

instance FromJSON SomeMarloweVersion where
  parseJSON json = do
    s :: Text <- parseJSON json
    case s of
      "v1" -> pure $ SomeMarloweVersion MarloweV1
      _ -> fail "Invalid marlowe version"

instance Binary SomeMarloweVersion where
  put (SomeMarloweVersion v) = case v of
    MarloweV1 -> putWord32be 0x01
  get =
    getWord32be >>= \case
      0x01 -> pure $ SomeMarloweVersion MarloweV1
      _ -> fail "Invalid marlowe version bytes"

putContract :: MarloweVersion v -> Contract v -> Put
putContract v = put . encode . contractToJSON v

getContract :: MarloweVersion v -> Get (Contract v)
getContract v = do
  bytes <- get
  case eitherDecode bytes of
    Left err -> fail err
    Right json -> case parse (contractFromJSON v) json of
      Error err -> fail err
      Success c -> pure c

putInputs :: MarloweVersion v -> Inputs v -> Put
putInputs MarloweV1 = put . Chain.toDatum

getInputs :: MarloweVersion v -> Get (Inputs v)
getInputs MarloweV1 = do
  raw <- get
  case Chain.fromDatum raw of
    Nothing -> fail "failed to decode inputs"
    Just r -> pure r

putDatum :: MarloweVersion v -> Datum v -> Put
putDatum v = put . toChainDatum v

getDatum :: MarloweVersion v -> Get (Datum v)
getDatum v = do
  raw <- get
  case fromChainDatum v raw of
    Nothing -> fail "failed to decode datum"
    Just d -> pure d

putPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Put
putPayoutDatum MarloweV1 = put

getPayoutDatum :: MarloweVersion v -> Get (PayoutDatum v)
getPayoutDatum MarloweV1 = get

contractToJSON :: MarloweVersion v -> Contract v -> Value
contractToJSON = \case
  MarloweV1 -> toJSON

contractFromJSON :: MarloweVersion v -> Value -> Parser (Contract v)
contractFromJSON = \case
  MarloweV1 -> parseJSON

payoutDatumToJSON :: MarloweVersion v -> PayoutDatum v -> Value
payoutDatumToJSON = \case
  MarloweV1 -> \case
    Chain.AssetId policyId tokenName ->
      toJSON
        ( String . extractBase16 . encodeBase16 . unPolicyId $ policyId
        , String . extractBase16 . encodeBase16 . unTokenName $ tokenName
        )

payoutDatumFromJSON :: MarloweVersion v -> Value -> Parser (PayoutDatum v)
payoutDatumFromJSON = \case
  MarloweV1 -> \json -> do
    (p, t) <- parseJSON json
    p' <- either (parseFail . T.unpack) (pure . Chain.PolicyId) . decodeBase16Untyped . encodeUtf8 $ t
    t' <- either (parseFail . T.unpack) (pure . Chain.TokenName) . decodeBase16Untyped . encodeUtf8 $ p
    pure $ Chain.AssetId p' t'

datumToJSON :: MarloweVersion v -> Datum v -> Value
datumToJSON = \case
  MarloweV1 -> toJSON

datumFromJSON :: MarloweVersion v -> Value -> Parser (Datum v)
datumFromJSON = \case
  MarloweV1 -> parseJSON

toChainPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Chain.Datum
toChainPayoutDatum = \case
  MarloweV1 -> \case
    Chain.AssetId policyId tokenName -> do
      let currencySymbol = Plutus.currencySymbol . unPolicyId $ policyId
          tokenName' = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName $ tokenName
      Chain.toDatum (currencySymbol, tokenName')

fromChainPayoutDatum :: MarloweVersion v -> Chain.Datum -> Maybe (PayoutDatum v)
fromChainPayoutDatum = \case
  MarloweV1 -> \datum -> do
    (p, t) <- Chain.fromDatum datum
    let p' = Chain.PolicyId . Plutus.fromBuiltin . Plutus.unCurrencySymbol $ p
        t' = Chain.TokenName . Plutus.fromBuiltin . Plutus.unTokenName $ t
    pure $ Chain.AssetId p' t'

toChainDatum :: MarloweVersion v -> Datum v -> Chain.Datum
toChainDatum = \case
  MarloweV1 -> Chain.toDatum

fromChainDatum :: MarloweVersion v -> Chain.Datum -> Maybe (Datum v)
fromChainDatum = \case
  MarloweV1 -> Chain.fromDatum

-- * Orphan instances

instance Binary PV2.Address
instance Binary PV2.Credential
instance Binary PV2.CurrencySymbol
instance Binary PV2.DatumHash
instance Binary PV2.ExBudget
instance Binary PV2.ExCPU
instance Binary PV2.ExMemory
instance Binary PV2.POSIXTime
instance Binary PV2.PubKeyHash
instance Binary PV2.SatInt
instance Binary PV2.StakingCredential
instance Binary PV2.TokenName
instance Binary PV2.ScriptHash
instance Binary V1.Action
instance Binary V1.Bound
instance Binary V1.ChoiceId
instance Binary V1.Contract
instance Binary V1.Input
instance Binary V1.InputContent
instance Binary V1.IntervalError
instance Binary V1.MarloweData
instance Binary V1.MarloweParams
instance Binary V1.Observation
instance Binary V1.Party
instance Binary V1.Payee
instance Binary V1.Payment
instance Binary V1.SafetyError
instance Binary V1.State
instance Binary V1.Token
instance (Binary a) => Binary (V1.Transaction a)
instance Binary V1.TransactionError
instance Binary V1.TransactionInput
instance Binary V1.TransactionOutput
instance Binary V1.TransactionWarning
instance Binary V1.ValueId
instance (Binary a) => Binary (V1.Case a)
instance (Binary a) => Binary (V1.Value a)
instance Binary NetworkId where
  put = \case
    Mainnet -> putWord8 0
    Testnet (NetworkMagic n) -> putWord8 1 *> put n
  get = do
    tag <- getWord8
    case tag of
      0 -> pure Mainnet
      1 -> Testnet . NetworkMagic <$> get
      _ -> fail $ "Invalid network ID constructor tag " <> show tag

instance Variations PV2.Address
instance Variations PV2.Credential
instance Variations PV2.CurrencySymbol
instance Variations PV2.POSIXTime
instance Variations PV2.PubKeyHash
instance Variations PV2.StakingCredential
instance Variations PV2.TokenName
instance Variations PV2.ScriptHash
instance Variations V1.Action
instance Variations V1.Bound
instance Variations V1.ChoiceId
instance Variations V1.Input
instance Variations V1.InputContent
instance Variations V1.TransactionInput
instance Variations V1.MarloweData
instance Variations V1.MarloweParams
instance Variations V1.Payee
instance Variations V1.State
instance Variations V1.Token
instance Variations V1.ValueId
instance Variations V1.IntervalError
instance Variations NetworkId where
  variations = NE.fromList [Mainnet, Testnet $ NetworkMagic 0]

instance Variations V1.Party where
  variations = NE.fromList $ NE.filter (not . hasStakingPointer) $ to <$> gVariations

hasStakingPointer :: V1.Party -> Bool
hasStakingPointer = \case
  V1.Address _ (PV2.Address _ (Just PV2.StakingPtr{})) -> True
  _ -> False

instance Binary PV2.BuiltinByteString where
  put = put . PV2.fromBuiltin
  get = PV2.toBuiltin <$> get @ByteString

instance Variations PV2.BuiltinByteString where
  variations = PV2.toBuiltin <$> variations @ByteString

instance Binary PV2.BuiltinData where
  put = put . C.serialiseToCBOR . C.unsafeHashableScriptData . C.fromPlutusData . PV2.builtinDataToData . PV2.toBuiltinData
  get = do
    bytes <- get @ByteString
    case C.deserialiseFromCBOR (C.proxyToAsType (Proxy @C.HashableScriptData)) bytes of
      Left err -> fail $ "Failed to decode BuiltinData: " <> show err
      Right hashableScriptData -> pure $
        PV2.dataToBuiltinData . C.toPlutusData . C.getScriptData $ hashableScriptData

instance (Binary k, Binary v) => Binary (AM.Map k v)

instance (Variations k, Variations v) => Variations (AM.Map k v)

-- The following require manual instances to avoid infinite recursion.
instance Variations V1.Contract where
  variations =
    join $
      NE.fromList
        [ pure V1.Close
        , V1.Pay <$> variations `varyAp` variations `varyAp` variations `varyAp` variations <*> pure V1.Close
        , V1.If <$> variations <*> pure V1.Close <*> pure V1.Close
        , V1.When <$> variations `varyAp` variations <*> pure V1.Close
        , V1.Let <$> variations `varyAp` variations <*> pure V1.Close
        , V1.Assert <$> variations <*> pure V1.Close
        ]

instance Variations (V1.Case V1.Contract) where
  variations =
    join $
      NE.fromList
        [ V1.Case <$> variations <*> pure V1.Close
        , V1.MerkleizedCase <$> variations `varyAp` variations
        ]

instance Variations V1.Observation where
  variations =
    join $
      NE.fromList
        [ pure $ V1.AndObs V1.FalseObs V1.FalseObs
        , pure $ V1.OrObs V1.FalseObs V1.FalseObs
        , pure $ V1.NotObs V1.FalseObs
        , V1.ChoseSomething <$> variations
        , pure $ V1.ValueGE V1.TimeIntervalStart V1.TimeIntervalStart
        , pure $ V1.ValueLE V1.TimeIntervalStart V1.TimeIntervalStart
        , pure $ V1.ValueGT V1.TimeIntervalStart V1.TimeIntervalStart
        , pure $ V1.ValueLT V1.TimeIntervalStart V1.TimeIntervalStart
        , pure $ V1.ValueEQ V1.TimeIntervalStart V1.TimeIntervalStart
        , pure V1.FalseObs
        , pure V1.TrueObs
        ]

instance Variations (V1.Value V1.Observation) where
  variations =
    join $
      NE.fromList
        [ V1.AvailableMoney <$> variations `varyAp` variations
        , V1.Constant <$> variations
        , pure $ V1.NegValue (V1.Constant 1)
        , pure $ V1.AddValue (V1.Constant 1) (V1.Constant 1)
        , pure $ V1.SubValue (V1.Constant 1) (V1.Constant 1)
        , pure $ V1.MulValue (V1.Constant 1) (V1.Constant 1)
        , pure $ V1.DivValue (V1.Constant 1) (V1.Constant 1)
        , V1.ChoiceValue <$> variations
        , pure V1.TimeIntervalStart
        , pure V1.TimeIntervalEnd
        , V1.UseValue <$> variations
        , pure $ V1.Cond V1.FalseObs (V1.Constant 1) (V1.Constant 1)
        ]

-- | A contract with its adjacency and closure information.
data ContractWithAdjacency v = ContractWithAdjacency
  { contractHash :: ContractHash
  -- ^ The hash of the contract.
  , contract :: Contract v
  -- ^ The contract.
  , adjacency :: Set ContractHash
  -- ^ The set of continuation hashes explicitly contained in the contract.
  , closure :: Set ContractHash
  -- ^ The set of hashes contained in the contract and all recursive continuations of the contract.
  -- includes the hash of the contract itself.
  -- Does not contain the hash of the close contract.
  }
deriving instance (Show (Contract v)) => Show (ContractWithAdjacency v)
deriving instance (Eq (Contract v)) => Eq (ContractWithAdjacency v)
deriving instance (Ord (Contract v)) => Ord (ContractWithAdjacency v)
deriving instance Generic (ContractWithAdjacency v)


