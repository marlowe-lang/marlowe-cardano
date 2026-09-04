{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deriving-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{--
 TODO:
 I believe that this domain type layer requires a significant rewrite.
 We created a layer of types but we can not assume any correctness invariants
 for them. They store raw bytes etc. and transformation from and to cardano-api
 types is painful.
 * Make many of the types just wrappers around the cardano-api types.
 * This should allow us to actually have even stronger or domain specific invariants.
 * We can preserve our own instances for utility classes like. decoders/encoders (binary, json etc.) 
--}

module Language.Marlowe.Runtime.ChainSync.Api where

import Cardano.Api (
  AlonzoEraOnwards (..),
  AnyCardanoEra (..),
  BabbageEra,
  CardanoEra (..),
  ConwayEra,
  ScriptInEra (..),
  SerialiseAsRawBytes (..),
  deserialiseFromBech32,
  metadataValueToJsonNoSchema,
  serialiseToBech32,
 )
import qualified Cardano.Api as C
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Ledger as Coin
import qualified Cardano.Ledger.BaseTypes as Base
import qualified Cardano.Ledger.BaseTypes as C
import Cardano.Ledger.Hashes (SafeToHash (..))
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (encCBOR), decodeFullAnnotator, toBuilder)
import Cardano.Ledger.Credential (ptrCertIx, ptrSlotNo, ptrTxIx)
import Cardano.Ledger.Slot (EpochSize)
-- import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Monad (guard, join, when, (<=<), (>=>))
import Data.Aeson (
  FromJSON (..),
  FromJSONKey (..),
  FromJSONKeyFunction (FromJSONKeyText, FromJSONKeyTextParser),
  ToJSON,
  ToJSONKey,
  toJSON,
  (.:),
  (.=),
 )
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseFail, toJSONKeyText)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Bifunctor (Bifunctor (..), bimap)
import Data.Binary (Binary (..), Get, Put, get, getWord8, put, putWord8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16, decodeBase16Untyped)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
import Data.Functor (($>))
import Data.Group (Group (invert))
import Data.Hashable (Hashable)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.SOP.NonEmpty (NonEmpty (..))
import qualified Data.Scientific as Scientific
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime (..), diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDateValid, toOrdinalDate)
import Data.Traversable (for)
import Data.Type.Equality (type (:~:) (Refl))
import qualified Data.Vector as Vector
import Data.Void (Void)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Language.Marlowe.Runtime.Cardano.Feature (hush)
-- import Network.Protocol.ChainSeek.Client (ChainSeekClient (..), ChainSeekClientSelector)
-- import qualified Network.Protocol.ChainSeek.Client as ChainSeekClient
-- import Network.Protocol.ChainSeek.Server (
--   ChainSeekQueryOTelRendered (ChainSeekQueryOTelRendered),
--   ChainSeekServer (..),
--   ChainSeekServerSelector,
--   RenderChainSeekQueryOTel,
--  )
-- import qualified Network.Protocol.ChainSeek.Server as ChainSeekServer
-- import Network.Protocol.ChainSeek.Types
-- import qualified Network.Protocol.ChainSeek.Types as ChainSeek
-- import Network.Protocol.Codec.Spec
-- import Network.Protocol.Handshake.Types (HasSignature (..))
-- import qualified Network.Protocol.Job.Types as Job
-- import Network.Protocol.Query.Client (QueryClientSelector, renderQueryClientSelectorOTel)
-- import Network.Protocol.Query.Server (
--   QueryServerSelector,
--   RenderRequestOTel,
--   RequestRenderedOTel (..),
--   renderQueryServerSelectorOTel,
--  )
-- import Network.Protocol.Query.Types (OTelRequest (reqTypeName))
-- import qualified Network.Protocol.Query.Types as Query
-- import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel)
-- import OpenTelemetry.Attributes (Attribute, PrimitiveAttribute (..))
-- import OpenTelemetry.Trace.Core (toAttribute)
import Ouroboros.Consensus.Block (GenesisWindow)
import qualified Ouroboros.Consensus.Block as O
import Ouroboros.Consensus.Peras.Params (PerasRoundLength(PerasRoundLength))
import Ouroboros.Consensus.Block.SupportsPeras (PerasRoundNo(PerasRoundNo))
import Ouroboros.Consensus.BlockchainTime (RelativeTime, SlotLength (..))
import Ouroboros.Consensus.HardFork.History (
  Bound (..),
  EraEnd (..),
  EraParams (..),
  -- EraParamsFormat (EraParamsWithGenesisWindow),
  EraSummary (..),
  PerasEnabled,
  Interpreter,
  SafeZone (..),
  Summary (Summary),
  mkInterpreter,
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParamsForTesting)
import qualified PlutusLedgerApi.V1 as Plutus
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)

-- import Cardano.Api.ProtocolParameters (
--   ProtocolParameters,
--   convertToLedgerProtocolParameters,
--  )
-- import qualified Cardano.Api.Script as Cardano.Api.Shelley
-- import Cardano.Api.Shelley (LedgerProtocolParameters, ProtocolParameters (..))
import qualified Data.ByteString.Builder as BB

-- import Adapter.Cardano.Api.ProtocolParameters ()
import Data.Variations (Variations (variations), varyAp)
import Data.Base16.Types (extractBase16)

-- | Extends a type with a "Genesis" member.
data WithGenesis a = Genesis | At a
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- instance (HasSignature a) => HasSignature (WithGenesis a) where
--   signature _ = T.intercalate " " ["WithGenesis", signature $ Proxy @a]

-- | A point in the chain, identified by a slot number, block header hash, and
-- block number.
type ChainPoint = WithGenesis BlockHeader

-- | A block header, consisting of a slot number, a hash, and a block number.
data BlockHeader = BlockHeader
  { slotNo :: SlotNo
  -- ^ The slot number when this block was produced.
  , headerHash :: BlockHeaderHash
  -- ^ The hash of this block's header.
  , blockNo :: BlockNo
  -- ^ The ordinal number of this block.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

newtype ChainTip = ChainTip { blockHeader :: Maybe BlockHeader }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

chainTipFromChainPoint :: ChainPoint -> ChainTip
chainTipFromChainPoint = \case
  Genesis -> ChainTip Nothing
  At header -> ChainTip (Just header)

genesisChainTip :: ChainTip
genesisChainTip = ChainTip Nothing

-- Last block from the node which we know about.
newtype NodeTip = NodeTip ChainTip
  deriving newtype (Eq, Show, Binary, Variations)

genesisNodeTip :: NodeTip
genesisNodeTip = NodeTip genesisChainTip

instance ToJSON NodeTip where
  toJSON (NodeTip tip) = A.object
    [ "NodeTip" .= toJSON tip
    ]

-- Last block which contained Marlowe relevant transactions.
newtype MarloweTip = MarloweTip ChainTip
  deriving newtype (Eq, Show, Binary, Variations)

genesisLocalMarloweTip :: MarloweTip
genesisLocalMarloweTip = MarloweTip genesisChainTip

instance ToJSON MarloweTip where
  toJSON (MarloweTip tip) = A.object
    [ "MarloweTip" .= toJSON tip
    ]

-- Last block processed by the indexer.
newtype IndexerTip = IndexerTip ChainTip
  deriving newtype (Eq, Show, Binary, Variations)

instance ToJSON IndexerTip where
  toJSON (IndexerTip tip) = A.object
    [ "IndexerTip" .= toJSON tip
    ]

genesisIndexerTip :: IndexerTip
genesisIndexerTip = IndexerTip genesisChainTip


-- instance HasSignature BlockHeader where
--   signature _ = "BlockHeader"

isAfter :: SlotNo -> BlockHeader -> Bool
isAfter s BlockHeader{..} = slotNo > s

-- | A transaction body
data Transaction = Transaction
  { txId :: TxId
  -- ^ The hash of this transaction.
  , validityRange :: ValidityRange
  -- ^ The range of slots during which this transaction is valid.
  , metadata :: TransactionMetadata
  -- ^ The metadata of this transaction
  , inputs :: Map TxOutRef (Maybe Redeemer)
  -- ^ The inputs consumed by the transaction.
  -- The ordering on the ledger is lexicographical.
  , outputs :: [TransactionOutput]
  -- ^ The outputs produced by the transaction.
  , mintedTokens :: Tokens
  -- ^ Tokens minted by the transaction.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | A validity range for a transaction
data ValidityRange
  = -- | The transaction is always valid.
    Unbounded
  | -- | The transaction is only valid after a specific slot.
    MinBound SlotNo
  | -- | The transaction is only valid before a specific slot.
    MaxBound SlotNo
  | -- | The transaction is only valid between two slots.
    MinMaxBound SlotNo SlotNo
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- Encodes `transaction_metadatum`:
-- https://github.com/input-output-hk/cardano-ledger/blob/node/1.35.3/eras/shelley/test-suite/cddl-files/shelley.cddl#L203
data Metadata
  = MetadataMap [(Metadata, Metadata)]
  | MetadataList [Metadata]
  | MetadataNumber Integer
  | MetadataBytes ByteString
  | MetadataText Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

parseMetadataMap :: forall k v. (Ord k) => (Metadata -> Maybe k) -> (Metadata -> Maybe v) -> Metadata -> Maybe (Map k v)
parseMetadataMap activeKeyPattern activeValuePattern = \case
  MetadataMap entries ->
    Map.fromList <$> for entries \case
      (activeKeyPattern -> Just key, activeValuePattern -> Just value) -> Just (key, value)
      _ -> Nothing
  _ -> Nothing

parseMetadataList :: (Metadata -> Maybe a) -> Metadata -> Maybe [a]
parseMetadataList activeItemPattern = \case
  MetadataList xs -> for xs \case
    (activeItemPattern -> Just x) -> Just x
    _ -> Nothing
  _ -> Nothing

parseMetadataNumber :: Metadata -> Maybe Integer
parseMetadataNumber = \case MetadataNumber n -> Just n; _ -> Nothing

parseMetadataBytes :: Metadata -> Maybe ByteString
parseMetadataBytes = \case MetadataBytes bs -> Just bs; _ -> Nothing

parseMetadataText :: Metadata -> Maybe Text
parseMetadataText = \case MetadataText bs -> Just bs; _ -> Nothing

-- Using the generic implementation produces an infinite list.
instance Variations Metadata where
  variations =
    join $
      NE.fromList
        [ pure $ MetadataMap []
        , pure $ MetadataMap [NE.head variations]
        , pure $ MetadataList []
        , pure $ MetadataList [NE.head variations]
        , MetadataNumber <$> variations
        , MetadataBytes <$> variations
        , MetadataText <$> variations
        ]

instance ToJSON Metadata where
  toJSON = metadataValueToJsonNoSchema . toCardanoMetadata

-- TODO replace with metadataValueFromJsonNoSchema when it is exported!
instance FromJSON Metadata where
  parseJSON = go
    where
      go
        :: Aeson.Value
        -> Parser Metadata
      go Aeson.Null = fail "Null not allowed"
      go Aeson.Bool{} = fail "Booleans not allowed"
      go (Aeson.Number d) =
        case Scientific.floatingOrInteger d :: Either Double Integer of
          Left _ -> fail "not an integer"
          Right n -> pure $ MetadataNumber n
      go (Aeson.String s)
        | Just s' <- T.stripPrefix "0x" s
        , let bs' = T.encodeUtf8 s'
        , Right bs <- decodeBase16Untyped bs'
        , not (BSC.any (\c -> c >= 'A' && c <= 'F') bs') =
            pure $ MetadataBytes bs
      go (Aeson.String s) = pure $ MetadataText s
      go (Aeson.Array vs) = fmap MetadataList . traverse go $ Vector.toList vs
      go (Aeson.Object kvs) =
        fmap MetadataMap
          . traverse (\(k, v) -> (,) (parseKey k) <$> go v)
          . sortOn fst
          . fmap (first Key.toText)
          $ KeyMap.toList kvs

      parseKey :: Text -> Metadata
      parseKey s =
        fromMaybe (MetadataText s) $
          parseAll
            ( (MetadataNumber <$> pSigned <* Atto.endOfInput)
                <|> (MetadataBytes <$> pBytes <* Atto.endOfInput)
            )
            s

      parseAll :: Atto.Parser a -> Text -> Maybe a
      parseAll p =
        either (const Nothing) Just
          . Atto.parseOnly p
          . T.encodeUtf8

      pUnsigned :: Atto.Parser Integer
      pUnsigned = do
        bs <- Atto.takeWhile1 Atto.isDigit
        -- no redundant leading 0s allowed, or we cannot round-trip properly
        guard (not (BS.length bs > 1 && BSC.head bs == '0'))
        return $! BS.foldl' step 0 bs
        where
          step a w = a * 10 + fromIntegral (w - 48)

      pSigned :: Atto.Parser Integer
      pSigned = Atto.signed pUnsigned

      pBytes :: Atto.Parser ByteString
      pBytes = do
        _ <- Atto.string "0x"
        remaining <- Atto.takeByteString
        when (BSC.any hexUpper remaining) $ fail ("Unexpected uppercase hex characters in " <> show remaining)
        case decodeBase16Untyped remaining of
          Right bs -> return bs
          _ -> fail ("Expecting base16 encoded string, found: " <> show remaining)
        where
          hexUpper c = c >= 'A' && c <= 'F'

toCardanoMetadata :: Metadata -> C.TxMetadataValue
toCardanoMetadata = \case
  MetadataMap ms -> C.TxMetaMap $ bimap toCardanoMetadata toCardanoMetadata <$> ms
  MetadataList ds -> C.TxMetaList $ toCardanoMetadata <$> ds
  MetadataNumber i -> C.TxMetaNumber i
  MetadataBytes bs -> C.TxMetaBytes bs
  MetadataText bs -> C.TxMetaText bs

fromCardanoMetadata :: C.TxMetadataValue -> Metadata
fromCardanoMetadata = \case
  C.TxMetaMap ms -> MetadataMap $ bimap fromCardanoMetadata fromCardanoMetadata <$> ms
  C.TxMetaList ds -> MetadataList $ fromCardanoMetadata <$> ds
  C.TxMetaNumber i -> MetadataNumber i
  C.TxMetaBytes bs -> MetadataBytes bs
  C.TxMetaText bs -> MetadataText bs

-- Handle convenient `JSON` based encoding for a subset of `Metadata`
-- type domain.
-- It is not an implementation of a possible `fromJSON` method.
fromJSONEncodedMetadata :: A.Value -> Maybe Metadata
fromJSONEncodedMetadata = \case
  A.Number n ->
    guard (fromInteger (floor n) == n) $> MetadataNumber (floor n)
  A.String t -> Just $ MetadataText t
  A.Array (Vector.toList -> elems) -> MetadataList <$> traverse fromJSONEncodedMetadata elems
  A.Object (Map.toList . KeyMap.toMapText -> props) ->
    MetadataMap <$> for props \(key, value) -> do
      value' <- fromJSONEncodedMetadata value
      pure (MetadataText key, value')
  A.Bool _ -> Nothing
  A.Null -> Nothing

toJSONEncodedMetadata :: Metadata -> Maybe A.Value
toJSONEncodedMetadata = \case
  MetadataMap ms ->
    A.Object . KeyMap.fromList <$> for ms \case
      (MetadataText k, v) -> do
        let k' = Key.fromText k
        v' <- toJSONEncodedMetadata v
        pure (k', v')
      _ -> Nothing
  MetadataList ds ->
    A.Array . Vector.fromList <$> for ds \e -> do
      toJSONEncodedMetadata e
  MetadataNumber i -> Just $ A.Number $ fromInteger i
  MetadataBytes _ -> Nothing
  MetadataText bs -> Just $ A.String bs

-- Encodes `transaction_metadata`:
-- https://github.com/input-output-hk/cardano-ledger/blob/node/1.35.3/eras/shelley/test-suite/cddl-files/shelley.cddl#L212
newtype TransactionMetadata = TransactionMetadata {unTransactionMetadata :: Map Word64 Metadata}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Binary, ToJSON, Variations)

fromJSONEncodedTransactionMetadata :: A.Value -> Maybe TransactionMetadata
fromJSONEncodedTransactionMetadata = \case
  A.Object (Map.toList . KeyMap.toMapText -> props) ->
    TransactionMetadata . Map.fromList <$> for props \(key, value) -> do
      lbl <- fmap fromInteger $ readMaybe . T.unpack $ key
      value' <- fromJSONEncodedMetadata value
      pure (lbl, value')
  _ -> Nothing

toCardanoTxMetadata :: TransactionMetadata -> C.TxMetadata
toCardanoTxMetadata (TransactionMetadata metadata) = C.TxMetadata $ toCardanoMetadata <$> metadata

fromCardanoTxMetadata :: C.TxMetadata -> TransactionMetadata
fromCardanoTxMetadata (C.TxMetadata metadata) = TransactionMetadata $ fromCardanoMetadata <$> metadata

-- It is a bit enigmatic why;

-- | An input of a transaction.
data TransactionInput = TransactionInput
  { txId :: TxId
  -- ^ The txId of the TransactionOutput this input consumes.
  , txIx :: TxIx
  -- ^ The txIx of the TransactionOutput this input consumes.
  , address :: Address
  -- ^ The address of the TransactionOutput this input consumes.
  , datumBytes :: Maybe Datum
  -- ^ The script datum for this input
  , redeemer :: Maybe Redeemer
  -- ^ The script redeemer datum for this input (if one was provided).
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | An output of a transaction.
data TransactionOutput = TransactionOutput
  { address :: Address
  -- ^ The address that receives the assets of this output.
  , assets :: TxOutAssets
  -- ^ The assets this output produces.
  , datumHash :: Maybe DatumHash
  -- ^ FIXME: I'm guessing - The hash of the script non-inlined datum associated with this output.
  , datum :: Maybe Datum
  -- ^ FIXME: I'm guessing - The script inlined-datum associated with this output.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Variations)

-- | A script datum that is used to spend the output of a script tx.
newtype Redeemer = Redeemer {unRedeemer :: Datum}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Variations)

-- | A datum as a sum-of-products.
data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

-- Using the generic implementation produces an infinite list.
instance Variations Datum where
  variations =
    join $
      NE.fromList
        [ pure $ Constr 1 []
        , pure $ Constr 1 [NE.head variations]
        , pure $ Map []
        , pure $ Map [NE.head variations]
        , pure $ List []
        , pure $ List [NE.head variations]
        , I <$> variations
        , B <$> variations
        ]

instance ToJSON Datum where
  toJSON = \case
    B bs -> toJSON (extractBase16 . encodeBase16 $ bs)
    I i -> toJSON i
    List elems ->
      A.object
        [ ("tag", "list")
        , ("value", toJSON . map toJSON $ elems)
        ]
    Constr i attrs ->
      A.object
        [ ("tag", "constr")
        , ("idx", toJSON i)
        , ("value", toJSON . map toJSON $ attrs)
        ]
    Map props ->
      A.object
        [ ("tag", "map")
        , ("value", toJSON . map toJSON $ props)
        ]

instance FromJSON Datum where
  parseJSON = \case
    Aeson.String s -> do
      Base16 bs <- parseJSON $ Aeson.String s
      pure $ B bs
    Aeson.Number n ->
      case Scientific.floatingOrInteger n :: Either Double Integer of
        Left _ -> fail "not an integer"
        Right i -> pure $ I i
    Aeson.Object props -> do
      (tag :: Text) <- props .: "tag"
      case tag of
        "map" -> Map <$> props .: "value"
        "list" -> List <$> props .: "value"
        "constr" -> Constr <$> props .: "idx" <*> props .: "value"
        _ -> fail $ "unknown tag: " <> show tag
    Aeson.Bool _ -> fail "Booleans not allowed"
    Aeson.Null -> fail "Null not allowed"
    Aeson.Array _ -> fail "Arrays not allowed"

fromDatum :: (Plutus.FromData a) => Datum -> Maybe a
fromDatum = Plutus.fromData . toPlutusData

toDatum :: (Plutus.ToData a) => a -> Datum
toDatum = fromPlutusData . Plutus.toData

fromRedeemer :: (Plutus.FromData a) => Redeemer -> Maybe a
fromRedeemer = fromDatum . unRedeemer

toRedeemer :: (Plutus.ToData a) => a -> Redeemer
toRedeemer = Redeemer . toDatum

-- | Convert from PlutusLedgerApi.V1.Data to Datum
fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i ds) = Constr i $ fromPlutusData <$> ds
fromPlutusData (Plutus.Map m) = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List ds) = List $ fromPlutusData <$> ds
fromPlutusData (Plutus.I i) = I i
fromPlutusData (Plutus.B b) = B b

-- | Convert to PlutusLedgerApi.V1.Data to Datum
toPlutusData :: Datum -> Plutus.Data
toPlutusData (Constr i ds) = Plutus.Constr i $ toPlutusData <$> ds
toPlutusData (Map m) = Plutus.Map $ bimap toPlutusData toPlutusData <$> m
toPlutusData (List ds) = Plutus.List $ toPlutusData <$> ds
toPlutusData (I i) = Plutus.I i
toPlutusData (B b) = Plutus.B b

-- | A collection of assets transferred by a transaction output.
data Assets = Assets
  { ada :: Lovelace
  -- ^ The ADA sent by the tx output.
  , tokens :: Tokens
  -- ^ Additional tokens sent by the tx output.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Let's make the instance explicit so we can assume "some" semantics.
instance Ord Assets where
  compare (Assets a1 t1) (Assets a2 t2) = compare (a1, t1) (a2, t2)

instance Semigroup Assets where
  (Assets a1 t1) <> (Assets a2 t2) =
    Assets
      { ada = a1 <> a2
      , tokens = t1 <> t2
      }

instance Monoid Assets where
  mempty = Assets mempty mempty

instance Group Assets where
  invert (Assets a t) = Assets (invert a) (invert t)

-- We protect basic invariant which is that lovelace and tokens are non negative.
-- More advanced ledger invariants (min ADA) are not checked here.
newtype TxOutAssets = TxOutAssets {unTxOutAssets :: Assets}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup TxOutAssets where
  (TxOutAssets a1) <> (TxOutAssets a2) = TxOutAssets $ a1 <> a2

-- We provide an mempty value for `TxOutAssets` because we want to
-- allow creation of outputs which we adjust for min ADA later.
instance Monoid TxOutAssets where
  mempty = TxOutAssets mempty

{-# COMPLETE TxOutAssetsContent #-}
pattern TxOutAssetsContent :: Assets -> TxOutAssets
pattern TxOutAssetsContent assets <- TxOutAssets assets

-- | Check if lovelace and tokens are non negative.
mkTxOutAssets :: Assets -> Maybe TxOutAssets
mkTxOutAssets assets@Assets{ada, tokens} = do
  guard $ ada >= mempty
  -- In the map we expect all quantities to be positive.
  guard $ all (> mempty) $ unTokens tokens
  pure $ TxOutAssets assets

unsafeTxOutAssets :: Assets -> TxOutAssets
unsafeTxOutAssets = TxOutAssets

subtractTxOutAssets :: TxOutAssets -> TxOutAssets -> Maybe TxOutAssets
subtractTxOutAssets (TxOutAssets a1) (TxOutAssets a2) = mkTxOutAssets $ a1 <> invert a2

subtractTxOutAssetsRounding :: TxOutAssets -> TxOutAssets -> TxOutAssets
subtractTxOutAssetsRounding (TxOutAssets a1) (TxOutAssets a2) = do
  let Assets lovelace tokens = a1 <> invert a2
  -- Now we should go and remove everything which is non positive
  TxOutAssets $ Assets (max mempty lovelace) (Tokens $ Map.filter (> mempty) $ unTokens tokens)

-- | A collection of token quantities by their asset ID.
newtype Tokens = Tokens {unTokens :: Map AssetId Quantity}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup Tokens where
  (<>) = fmap Tokens . on (Map.unionWith (<>)) unTokens

instance Monoid Tokens where
  mempty = Tokens mempty

instance Group Tokens where
  invert (Tokens m) = Tokens $ Map.map invert m

-- | A newtype wrapper for parsing base 16 strings as byte strings.
newtype Base16 = Base16 {unBase16 :: ByteString}

instance Show Base16 where
  show = show . encodeBase16 . unBase16

instance IsString Base16 where
  fromString = either (error . T.unpack) Base16 . decodeBase16Untyped . encodeUtf8 . T.pack

instance FromJSON Base16 where
  parseJSON = either (parseFail . T.unpack) (pure . Base16) . decodeBase16Untyped . encodeUtf8 <=< parseJSON

instance FromJSONKey Base16 where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) (pure . Base16) . decodeBase16Untyped . encodeUtf8

instance ToJSON Base16 where
  toJSON = toJSON . extractBase16 . encodeBase16 . unBase16

instance ToJSONKey Base16 where
  toJSONKey = toJSONKeyText $ extractBase16 . encodeBase16 . unBase16

-- 32 bytes of sha256 hash of the contract datum.
newtype DatumHash = DatumHash {unDatumHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable, NFData)
  deriving (IsString, Show, ToJSON) via Base16

fromByteString :: ByteString -> Maybe DatumHash
fromByteString = \case
  bs | BS.length bs == 32 -> Just $ DatumHash bs
  _ -> Nothing

instance FromJSON DatumHash where
  parseJSON json = do
    Base16 bytes <- parseJSON json
    case fromByteString bytes of
      Just dh -> pure dh
      Nothing -> fail $ "DatumHash must be 32 bytes, but got: " <> show (BS.length bytes)

newtype TxId = TxId {unTxId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON, ToJSONKey) via Base16

instance FromJSON TxId where
  parseJSON = Aeson.withText "TxOutRef" $ pure . fromString . T.unpack

newtype TxIx = TxIx {unTxIx :: Word16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Bounded, Enum, ToJSON, ToJSONKey, Variations, Hashable)

newtype CertIx = CertIx {unCertIx :: Word16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Bounded, Enum, Variations, Hashable)

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: TxIx
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, Variations, Hashable)

instance IsString TxOutRef where
  fromString = fromJust . parseTxOutRef . T.pack

instance FromJSON TxOutRef where
  parseJSON = Aeson.withText "TxOutRef" $ pure . fromString . T.unpack

{- HLINT ignore "Functor law" -}
parseTxOutRef :: Text -> Maybe TxOutRef
parseTxOutRef val = case T.splitOn "#" val of
  [txId, txIx] ->
    TxOutRef
      <$> (TxId <$> either (const Nothing) Just (decodeBase16Untyped . encodeUtf8 $ txId))
      <*> (TxIx <$> readMaybe (T.unpack txIx))
  _ -> Nothing

renderTxOutRef :: TxOutRef -> Text
renderTxOutRef TxOutRef{..} =
  mconcat
    [ extractBase16 . encodeBase16 $ unTxId txId
    , "#"
    , T.pack $ show $ unTxIx txIx
    ]

txOutRefFromTransactionInput :: TransactionInput -> TxOutRef
txOutRefFromTransactionInput input = TxOutRef input.txId input.txIx

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations, Hashable)

newtype BlockNo = BlockNo {unBlockNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Bounded, Binary, ToJSON, Variations, Hashable)

newtype BlockHeaderHash = BlockHeaderHash {unBlockHeaderHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

data AssetId = AssetId
  { policyId :: PolicyId
  , tokenName :: TokenName
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, ToJSONKey, FromJSON, Variations, Hashable)

newtype PolicyId = PolicyId {unPolicyId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via Base16

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Binary, Variations, Hashable)

instance ToJSONKey TokenName where
  toJSONKey = toJSONKeyText $ T.pack . BSC.unpack . unTokenName

instance ToJSON TokenName where
  toJSON = Aeson.String . T.pack . BSC.unpack . unTokenName

instance FromJSON TokenName where
  parseJSON = Aeson.withText "TokenName" (pure . TokenName . BSC.pack . T.unpack)

instance FromJSONKey TokenName where
  fromJSONKey = FromJSONKeyText (TokenName . BSC.pack . T.unpack)

newtype Quantity = Quantity {unQuantity :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup Quantity where
  (Quantity q1) <> (Quantity q2) = Quantity $ q1 + q2

instance Monoid Quantity where
  mempty = Quantity 0

instance Group Quantity where
  invert = Quantity . negate . unQuantity

newtype Lovelace = Lovelace {unLovelace :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, ToJSON, Variations)

instance Semigroup Lovelace where
  (Lovelace l1) <> (Lovelace l2) = Lovelace $ l1 + l2

instance Monoid Lovelace where
  mempty = Lovelace 0

instance Group Lovelace where
  invert = Lovelace . negate . unLovelace

newtype Address = Address {unAddress :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON, FromJSON, ToJSONKey) via Base16

toBech32 :: Address -> Maybe Text
toBech32 =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley address ->
      Just $ serialiseToBech32 address
    _ -> Nothing

fromBech32 :: Text -> Maybe Address
fromBech32 =
  fmap (Address . serialiseToRawBytes)
    . either (const Nothing) Just
    . deserialiseFromBech32 @(Cardano.Address Cardano.ShelleyAddr)

fromCardanoShelleyAddress :: Cardano.Address Cardano.ShelleyAddr -> Address
fromCardanoShelleyAddress = Address . serialiseToRawBytes

fromCardanoAddressAny :: Cardano.AddressAny -> Maybe Address
fromCardanoAddressAny = \case
  Cardano.AddressShelley shelleyAddr -> Just $ fromCardanoShelleyAddress shelleyAddr
  _ -> Nothing

toCardanoAddressAny :: Address -> Maybe Cardano.AddressAny
toCardanoAddressAny = hush . Cardano.deserialiseFromRawBytes Cardano.AsAddressAny . unAddress

paymentCredential :: Address -> Maybe Credential
paymentCredential =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley (Cardano.ShelleyAddress _ credential _) ->
      Just $ fromCardanoPaymentCredential $ Cardano.fromShelleyPaymentCredential credential
    _ -> Nothing

stakeReference :: Address -> Maybe StakeReference
stakeReference =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley (Cardano.ShelleyAddress _ _ ref) ->
      fromCardanoStakeAddressReference $ Cardano.fromShelleyStakeReference ref
    _ -> Nothing

data Credential
  = PaymentKeyCredential PaymentKeyHash
  | ScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations, Hashable)

data StakeCredential
  = StakeKeyCredential StakeKeyHash
  | StakeScriptCredential ScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations, Hashable)

fromCardanoPaymentCredential :: Cardano.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  Cardano.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  Cardano.PaymentCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

newtype PaymentKeyHash = PaymentKeyHash {unPaymentKeyHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

newtype StakeKeyHash = StakeKeyHash {unStakeKeyHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Variations, Hashable)
  deriving (IsString, Show, ToJSON) via Base16

fromCardanoPaymentKeyHash :: Cardano.Hash Cardano.PaymentKey -> PaymentKeyHash
fromCardanoPaymentKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

fromCardanoStakeKeyHash :: Cardano.Hash Cardano.StakeKey -> StakeKeyHash
fromCardanoStakeKeyHash = StakeKeyHash . Cardano.serialiseToRawBytes

newtype ScriptHash = ScriptHash {unScriptHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable)
  deriving (IsString, Show, FromJSON, FromJSONKey, ToJSON, ToJSONKey) via Base16
  deriving anyclass (Binary, Variations)

policyIdToScriptHash :: PolicyId -> ScriptHash
policyIdToScriptHash (PolicyId h) = ScriptHash h

scriptHashToPolicyId :: ScriptHash -> PolicyId
scriptHashToPolicyId (ScriptHash h) = PolicyId h

fromCardanoScriptHash :: Cardano.ScriptHash -> ScriptHash
fromCardanoScriptHash = ScriptHash . Cardano.serialiseToRawBytes

newtype PlutusScript = PlutusScript {unPlutusScript :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving (IsString, Show, ToJSON) via Base16
  deriving anyclass (Binary, Variations)

data StakeReference
  = StakeCredential StakeCredential
  | StakePointer SlotNo TxIx CertIx
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations, Hashable)

fromCardanoStakeAddressReference :: Cardano.StakeAddressReference -> Maybe StakeReference
fromCardanoStakeAddressReference = \case
  Cardano.NoStakeAddress -> Nothing
  Cardano.StakeAddressByValue credential -> Just $ StakeCredential $ fromCardanoStakeCredential credential
  Cardano.StakeAddressByPointer (Cardano.StakeAddressPointer ptr) ->
    Just $
      StakePointer
        (SlotNo $ Cardano.unSlotNo $ ptrSlotNo ptr)
        (let Base.TxIx txIx = ptrTxIx ptr in TxIx txIx)
        (let Base.CertIx certIx = ptrCertIx ptr in CertIx certIx)

fromCardanoStakeCredential :: Cardano.StakeCredential -> StakeCredential
fromCardanoStakeCredential = \case
  Cardano.StakeCredentialByKey skh -> StakeKeyCredential $ fromCardanoStakeKeyHash skh
  Cardano.StakeCredentialByScript scriptHash -> StakeScriptCredential $ fromCardanoScriptHash scriptHash

-- | Reasons a 'FindTx' request can be rejected.
data TxError
  = TxNotFound
  | TxInPast BlockHeader
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons a 'FindTxsTo' request can be rejected.
data FindTxsToError
  = NoAddresses
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons a 'FindConsumingTx' request can be rejected.
data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | Reasons an 'Intersect' request can be rejected.
data IntersectError = IntersectionNotFound
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- | The 'query' type for the Marlowe Chain Sync.
data Move err result where
  -- | Advance a fixed number of blocks without collecting any results..
  AdvanceBlocks :: Natural -> Move Void ()
  -- | Jump to the latest intersection from a list of known block headers
  -- without collecting any results.
  Intersect :: [BlockHeader] -> Move IntersectError ()
  -- | Advance to the block when a tx out is consumed and collect the tx that
  -- consumes the tx out.
  FindConsumingTxs :: Set TxOutRef -> Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)
  -- | Advance to the block containing a transaction. The boolean flag
  -- indicates whether or not to wait for the transaction to be produced.
  FindTx :: TxId -> Bool -> Move TxError Transaction
  -- | Advance to the block containing transactions that send or consume outputs
  -- to any addresses with the requested credentials.
  FindTxsFor :: NESet Credential -> Move Void (Set Transaction)
  -- | Advances to the tip block. Waits if already at the tip.
  AdvanceToTip :: Move Void ()

-- instance HasSignature Move where
--   signature _ = "Move"

deriving instance Show (Move err result)
deriving instance Eq (Move err result)
deriving instance Ord (Move err result)

instance Binary UTCTime where
  put UTCTime{..} = do
    let (year, dayOfYear) = toOrdinalDate utctDay
    put year
    put dayOfYear
    put $ diffTimeToPicoseconds utctDayTime

  get = do
    year <- get
    dayOfYear <- get
    utctDayTime <- picosecondsToDiffTime <$> get
    utctDay <- case fromOrdinalDateValid year dayOfYear of
      Nothing -> fail "Invalid ISO 8601 ordinal date"
      Just a -> pure a
    pure UTCTime{..}

data GetUTxOsQuery
  = GetUTxOsAtAddresses (Set Address)
  | GetUTxOsForTxOutRefs (Set TxOutRef)
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

-- Semigroup and Monoid seem to be safe - we cover here a subset of a partial function.
newtype UTxOs = UTxOs { utxosMap :: Map TxOutRef TransactionOutput }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, Variations)
  deriving anyclass (Binary, ToJSON)

lookupUTxO :: TxOutRef -> UTxOs -> Maybe TransactionOutput
lookupUTxO txOutRef (UTxOs utxos) = Map.lookup txOutRef utxos

toUTxOsList :: UTxOs -> [UTxO]
toUTxOsList (UTxOs (Map.toList -> utxos)) = fmap (uncurry UTxO) utxos

fromUTxOsList :: [UTxO] -> UTxOs
fromUTxOsList = UTxOs . Map.fromList . fmap toUTxOTuple

data UTxO = UTxO
  { txOutRef :: TxOutRef
  , transactionOutput :: TransactionOutput
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

toUTxOTuple :: UTxO -> (TxOutRef, TransactionOutput)
toUTxOTuple (UTxO txOutRef transactionOutput) = (txOutRef, transactionOutput)

-- instance ChainSeek.QueryVariations Move where
--   tags =
--     NE.fromList
--       [ ChainSeek.SomeTag TagAdvanceBlocks
--       , ChainSeek.SomeTag TagIntersect
--       , ChainSeek.SomeTag TagFindConsumingTxs
--       , ChainSeek.SomeTag TagFindTx
--       , ChainSeek.SomeTag TagFindTxsFor
--       , ChainSeek.SomeTag TagAdvanceToTip
--       ]
--   queryVariations = \case
--     TagAdvanceBlocks -> AdvanceBlocks <$> variations
--     TagIntersect -> Intersect <$> variations
--     TagFindConsumingTxs -> FindConsumingTxs <$> variations
--     TagFindTx -> FindTx <$> variations `varyAp` variations
--     TagFindTxsFor -> FindTxsFor <$> variations
--     TagAdvanceToTip -> pure AdvanceToTip
--   errVariations = \case
--     TagAdvanceBlocks -> []
--     TagIntersect -> NE.toList variations
--     TagFindConsumingTxs -> NE.toList variations
--     TagFindTx -> NE.toList variations
--     TagFindTxsFor -> []
--     TagAdvanceToTip -> []
--   resultVariations = \case
--     TagAdvanceBlocks -> variations
--     TagIntersect -> variations
--     TagFindConsumingTxs -> variations
--     TagFindTx -> variations
--     TagFindTxsFor -> variations
--     TagAdvanceToTip -> variations
-- 
-- type RuntimeChainSeek = ChainSeek Move ChainPoint ChainPoint
-- 
-- type RuntimeChainSeekClient = ChainSeekClient Move ChainPoint ChainPoint
-- 
-- type RuntimeChainSeekClientSelector = ChainSeekClientSelector Move ChainPoint ChainPoint
-- 
-- type RuntimeChainSeekServer = ChainSeekServer Move ChainPoint ChainPoint
-- 
-- type RuntimeChainSeekServerSelector = ChainSeekServerSelector Move ChainPoint ChainPoint
-- 
-- instance Query Move where
--   data Tag Move err result where
--     TagAdvanceBlocks :: Tag Move Void ()
--     TagIntersect :: Tag Move IntersectError ()
--     TagFindTx :: Tag Move TxError Transaction
--     TagFindConsumingTxs :: Tag Move (Map TxOutRef UTxOError) (Map TxOutRef Transaction)
--     TagFindTxsFor :: Tag Move Void (Set Transaction)
--     TagAdvanceToTip :: Tag Move Void ()
-- 
--   tagFromQuery = \case
--     AdvanceBlocks _ -> TagAdvanceBlocks
--     Intersect _ -> TagIntersect
--     FindTx _ _ -> TagFindTx
--     FindConsumingTxs _ -> TagFindConsumingTxs
--     FindTxsFor _ -> TagFindTxsFor
--     AdvanceToTip -> TagAdvanceToTip
-- 
--   tagEq = curry \case
--     (TagAdvanceBlocks, TagAdvanceBlocks) -> Just (Refl, Refl)
--     -- Please don't refactor this to use a single catch-all wildcard pattern.
--     -- The idea of doing it this way is to cause an incomplete pattern match
--     -- warning when a new 'Tag' constructor is added.
--     (TagAdvanceBlocks, _) -> Nothing
--     (TagIntersect, TagIntersect) -> Just (Refl, Refl)
--     (TagIntersect, _) -> Nothing
--     (TagFindTx, TagFindTx) -> Just (Refl, Refl)
--     (TagFindTx, _) -> Nothing
--     (TagFindConsumingTxs, TagFindConsumingTxs) -> Just (Refl, Refl)
--     (TagFindConsumingTxs, _) -> Nothing
--     (TagFindTxsFor, TagFindTxsFor) -> Just (Refl, Refl)
--     (TagFindTxsFor, _) -> Nothing
--     (TagAdvanceToTip, TagAdvanceToTip) -> Just (Refl, Refl)
--     (TagAdvanceToTip, _) -> Nothing
-- 
--   putTag = \case
--     TagAdvanceBlocks -> putWord8 0x01
--     TagIntersect -> putWord8 0x02
--     TagFindTx -> putWord8 0x03
--     TagFindConsumingTxs -> putWord8 0x04
--     TagAdvanceToTip -> putWord8 0x05
--     TagFindTxsFor -> putWord8 0x06
-- 
--   putQuery = \case
--     AdvanceBlocks blocks -> put blocks
--     Intersect points -> put points
--     FindTx txId wait -> put txId *> put wait
--     FindConsumingTxs utxos -> put utxos
--     AdvanceToTip -> mempty
--     FindTxsFor credentials -> put $ NESet.toList credentials
-- 
--   getTag = do
--     tag <- getWord8
--     case tag of
--       0x01 -> pure $ SomeTag TagAdvanceBlocks
--       0x02 -> pure $ SomeTag TagIntersect
--       0x03 -> pure $ SomeTag TagFindTx
--       0x04 -> pure $ SomeTag TagFindConsumingTxs
--       0x05 -> pure $ SomeTag TagAdvanceToTip
--       0x06 -> pure $ SomeTag TagFindTxsFor
--       _ -> fail $ "Invalid move tag " <> show tag
-- 
--   getQuery = \case
--     TagAdvanceBlocks -> AdvanceBlocks <$> get
--     TagIntersect -> Intersect <$> get
--     TagFindTx -> FindTx <$> get <*> get
--     TagFindConsumingTxs -> FindConsumingTxs <$> get
--     TagAdvanceToTip -> pure AdvanceToTip
--     TagFindTxsFor -> FindTxsFor . NESet.fromList <$> get
-- 
--   putResult = \case
--     TagAdvanceBlocks -> mempty
--     TagFindTx -> put
--     TagIntersect -> mempty
--     TagFindConsumingTxs -> put
--     TagAdvanceToTip -> mempty
--     TagFindTxsFor -> put
-- 
--   getResult = \case
--     TagAdvanceBlocks -> pure ()
--     TagFindTx -> get
--     TagIntersect -> pure ()
--     TagFindConsumingTxs -> get
--     TagAdvanceToTip -> pure ()
--     TagFindTxsFor -> get
-- 
--   putErr = \case
--     TagAdvanceBlocks -> put
--     TagFindTx -> put
--     TagIntersect -> put
--     TagFindConsumingTxs -> put
--     TagFindTxsFor -> put
--     TagAdvanceToTip -> put
-- 
--   getErr = \case
--     TagAdvanceBlocks -> get
--     TagFindTx -> get
--     TagIntersect -> get
--     TagFindConsumingTxs -> get
--     TagFindTxsFor -> get
--     TagAdvanceToTip -> get
-- 
-- instance ChainSeek.ShowQuery Move where
--   showsPrecTag _ =
--     showString . \case
--       TagAdvanceBlocks -> "TagAdvanceBlocks"
--       TagIntersect -> "TagIntersect"
--       TagFindConsumingTxs -> "TagFindConsumingTxs"
--       TagFindTx -> "TagFindTx"
--       TagFindTxsFor -> "TagFindTxsFor"
--       TagAdvanceToTip -> "TagAdvanceToTip"
-- 
--   showsPrecQuery p = \case
--     AdvanceBlocks blocks ->
--       showParen
--         (p >= 11)
--         ( showString "AdvanceBlocks"
--             . showSpace
--             . showsPrec 11 blocks
--         )
--     Intersect blocks ->
--       showParen
--         (p >= 11)
--         ( showString "Intersect"
--             . showSpace
--             . showsPrec 11 blocks
--         )
--     FindConsumingTxs txOuts ->
--       showParen
--         (p >= 11)
--         ( showString "FindConsumingTxs"
--             . showSpace
--             . showsPrec 11 txOuts
--         )
--     FindTx wait txId ->
--       showParen
--         (p >= 11)
--         ( showString "FindTx"
--             . showSpace
--             . showsPrec 11 wait
--             . showSpace
--             . showsPrec 11 txId
--         )
--     FindTxsFor credentials ->
--       showParen
--         (p >= 11)
--         ( showString "FindTxsFor"
--             . showSpace
--             . showsPrec 11 credentials
--         )
--     AdvanceToTip -> showString "AdvanceToTip"
-- 
--   showsPrecErr p = \case
--     TagAdvanceBlocks -> showsPrec p
--     TagIntersect -> showsPrec p
--     TagFindConsumingTxs -> showsPrec p
--     TagFindTx -> showsPrec p
--     TagFindTxsFor -> showsPrec p
--     TagAdvanceToTip -> showsPrec p
-- 
--   showsPrecResult p = \case
--     TagAdvanceBlocks -> showsPrec p
--     TagIntersect -> showsPrec p
--     TagFindConsumingTxs -> showsPrec p
--     TagFindTx -> showsPrec p
--     TagFindTxsFor -> showsPrec p
--     TagAdvanceToTip -> showsPrec p
-- 
-- data ChainSyncQuery a where
--   GetSecurityParameter :: ChainSyncQuery Int
--   GetNetworkId :: ChainSyncQuery NetworkId
--   GetProtocolParameters :: BabbageEraOnwards era -> ChainSyncQuery (Cardano.Api.Shelley.LedgerProtocolParameters era)
--   GetSystemStart :: ChainSyncQuery SystemStart
--   GetEraHistory :: ChainSyncQuery EraHistory
--   GetUTxOs :: GetUTxOsQuery -> ChainSyncQuery UTxOs
--   GetNodeTip :: ChainSyncQuery ChainPoint
--   GetTip :: ChainSyncQuery ChainPoint
--   GetEra :: ChainSyncQuery AnyCardanoEra
--   GetScripts :: BabbageEraOnwards era -> Set ScriptHash -> ChainSyncQuery (Map ScriptHash (ScriptInEra era))
-- 
-- type ChainSyncQueryClientSelector = QueryClientSelector ChainSyncQuery
-- 
-- type ChainSyncQueryServerSelector = QueryServerSelector ChainSyncQuery
-- 
-- renderChainSyncQueryClientSelector :: RenderSelectorOTel ChainSyncQueryClientSelector
-- renderChainSyncQueryClientSelector = renderQueryClientSelectorOTel renderChainSyncQueryOTel
-- 
-- renderChainSyncQueryServerSelector :: RenderSelectorOTel ChainSyncQueryServerSelector
-- renderChainSyncQueryServerSelector = renderQueryServerSelectorOTel renderChainSyncQueryOTel
-- 
-- renderChainSyncQueryOTel :: RenderRequestOTel ChainSyncQuery
-- renderChainSyncQueryOTel = \case
--   GetSecurityParameter ->
--     RequestRenderedOTel
--       { requestName = "get-security-parameter"
--       , requestAttributes = []
--       , responseAttributes = \k -> [("security-parameter", toAttribute k)]
--       }
--   GetNetworkId ->
--     RequestRenderedOTel
--       { requestName = "get-network-id"
--       , requestAttributes = []
--       , responseAttributes = \case
--           Mainnet -> [("network-kind", "mainnet")]
--           Testnet (NetworkMagic i) ->
--             [ ("network-kind", "testnet")
--             , ("network-magic", toAttribute $ IntAttribute $ fromIntegral i)
--             ]
--       }
--   GetProtocolParameters era ->
--     RequestRenderedOTel
--       { requestName = "get-protocol-parameters"
--       , requestAttributes =
--           [
--             ( "era"
--             , case era of
--                 BabbageEraOnwardsBabbage -> "babbage"
--                 BabbageEraOnwardsConway -> "conway"
--             )
--           ]
--       , responseAttributes = \params ->
--           [
--             ( "protocol-parameters"
--             , toAttribute $ TextAttribute $ TL.toStrict $ case era of
--                 BabbageEraOnwardsBabbage -> encodeToLazyText params
--                 BabbageEraOnwardsConway -> encodeToLazyText params
--             )
--           ]
--       }
--   GetSystemStart ->
--     RequestRenderedOTel
--       { requestName = "get-system-start"
--       , requestAttributes = []
--       , responseAttributes = \(SystemStart time) ->
--           [("system-start", fromString $ show time)]
--       }
--   GetEraHistory ->
--     RequestRenderedOTel
--       { requestName = "get-era-history"
--       , requestAttributes = []
--       , responseAttributes = \(EraHistory (unInterpreter -> Summary summary)) ->
--           summaryAttributes summary $
--             Counting.Exactly $
--               K "byron"
--                 :* K "shelley"
--                 :* K "allegra"
--                 :* K "mary"
--                 :* K "alonzo"
--                 :* K "babbage"
--                 :* K "conway"
--                 :* Nil
--       }
--   GetUTxOs query ->
--     RequestRenderedOTel
--       { requestName = "get-utxos"
--       , requestAttributes = case query of
--           GetUTxOsAtAddresses addresses ->
--             [
--               ( "for-addresses"
--               , toAttribute $ mapMaybe toBech32 $ Set.toList addresses
--               )
--             ]
--           GetUTxOsForTxOutRefs outs ->
--             [
--               ( "for-outputs"
--               , toAttribute $ renderTxOutRef <$> Set.toList outs
--               )
--             ]
--       , responseAttributes = \utxos ->
--           [("outputs", toAttribute $ fmap renderTxOutRef $ Map.keys $ unUTxOs utxos)]
--       }
--   GetNodeTip ->
--     RequestRenderedOTel
--       { requestName = "get-node-tip"
--       , requestAttributes = []
--       , responseAttributes = tipAttributes
--       }
--   GetTip ->
--     RequestRenderedOTel
--       { requestName = "get-tip"
--       , requestAttributes = []
--       , responseAttributes = tipAttributes
--       }
--   GetEra ->
--     RequestRenderedOTel
--       { requestName = "get-era"
--       , requestAttributes = []
--       , responseAttributes = \(AnyCardanoEra era) ->
--           [
--             ( "era"
--             , case era of
--                 ByronEra -> "byron"
--                 ShelleyEra -> "shelley"
--                 AllegraEra -> "allegra"
--                 MaryEra -> "mary"
--                 AlonzoEra -> "alonzo"
--                 BabbageEra -> "babbage"
--                 ConwayEra -> "conway"
--             )
--           ]
--       }
--   GetScripts era scripts ->
--     RequestRenderedOTel
--       { requestName = "get-scripts"
--       , requestAttributes =
--           [
--             ( "era"
--             , case era of
--                 BabbageEraOnwardsBabbage -> "babbage"
--                 BabbageEraOnwardsConway -> "conway"
--             )
--           , ("scripts", toAttribute $ fmap (T.pack . show) $ Set.toList scripts)
--           ]
--       , responseAttributes = \result -> [("scripts", toAttribute $ fmap (T.pack . show) $ Map.keys result)]
--       }
-- 
-- summaryAttributes :: NonEmpty xs EraSummary -> Counting.Exactly xs Text -> [(Text, Attribute)]
-- summaryAttributes (NonEmptyOne summary) (Counting.Exactly (K era :* _)) =
--   eraSummaryAttributes era summary
-- summaryAttributes (NonEmptyCons summary summaries) (Counting.Exactly (K era :* eras)) =
--   eraSummaryAttributes era summary <> summaryAttributes summaries (Counting.Exactly eras)
-- 
-- eraSummaryAttributes :: Text -> EraSummary -> [(Text, Attribute)]
-- eraSummaryAttributes eraName EraSummary{..} =
--   first ((eraName <> ".") <>)
--     <$> fold
--       [ eraStartAttributes eraStart
--       , eraEndAttributes eraEnd
--       , eraParamsAttributes eraParams
--       ]
-- 
-- eraStartAttributes :: Bound -> [(Text, Attribute)]
-- eraStartAttributes Bound{..} =
--   [ ("era-start.relative-time", fromString $ show boundTime)
--   , ("era-start.slot", toAttribute $ IntAttribute $ fromIntegral $ O.unSlotNo boundSlot)
--   , ("era-start.epoch", toAttribute $ IntAttribute $ fromIntegral $ unEpochNo boundEpoch)
--   ]
-- 
-- eraEndAttributes :: EraEnd -> [(Text, Attribute)]
-- eraEndAttributes = \case
--   EraUnbounded -> [("era-end", "unbounded")]
--   EraEnd Bound{..} ->
--     [ ("era-end.relative-time", fromString $ show boundTime)
--     , ("era-end.slot", toAttribute $ IntAttribute $ fromIntegral $ O.unSlotNo boundSlot)
--     , ("era-end.epoch", toAttribute $ IntAttribute $ fromIntegral $ unEpochNo boundEpoch)
--     ]
-- 
-- eraParamsAttributes :: EraParams -> [(Text, Attribute)]
-- eraParamsAttributes EraParams{..} =
--   [ ("epoch-size", toAttribute $ IntAttribute $ fromIntegral $ unEpochSize eraEpochSize)
--   , ("slot-length", toAttribute $ IntAttribute $ floor $ nominalDiffTimeToSeconds $ getSlotLength eraSlotLength)
--   ,
--     ( "safe-zone"
--     , case eraSafeZone of
--         StandardSafeZone w -> toAttribute $ IntAttribute $ fromIntegral w
--         UnsafeIndefiniteSafeZone -> "indefinite"
--     )
--   ]

instance Ord AnyCardanoEra where
  compare = on compare fromEnum

instance Binary AnyCardanoEra where
  put = putWord8 . fromIntegral . fromEnum
  get = do
    tag <- getWord8
    case tag of
      0 -> pure $ AnyCardanoEra ByronEra
      1 -> pure $ AnyCardanoEra ShelleyEra
      2 -> pure $ AnyCardanoEra AllegraEra
      3 -> pure $ AnyCardanoEra MaryEra
      4 -> pure $ AnyCardanoEra AlonzoEra
      5 -> pure $ AnyCardanoEra BabbageEra
      6 -> pure $ AnyCardanoEra ConwayEra
      _ -> fail $ "Invalid era tag: " <> show tag

-- deriving instance Show (ChainSyncQuery a)
-- instance Eq (ChainSyncQuery a) where
--   (GetProtocolParameters BabbageEraOnwardsBabbage) == (GetProtocolParameters BabbageEraOnwardsBabbage) = True
--   (GetProtocolParameters BabbageEraOnwardsConway) == (GetProtocolParameters BabbageEraOnwardsConway) = True
--   GetSecurityParameter == GetSecurityParameter = True
--   GetNetworkId == GetNetworkId = True
--   GetSystemStart == GetSystemStart = True
--   GetEraHistory == GetEraHistory = True
--   GetUTxOs query == GetUTxOs query' = query == query'
--   GetNodeTip == GetNodeTip = True
--   GetTip == GetTip = True
--   GetEra == GetEra = True
--   GetScripts era scripts == GetScripts era' scripts' = era == era' && scripts == scripts'
--   _ == _ = False
-- 
-- instance HasSignature ChainSyncQuery where
--   signature _ = "ChainSyncQuery"
-- 
-- instance Query.RequestVariations ChainSyncQuery where
--   tagVariations =
--     NE.fromList
--       [ Query.SomeTag TagGetSecurityParameter
--       , Query.SomeTag TagGetNetworkId
--       , Query.SomeTag $ TagGetProtocolParameters BabbageEraOnwardsBabbage
--       , Query.SomeTag TagGetSystemStart
--       , Query.SomeTag TagGetEraHistory
--       , Query.SomeTag TagGetUTxOs
--       , Query.SomeTag TagGetNodeTip
--       , Query.SomeTag TagGetTip
--       , Query.SomeTag TagGetEra
--       , Query.SomeTag $ TagGetScripts BabbageEraOnwardsBabbage
--       , Query.SomeTag $ TagGetScripts BabbageEraOnwardsConway
--       ]
--   requestVariations = \case
--     TagGetSecurityParameter -> pure GetSecurityParameter
--     TagGetNetworkId -> pure GetNetworkId
--     TagGetProtocolParameters era -> pure (GetProtocolParameters era)
--     TagGetSystemStart -> pure GetSystemStart
--     TagGetEraHistory -> pure GetEraHistory
--     TagGetUTxOs -> GetUTxOs <$> variations
--     TagGetNodeTip -> pure GetNodeTip
--     TagGetTip -> pure GetTip
--     TagGetEra -> pure GetEra
--     TagGetScripts era -> GetScripts era <$> variations
--   resultVariations = \case
--     TagGetSecurityParameter -> variations
--     TagGetNetworkId -> Mainnet NE.:| [Testnet $ NetworkMagic 0]
--     TagGetProtocolParameters BabbageEraOnwardsBabbage -> variations
--     TagGetProtocolParameters BabbageEraOnwardsConway -> variations
--     TagGetSystemStart -> SystemStart <$> variations
--     TagGetEraHistory -> variations
--     TagGetUTxOs -> variations
--     TagGetNodeTip -> variations
--     TagGetTip -> variations
--     TagGetEra -> variations
--     TagGetScripts BabbageEraOnwardsBabbage -> variations
--     TagGetScripts BabbageEraOnwardsConway -> variations
-- 
-- TODO (Refactor Deprecated ProtocolParameters) : Below three instances are ugly workarounds which still rely on deprecated ProtocolParameters.
-- They should be removed and replaced by statically defined protocol params - we have JSON files in the repo which
-- should be included in the code.
--
-- instance Variations ProtocolParameters where
--   variations =
--     ( \ProtocolParameters{..} ->
--         ProtocolParameters
--           { protocolParamPrices =
--               Just (C.ExecutionUnitPrices{priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
--           , ..
--           }
--     )
--       <$> variations
--
-- convertToLedgerProtocolParameters
--   :: C.ShelleyBasedEra era
--   -> C.ProtocolParameters
--   -> Maybe (LedgerProtocolParameters era)
-- convertToLedgerProtocolParameters sbe pp =
--   LedgerProtocolParameters <$> C.toLedgerPParams sbe pp
-- 
-- 
-- instance Variations (LedgerProtocolParameters BabbageEra) where
--   variations = case traverse (convertToLedgerProtocolParameters C.ShelleyBasedEraBabbage) variations of
--     Left err -> error $ "ChainSync.Api.Variations PParams: " <> show err
--     Right vs -> vs
-- 
-- instance Variations (LedgerProtocolParameters ConwayEra) where
--   variations = case traverse (convertToLedgerProtocolParameters C.ShelleyBasedEraConway) variations of
--     Left err -> error $ "ChainSync.Api.Variations PParams: " <> show err
--     Right vs -> vs

instance Variations (ScriptInEra BabbageEra) where
  variations =
    pure $
      ScriptInEra C.PlutusScriptV1InBabbage $
        C.PlutusScript C.PlutusScriptV1 $
          C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

instance Variations (ScriptInEra ConwayEra) where
  variations =
    pure $
      ScriptInEra C.PlutusScriptV1InConway $
        C.PlutusScript C.PlutusScriptV1 $
          C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

instance Binary (ScriptInEra BabbageEra) where
  put = put . originalBytes . C.toShelleyScript
  get = do
    bytes <- get
    C.fromShelleyBasedScript C.ShelleyBasedEraBabbage <$> getDecodeFull bytes

instance Binary (ScriptInEra ConwayEra) where
  put = put . originalBytes . C.toShelleyScript
  get = do
    bytes <- get
    C.fromShelleyBasedScript C.ShelleyBasedEraConway <$> getDecodeFull bytes

getDecodeFull :: (DecCBOR (Annotator a)) => LBS.ByteString -> Get a
getDecodeFull = either (fail . show) pure . decodeFullAnnotator Base.shelleyProtVer "getDecodeFull" decCBOR

putEncodeFull :: (EncCBOR a) => a -> Put
putEncodeFull a = put $ BB.toLazyByteString $ toBuilder Base.shelleyProtVer (encCBOR a)

unInterpreter :: Interpreter xs -> Summary xs
unInterpreter = unsafeCoerce

eraEq :: AlonzoEraOnwards era1 -> AlonzoEraOnwards era2 -> Maybe (era1 :~: era2)
eraEq AlonzoEraOnwardsAlonzo AlonzoEraOnwardsAlonzo = Just Refl
eraEq AlonzoEraOnwardsAlonzo _ = Nothing
eraEq AlonzoEraOnwardsBabbage AlonzoEraOnwardsBabbage = Just Refl
eraEq AlonzoEraOnwardsBabbage _ = Nothing
eraEq AlonzoEraOnwardsConway AlonzoEraOnwardsConway = Just Refl
eraEq AlonzoEraOnwardsConway _ = Nothing
eraEq AlonzoEraOnwardsDijkstra AlonzoEraOnwardsDijkstra = Just Refl
eraEq AlonzoEraOnwardsDijkstra _ = Nothing

-- * Orphan instances

instance Variations AnyCardanoEra where
  variations = NE.fromList [minBound .. maxBound]

instance Variations C.PraosNonce where
  variations = C.makePraosNonce <$> variations

instance Variations Coin.Coin where
  variations = Coin.Coin <$> variations

instance Variations C.EpochNo

instance Variations C.EpochInterval

instance Variations C.AnyPlutusScriptVersion where
  variations =
    NE.fromList
      [ C.AnyPlutusScriptVersion C.PlutusScriptV1
      , C.AnyPlutusScriptVersion C.PlutusScriptV2
      , C.AnyPlutusScriptVersion C.PlutusScriptV3
      ]

instance Variations C.CostModel where
  variations = pure $ C.CostModel $ Map.elems $ fromJust defaultCostModelParamsForTesting

instance Variations C.ExecutionUnitPrices where
  variations = C.ExecutionUnitPrices <$> variations `varyAp` variations

instance Variations C.ExecutionUnits where
  variations = C.ExecutionUnits <$> variations `varyAp` variations

instance Variations C.EraHistory where
  variations = C.EraHistory <$> variations

instance (Variations (NonEmpty xs EraSummary)) => Variations (Interpreter xs) where
  variations = mkInterpreter <$> variations

instance (Variations (NonEmpty xs EraSummary)) => Variations (Summary xs) where
  variations = Summary <$> variations

instance {-# OVERLAPPING #-} (Variations a) => Variations (NonEmpty '[x] a) where
  variations = NonEmptyOne <$> variations

instance {-# OVERLAPPING #-} (Variations a, Variations (NonEmpty xs a)) => Variations (NonEmpty (x ': xs) a) where
  variations = NonEmptyCons <$> variations `varyAp` pure (NE.head variations)

instance Variations EraSummary

instance Variations EraParams

instance Variations a => Variations (PerasEnabled a)

instance Variations PerasRoundLength where
  variations =
    NE.fromList
      [ PerasRoundLength 1
      , PerasRoundLength 10
      , PerasRoundLength 100
      , PerasRoundLength 1000
      ]

instance Variations PerasRoundNo where
  variations =
    NE.fromList
      [ PerasRoundNo 1
      , PerasRoundNo 10
      , PerasRoundNo 100
      , PerasRoundNo 1000
      ]

instance Variations GenesisWindow

deriving instance (Generic GenesisWindow)

instance Variations SafeZone

instance Variations EpochSize

instance Variations SlotLength

instance Variations EraEnd

instance Variations Bound

instance Variations Cardano.SlotNo

instance Variations RelativeTime

-- renderChainSeekServerSelectorOTel :: RenderSelectorOTel RuntimeChainSeekServerSelector
-- renderChainSeekServerSelectorOTel =
--   ChainSeekServer.renderChainSeekServerSelectorOTel pointAttributes tipAttributes renderChainSeekQueryOTel
-- 
-- renderChainSeekClientSelectorOTel :: RenderSelectorOTel RuntimeChainSeekClientSelector
-- renderChainSeekClientSelectorOTel =
--   ChainSeekClient.renderChainSeekClientSelectorOTel pointAttributes tipAttributes renderChainSeekQueryOTel
-- 
-- pointAttributes :: Bool -> ChainPoint -> [(T.Text, Attribute)]
-- pointAttributes isEnd point = case point of
--   Genesis -> [("chain-point" <> suffix, "genesis")]
--   At BlockHeader{..} ->
--     [ ("chain-point.blockNo" <> suffix, toAttribute $ IntAttribute $ fromIntegral blockNo)
--     , ("chain-point.slotNo" <> suffix, toAttribute $ IntAttribute $ fromIntegral slotNo)
--     , ("chain-point.block-header-hash" <> suffix, toAttribute $ TextAttribute $ read $ show headerHash)
--     ]
--   where
--     suffix
--       | isEnd = ".end"
--       | otherwise = ".start"
-- 
-- tipAttributes :: ChainPoint -> [(T.Text, Attribute)]
-- tipAttributes = \case
--   Genesis -> [("chain-tip", "genesis")]
--   At BlockHeader{..} ->
--     [ ("chain-tip.blockNo", toAttribute $ IntAttribute $ fromIntegral blockNo)
--     , ("chain-tip.slotNo", toAttribute $ IntAttribute $ fromIntegral slotNo)
--     , ("chain-tip.block-header-hash", toAttribute $ TextAttribute $ read $ show headerHash)
--     ]
-- 
-- renderChainSeekQueryOTel :: RenderChainSeekQueryOTel Move
-- renderChainSeekQueryOTel = \case
--   AdvanceBlocks count ->
--     ChainSeekQueryOTelRendered
--       { queryName = "advance_blocks"
--       , queryAttributes = [("chain-seek.query.advance-blocks.count", toAttribute @Int $ fromIntegral count)]
--       , errorAttributes = \case {}
--       , resultAttributes = const []
--       }
--   Intersect blocks ->
--     ChainSeekQueryOTelRendered
--       { queryName = "intersect"
--       , queryAttributes =
--           [
--             ( "chain-seek.query.intersect.blocks"
--             , toAttribute $ TextAttribute . read . show . headerHash <$> blocks
--             )
--           ]
--       , errorAttributes = const [("chain-seek.query.intersect.match", "none")]
--       , resultAttributes = const [("chain-seek.query.intersect.match", "end-point")]
--       }
--   FindConsumingTxs txOutRefs ->
--     ChainSeekQueryOTelRendered
--       { queryName = "find_consuming_txs"
--       , queryAttributes =
--           [
--             ( "chain-seek.query.find-consuming-txs.ids"
--             , toAttribute $ renderTxOutRef <$> Set.toList txOutRefs
--             )
--           ]
--       , errorAttributes = \errors ->
--           [
--             ( "chain-seek.query.find-consuming-txs.errors"
--             , toAttribute do
--                 (txOutRef, err) <- Map.toList errors
--                 pure $ TextAttribute $ renderTxOutRef txOutRef <> ": " <> T.pack (show err)
--             )
--           ]
--       , resultAttributes = \results ->
--           [
--             ( "chain-seek.query.find-consuming-txs.results"
--             , toAttribute do
--                 (txOutRef, Transaction{..}) <- Map.toList results
--                 pure $ TextAttribute $ renderTxOutRef txOutRef <> ": " <> read (show txId)
--             )
--           ]
--       }
--   FindTx txId wait ->
--     ChainSeekQueryOTelRendered
--       { queryName = "find_tx"
--       , queryAttributes =
--           [ ("chain-seek.query.find-tx.txId", toAttribute $ TextAttribute $ read $ show txId)
--           , ("chain-seek.query.find-tx.wait-if-not-found", toAttribute wait)
--           ]
--       , errorAttributes = \err -> [("chain-seek.query.find-tx.error", fromString $ show err)]
--       , resultAttributes = \Transaction{txId = txId'} ->
--           [("chain-seek.query.find-tx.tx-id", toAttribute $ TextAttribute $ read $ show txId')]
--       }
--   FindTxsFor credentials ->
--     ChainSeekQueryOTelRendered
--       { queryName = "find_txs_for"
--       , queryAttributes =
--           [
--             ( "chain-seek.query.find-txs-for.credentials"
--             , toAttribute $ TextAttribute . T.pack . show <$> NE.toList (NESet.toList credentials)
--             )
--           ]
--       , errorAttributes = \case {}
--       , resultAttributes = \results ->
--           [
--             ( "chain-seek.query.find-txs-for.tx-tds"
--             , toAttribute $ TextAttribute . read . show . (\Transaction{..} -> txId) <$> Set.toList results
--             )
--           ]
--       }
--   AdvanceToTip ->
--     ChainSeekQueryOTelRendered
--       { queryName = "advance_to_tip"
--       , queryAttributes = []
--       , errorAttributes = \case {}
--       , resultAttributes = const []
--       }
-- 
-- instance Query.Request ChainSyncQuery where
--   data Tag ChainSyncQuery result where
--     TagGetSecurityParameter :: Query.Tag ChainSyncQuery Int
--     TagGetNetworkId :: Query.Tag ChainSyncQuery NetworkId
--     TagGetProtocolParameters :: BabbageEraOnwards era -> Query.Tag ChainSyncQuery (LedgerProtocolParameters era)
--     TagGetSystemStart :: Query.Tag ChainSyncQuery SystemStart
--     TagGetEraHistory :: Query.Tag ChainSyncQuery EraHistory
--     TagGetUTxOs :: Query.Tag ChainSyncQuery UTxOs
--     TagGetNodeTip :: Query.Tag ChainSyncQuery ChainPoint
--     TagGetTip :: Query.Tag ChainSyncQuery ChainPoint
--     TagGetEra :: Query.Tag ChainSyncQuery AnyCardanoEra
--     TagGetScripts :: BabbageEraOnwards era -> Query.Tag ChainSyncQuery (Map ScriptHash (ScriptInEra era))
--   tagEq TagGetSecurityParameter TagGetSecurityParameter = Just Refl
--   tagEq TagGetSecurityParameter _ = Nothing
--   tagEq TagGetNetworkId TagGetNetworkId = Just Refl
--   tagEq TagGetNetworkId _ = Nothing
--   tagEq (TagGetProtocolParameters BabbageEraOnwardsBabbage) (TagGetProtocolParameters BabbageEraOnwardsBabbage) = Just Refl
--   tagEq (TagGetProtocolParameters BabbageEraOnwardsBabbage) _ = Nothing
--   tagEq (TagGetProtocolParameters BabbageEraOnwardsConway) (TagGetProtocolParameters BabbageEraOnwardsConway) = Just Refl
--   tagEq (TagGetProtocolParameters BabbageEraOnwardsConway) _ = Nothing
--   tagEq TagGetEraHistory TagGetEraHistory = Just Refl
--   tagEq TagGetEraHistory _ = Nothing
--   tagEq TagGetSystemStart TagGetSystemStart = Just Refl
--   tagEq TagGetSystemStart _ = Nothing
--   tagEq TagGetUTxOs TagGetUTxOs = Just Refl
--   tagEq TagGetUTxOs _ = Nothing
--   tagEq TagGetNodeTip TagGetNodeTip = Just Refl
--   tagEq TagGetNodeTip _ = Nothing
--   tagEq TagGetTip TagGetTip = Just Refl
--   tagEq TagGetTip _ = Nothing
--   tagEq TagGetEra TagGetEra = Just Refl
--   tagEq TagGetEra _ = Nothing
--   tagEq (TagGetScripts BabbageEraOnwardsBabbage) (TagGetScripts BabbageEraOnwardsBabbage) = Just Refl
--   tagEq (TagGetScripts BabbageEraOnwardsBabbage) _ = Nothing
--   tagEq (TagGetScripts BabbageEraOnwardsConway) (TagGetScripts BabbageEraOnwardsConway) = Just Refl
--   tagEq (TagGetScripts BabbageEraOnwardsConway) _ = Nothing
--   tagFromReq = \case
--     GetSecurityParameter -> TagGetSecurityParameter
--     GetNetworkId -> TagGetNetworkId
--     GetProtocolParameters era -> TagGetProtocolParameters era
--     GetEraHistory -> TagGetEraHistory
--     GetSystemStart -> TagGetSystemStart
--     GetUTxOs _ -> TagGetUTxOs
--     GetNodeTip -> TagGetNodeTip
--     GetTip -> TagGetTip
--     GetEra -> TagGetEra
--     GetScripts era _ -> TagGetScripts era
-- 
-- deriving instance Show (Query.Tag ChainSyncQuery a)
-- instance Eq (Query.Tag ChainSyncQuery a) where
--   TagGetSecurityParameter == TagGetSecurityParameter = True
--   TagGetNetworkId == TagGetNetworkId = True
--   TagGetProtocolParameters BabbageEraOnwardsBabbage == TagGetProtocolParameters BabbageEraOnwardsBabbage = True
--   TagGetProtocolParameters BabbageEraOnwardsConway == TagGetProtocolParameters BabbageEraOnwardsConway = True
--   TagGetEraHistory == TagGetEraHistory = True
--   TagGetSystemStart == TagGetSystemStart = True
--   TagGetUTxOs == TagGetUTxOs = True
--   TagGetNodeTip == TagGetNodeTip = True
--   TagGetTip == TagGetTip = True
--   TagGetEra == TagGetEra = True
--   TagGetScripts BabbageEraOnwardsBabbage == TagGetScripts BabbageEraOnwardsBabbage = True
--   TagGetScripts BabbageEraOnwardsConway == TagGetScripts BabbageEraOnwardsConway = True
--   _ == _ = False
-- 
-- instance Query.BinaryRequest ChainSyncQuery where
--   putReq = \case
--     GetSecurityParameter -> putWord8 0x01
--     GetNetworkId -> putWord8 0x02
--     GetProtocolParameters era -> do
--       putWord8 0x03
--       putWord8 case era of
--         BabbageEraOnwardsBabbage -> 0x00
--         BabbageEraOnwardsConway -> 0x01
--     GetSystemStart -> putWord8 0x04
--     GetEraHistory -> putWord8 0x05
--     GetUTxOs q -> do
--       putWord8 0x06
--       case q of
--         (GetUTxOsAtAddresses addresses) -> do
--           putWord8 0x01
--           put addresses
--         (GetUTxOsForTxOutRefs txOutRefs) -> do
--           putWord8 0x02
--           put txOutRefs
--     GetNodeTip -> putWord8 0x07
--     GetTip -> putWord8 0x08
--     GetEra -> putWord8 0x09
--     GetScripts era scripts -> do
--       putWord8 0x0a
--       putWord8 case era of
--         BabbageEraOnwardsBabbage -> 0x00
--         BabbageEraOnwardsConway -> 0x01
--       put scripts
--   getReq = do
--     tag <- getWord8
--     case tag of
--       0x01 -> pure $ Query.SomeRequest GetSecurityParameter
--       0x02 -> pure $ Query.SomeRequest GetNetworkId
--       0x03 -> do
--         tag' <- getWord8
--         case tag' of
--           0x00 -> pure $ Query.SomeRequest $ GetProtocolParameters BabbageEraOnwardsBabbage
--           0x01 -> pure $ Query.SomeRequest $ GetProtocolParameters BabbageEraOnwardsConway
--           _ -> fail "Invalid BabbageEraOnwards tag"
--       0x04 -> pure $ Query.SomeRequest GetSystemStart
--       0x05 -> pure $ Query.SomeRequest GetEraHistory
--       0x06 -> do
--         tag' <- getWord8
--         Query.SomeRequest . GetUTxOs <$> case tag' of
--           0x01 -> do
--             GetUTxOsAtAddresses <$> get
--           0x02 -> do
--             GetUTxOsForTxOutRefs <$> get
--           _ -> fail "Invalid GetUTxOsQuery tag"
--       0x07 -> pure $ Query.SomeRequest GetNodeTip
--       0x08 -> pure $ Query.SomeRequest GetTip
--       0x09 -> pure $ Query.SomeRequest GetEra
--       0x10 -> do
--         tag' <- getWord8
--         case tag' of
--           0x00 -> Query.SomeRequest . GetScripts BabbageEraOnwardsBabbage <$> get
--           0x01 -> Query.SomeRequest . GetScripts BabbageEraOnwardsConway <$> get
--           _ -> fail "Invalid BabbageEraOnwards tag"
--       _ -> fail "Invalid ChainSyncQuery tag"
--   putResult = \case
--     TagGetSecurityParameter -> put
--     TagGetNetworkId ->
--       put . \case
--         Mainnet -> Nothing
--         Testnet (NetworkMagic magic) -> Just magic
--     TagGetProtocolParameters BabbageEraOnwardsBabbage -> put . Aeson.encode
--     TagGetProtocolParameters BabbageEraOnwardsConway -> put . Aeson.encode
--     TagGetEraHistory -> \case
--       EraHistory interpreter -> give EraParamsWithGenesisWindow $ put $ serialise interpreter
--     TagGetSystemStart -> \case
--       SystemStart start -> put start
--     TagGetUTxOs -> put
--     TagGetNodeTip -> put
--     TagGetTip -> put
--     TagGetEra -> put
--     TagGetScripts BabbageEraOnwardsBabbage -> put
--     TagGetScripts BabbageEraOnwardsConway -> put
--   getResult = \case
--     TagGetSecurityParameter -> get
--     TagGetNetworkId -> maybe Mainnet (Testnet . NetworkMagic) <$> get
--     TagGetProtocolParameters era -> C.babbageEraOnwardsConstraints era $ do
--       bytes <- get
--       case Aeson.decode bytes of
--         Nothing -> fail "failed to decode protocol parameters JSON"
--         Just params -> pure params
--     TagGetEraHistory -> do
--       bytes <- get
--       case give EraParamsWithGenesisWindow $ deserialiseOrFail bytes of
--         Left err -> fail $ show err
--         Right interpreter -> pure $ EraHistory interpreter
--     TagGetSystemStart -> SystemStart <$> get
--     TagGetUTxOs -> get
--     TagGetNodeTip -> get
--     TagGetTip -> get
--     TagGetEra -> get
--     TagGetScripts BabbageEraOnwardsBabbage -> get
--     TagGetScripts BabbageEraOnwardsConway -> get
-- 
-- instance Query.ShowRequest ChainSyncQuery where
--   showsPrecResult p = \case
--     TagGetSecurityParameter -> showsPrec p
--     TagGetNetworkId -> showsPrec p
--     TagGetProtocolParameters BabbageEraOnwardsBabbage -> showsPrec p
--     TagGetProtocolParameters BabbageEraOnwardsConway -> showsPrec p
--     TagGetSystemStart -> showsPrec p
--     TagGetEraHistory -> \(EraHistory interpreter) ->
--       showParen
--         (p >= 11)
--         ( showString "EraHistory"
--             . showSpace
--             . showString "CardanoMode"
--             . showSpace
--             . showParen
--               True
--               ( showString "mkInterpreter"
--                   . showSpace
--                   . showsPrec 11 (unInterpreter interpreter)
--               )
--         )
--     TagGetUTxOs -> showsPrec p
--     TagGetNodeTip -> showsPrec p
--     TagGetTip -> showsPrec p
--     TagGetEra -> showsPrec p
--     TagGetScripts _ -> showsPrec p
-- 
-- instance Query.OTelRequest ChainSyncQuery where
--   reqTypeName _ = "chain_sync"
--   reqName = \case
--     TagGetSecurityParameter -> "security_parameter"
--     TagGetNetworkId -> "network_id"
--     TagGetProtocolParameters _ -> "protocol_parameters"
--     TagGetSystemStart -> "system_start"
--     TagGetEraHistory -> "era_history"
--     TagGetUTxOs -> "utxos"
--     TagGetNodeTip -> "node_tip"
--     TagGetTip -> "tip"
--     TagGetEra -> "era"
--     TagGetScripts _ -> "scripts"


-- data ChainSyncCommand status err result where
--   SubmitTx :: AlonzoEraOnwards era -> Tx era -> ChainSyncCommand Void String ()
-- 
-- instance HasSignature ChainSyncCommand where
--   signature _ = "ChainSyncCommand"
-- 
-- instance Job.Command ChainSyncCommand where
--   data Tag ChainSyncCommand status err result where
--     TagSubmitTx :: AlonzoEraOnwards era -> Job.Tag ChainSyncCommand Void String ()
-- 
--   data JobId ChainSyncCommand status err result
-- 
--   tagFromCommand = \case
--     SubmitTx era _ -> TagSubmitTx era
--   tagFromJobId = \case {}
--   tagEq (TagSubmitTx era1) (TagSubmitTx era2) = do
--     Refl <- eraEq era1 era2
--     pure (Refl, Refl, Refl)
--   putTag = \case
--     TagSubmitTx era -> do
--       putWord8 0x01
--       case era of
--         AlonzoEraOnwardsAlonzo -> putWord8 0x01
--         AlonzoEraOnwardsBabbage -> putWord8 0x02
--         AlonzoEraOnwardsConway -> putWord8 0x03
--   getTag =
--     getWord8 >>= \case
--       0x01 ->
--         getWord8 >>= \case
--           0x01 -> pure $ Job.SomeTag $ TagSubmitTx AlonzoEraOnwardsAlonzo
--           0x02 -> pure $ Job.SomeTag $ TagSubmitTx AlonzoEraOnwardsBabbage
--           0x03 -> pure $ Job.SomeTag $ TagSubmitTx AlonzoEraOnwardsConway
--           tag -> fail $ "invalid era tag " <> show tag
--       tag -> fail $ "invalid command tag " <> show tag
--   putJobId = \case {}
--   getJobId = \case
--     TagSubmitTx _ -> fail "SubmitTx does not support job IDs"
--   putCommand = \case
--     SubmitTx AlonzoEraOnwardsAlonzo tx -> put $ serialiseToCBOR tx
--     SubmitTx AlonzoEraOnwardsBabbage tx -> put $ serialiseToCBOR tx
--     SubmitTx AlonzoEraOnwardsConway tx -> put $ serialiseToCBOR tx
--   getCommand = \case
--     TagSubmitTx era ->
--       SubmitTx era <$> do
--         bytes <- get @ByteString
--         case era of
--           AlonzoEraOnwardsAlonzo -> case deserialiseFromCBOR (AsTx AsAlonzoEra) bytes of
--             Left err -> fail $ show err
--             Right tx -> pure tx
--           AlonzoEraOnwardsBabbage -> case deserialiseFromCBOR (AsTx AsBabbageEra) bytes of
--             Left err -> fail $ show err
--             Right tx -> pure tx
--           AlonzoEraOnwardsConway -> case deserialiseFromCBOR (AsTx AsConwayEra) bytes of
--             Left err -> fail $ show err
--             Right tx -> pure tx
--   putStatus = \case
--     TagSubmitTx _ -> absurd
--   getStatus = \case
--     TagSubmitTx _ -> fail "SubmitTx does not support job statuses"
--   putErr = \case
--     TagSubmitTx _ -> put
--   getErr = \case
--     TagSubmitTx _ -> get
--   putResult = \case
--     TagSubmitTx _ -> mempty
--   getResult = \case
--     TagSubmitTx _ -> pure ()
-- 
-- instance Job.ShowCommand ChainSyncCommand where
--   showsPrecTag p =
--     showParen (p >= 11) . showString . \case
--       TagSubmitTx AlonzoEraOnwardsAlonzo -> "TagSubmitTx AlonzoEraOnwardsAlonzo"
--       TagSubmitTx AlonzoEraOnwardsBabbage -> "TagSubmitTx AlonzoEraOnwardsBabbage"
--       TagSubmitTx AlonzoEraOnwardsConway -> "TagSubmitTx AlonzoEraOnwardsConway"
--   showsPrecCommand p =
--     showParen (p >= 11) . \case
--       SubmitTx AlonzoEraOnwardsAlonzo tx ->
--         ( showString "TagSubmitTx AlonzoEraOnwardsAlonzo"
--             . showSpace
--             . showsPrec p tx
--         )
--       SubmitTx AlonzoEraOnwardsBabbage tx ->
--         ( showString "TagSubmitTx AlonzoEraOnwardsBabbage"
--             . showSpace
--             . showsPrec p tx
--         )
--       SubmitTx AlonzoEraOnwardsConway tx ->
--         ( showString "TagSubmitTx AlonzoEraOnwardsConway"
--             . showSpace
--             . showsPrec p tx
--         )
--   showsPrecJobId _ = \case {}
--   showsPrecStatus _ = \case
--     TagSubmitTx _ -> absurd
--   showsPrecErr p = \case
--     TagSubmitTx _ -> showsPrec p
--   showsPrecResult p = \case
--     TagSubmitTx _ -> showsPrec p
-- 
-- instance Job.OTelCommand ChainSyncCommand where
--   commandTypeName _ = "chain_sync"
--   commandName = \case
--     TagSubmitTx era ->
--       "submit_tx/" <> case era of
--         AlonzoEraOnwardsAlonzo -> "alonzo"
--         AlonzoEraOnwardsBabbage -> "babbage"
--         AlonzoEraOnwardsConway -> "babbage"
-- 

