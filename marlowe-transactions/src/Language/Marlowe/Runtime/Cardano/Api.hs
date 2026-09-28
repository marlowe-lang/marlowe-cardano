{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

-- FIXME: Move most of the types to thin newtype wrappers around the Cardano API types
-- to avoid those Maybes
module Language.Marlowe.Runtime.Cardano.Api where

import Cardano.Api (getScriptData, unsafeHashableScriptData, TxBodyContent(..))
import qualified Cardano.Api as C
import qualified Cardano.Api.Value as Value
import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (fold)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Debug.Trace (traceShow)
import Language.Marlowe.Runtime.Cardano.Feature
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Cardano.Api.Kupo ()
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Crypto.Hash as Ledger
import Data.ByteString.Short (fromShort)
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus as Ledger
import Cardano.Ledger.Api (binaryDataToData)
import qualified Cardano.Ledger.Conway as L
import Cardano.Ledger.Plutus (unData)
import qualified Language.Marlowe.Runtime.Cardano.Api.Kupo as Kupo
import Data.Functor ((<&>))
import qualified Cardano.Ledger.BaseTypes as Ledger
import Control.Error (note)
import Data.Traversable (for)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Set as Set
import qualified PlutusLedgerApi.Common as P
import Data.These (These (This, That, These))

babbageEraOnwardsToMaryEraOnwards :: C.BabbageEraOnwards era -> C.MaryEraOnwards era
babbageEraOnwardsToMaryEraOnwards = \case
  C.BabbageEraOnwardsBabbage -> C.MaryEraOnwardsBabbage
  C.BabbageEraOnwardsConway -> C.MaryEraOnwardsConway
  C.BabbageEraOnwardsDijkstra -> C.MaryEraOnwardsDijkstra

babbageEraOnwardsToAlonzoEraOnwards :: C.BabbageEraOnwards era -> C.AlonzoEraOnwards era
babbageEraOnwardsToAlonzoEraOnwards = \case
  C.BabbageEraOnwardsBabbage -> C.AlonzoEraOnwardsBabbage
  C.BabbageEraOnwardsConway -> C.AlonzoEraOnwardsConway
  C.BabbageEraOnwardsDijkstra -> C.AlonzoEraOnwardsDijkstra

babbageEraOnwardsToShelleyBasedEra :: C.BabbageEraOnwards era -> C.ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = \case
  C.BabbageEraOnwardsBabbage -> C.ShelleyBasedEraBabbage
  C.BabbageEraOnwardsConway -> C.ShelleyBasedEraConway
  C.BabbageEraOnwardsDijkstra -> C.ShelleyBasedEraDijkstra

toCardanoScriptHash :: ScriptHash -> Maybe C.ScriptHash
toCardanoScriptHash = hush . C.deserialiseFromRawBytes C.AsScriptHash . unScriptHash

toCardanoPlutusScript :: forall lang. (C.HasTypeProxy lang) => PlutusScript -> Maybe (C.PlutusScript lang)
toCardanoPlutusScript = hush . C.deserialiseFromRawBytes (C.proxyToAsType (Proxy :: Proxy (C.PlutusScript lang))) . unPlutusScript

-- TODO: Check if this serialisation would detect version mismatch
fromCardanoPlutusScript
  :: forall lang
   . C.HasTypeProxy lang
  => C.PlutusScript lang
  -> PlutusScript
fromCardanoPlutusScript = PlutusScript . C.serialiseToRawBytes

fromPlutusSerialisedScript
  :: forall lang
   . C.HasTypeProxy lang
  => C.PlutusScriptVersion lang
  -> P.SerialisedScript
  -> PlutusScript
fromPlutusSerialisedScript _v bytes = fromCardanoPlutusScript (C.PlutusScriptSerialised bytes :: C.PlutusScript lang)

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash ps = hashPlutusScript C.PlutusScriptV2 <|> hashPlutusScript C.PlutusScriptV1 <|> hashPlutusScript C.PlutusScriptV3
  where
    hashPlutusScript :: C.IsPlutusScriptLanguage lang => C.PlutusScriptVersion lang -> Maybe ScriptHash
    hashPlutusScript pv = do
      script <- C.PlutusScript pv <$> toCardanoPlutusScript ps
      pure . fromCardanoScriptHash . C.hashScript $ script

toCardanoDatumHash :: DatumHash -> Maybe (C.Hash C.ScriptData)
toCardanoDatumHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) . unDatumHash

fromCardanoDatumHash :: C.Hash C.ScriptData -> DatumHash
fromCardanoDatumHash = DatumHash . C.serialiseToRawBytes

toCardanoScriptData :: Datum -> C.ScriptData
toCardanoScriptData = \case
  Constr i ds -> C.ScriptDataConstructor i $ toCardanoScriptData <$> ds
  Map ds -> C.ScriptDataMap $ bimap toCardanoScriptData toCardanoScriptData <$> ds
  List ds -> C.ScriptDataList $ toCardanoScriptData <$> ds
  I i -> C.ScriptDataNumber i
  B bs -> C.ScriptDataBytes bs

fromCardanoScriptData :: C.ScriptData -> Datum
fromCardanoScriptData = \case
  C.ScriptDataConstructor i ds -> Constr i $ fromCardanoScriptData <$> ds
  C.ScriptDataMap ds -> Map $ bimap fromCardanoScriptData fromCardanoScriptData <$> ds
  C.ScriptDataList ds -> List $ fromCardanoScriptData <$> ds
  C.ScriptDataNumber i -> I i
  C.ScriptDataBytes bs -> B bs

toCardanoPaymentKeyHash :: PaymentKeyHash -> Maybe (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) . unPaymentKeyHash

toCardanoPolicyId :: PolicyId -> Maybe C.PolicyId
toCardanoPolicyId = hush . C.deserialiseFromRawBytes C.AsPolicyId . unPolicyId

toCardanoPaymentCredential :: Credential -> Maybe C.PaymentCredential
toCardanoPaymentCredential = \case
  PaymentKeyCredential pkh -> C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash pkh
  ScriptCredential sh -> C.PaymentCredentialByScript <$> toCardanoScriptHash sh

fromCardanoPaymentCredential :: C.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  C.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  C.PaymentCredentialByScript sh -> ScriptCredential $ fromCardanoScriptHash sh

toCardanoStakeKeyHash :: StakeKeyHash -> Maybe (C.Hash C.StakeKey)
toCardanoStakeKeyHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsStakeKey) . unStakeKeyHash

toCardanoStakeCredential :: StakeCredential -> Maybe C.StakeCredential
toCardanoStakeCredential = \case
  StakeKeyCredential pkh -> C.StakeCredentialByKey <$> toCardanoStakeKeyHash pkh
  StakeScriptCredential sh -> C.StakeCredentialByScript <$> toCardanoScriptHash sh

fromCardanoPolicyId :: C.PolicyId -> PolicyId
fromCardanoPolicyId = PolicyId . C.serialiseToRawBytes

toCardanoAssetName :: TokenName -> C.AssetName
toCardanoAssetName = C.UnsafeAssetName . unTokenName

fromCardanoAssetName :: C.AssetName -> TokenName
fromCardanoAssetName (C.UnsafeAssetName name) = TokenName name

toCardanoQuantity :: Quantity -> C.Quantity
toCardanoQuantity = C.Quantity . unQuantity

fromCardanoQuantity :: C.Quantity -> Quantity
fromCardanoQuantity (C.Quantity q) = Quantity q

toCardanoLovelace :: Lovelace -> C.Coin
toCardanoLovelace = C.Coin . unLovelace

fromCardanoLovelace :: C.Coin -> Lovelace
fromCardanoLovelace (C.Coin l) = Lovelace l

fromCardanoBlockHeaderHash :: C.Hash C.BlockHeader -> BlockHeaderHash
fromCardanoBlockHeaderHash = BlockHeaderHash . C.serialiseToRawBytes

toCardanoBlockHeaderHash :: BlockHeaderHash -> Maybe (C.Hash C.BlockHeader)
toCardanoBlockHeaderHash = hush . C.deserialiseFromRawBytes (C.proxyToAsType (Proxy :: Proxy (C.Hash C.BlockHeader))) . unBlockHeaderHash

fromCardanoBlockNo :: C.BlockNo -> BlockNo
fromCardanoBlockNo (C.BlockNo n) = BlockNo n

toCardanoBlockNo :: BlockNo -> C.BlockNo
toCardanoBlockNo (BlockNo n) = C.BlockNo n

fromCardanoSlotNo :: C.SlotNo -> SlotNo
fromCardanoSlotNo (C.SlotNo slotNo) = SlotNo slotNo

toCardanoSlotNo :: SlotNo -> C.SlotNo
toCardanoSlotNo slotNo = C.SlotNo slotNo.unSlotNo

fromCardanoBlockHeader :: C.BlockHeader -> BlockHeader
fromCardanoBlockHeader (C.BlockHeader slotNo blockHash blockNo) =
  BlockHeader
    { slotNo = fromCardanoSlotNo slotNo
    , headerHash = fromCardanoBlockHeaderHash blockHash
    , blockNo = fromCardanoBlockNo blockNo
    }

toCardanoBlockHeader :: BlockHeader -> Maybe C.BlockHeader
toCardanoBlockHeader BlockHeader{..} = do
  h <- toCardanoBlockHeaderHash headerHash
  pure $ C.BlockHeader
    (toCardanoSlotNo slotNo)
    h
    (toCardanoBlockNo blockNo)

fromCardanoChainTip :: C.ChainTip -> ChainTip
fromCardanoChainTip = \case
  C.ChainTipAtGenesis -> ChainTip Nothing
  C.ChainTip slotNo blockHash blockNo -> ChainTip . Just $ BlockHeader
    (fromCardanoSlotNo slotNo)
    (fromCardanoBlockHeaderHash blockHash)
    (fromCardanoBlockNo blockNo)

tokensToCardanoValue :: Tokens -> Maybe C.Value
tokensToCardanoValue = fmap Value.valueFromList . traverse toCardanoValue' . Map.toList . unTokens
  where
    toCardanoValue' :: (AssetId, Quantity) -> Maybe (C.AssetId, C.Quantity)
    toCardanoValue' (AssetId policyId tokenName, quantity) =
      (,)
        <$> (C.AssetId <$> toCardanoPolicyId policyId <*> pure (toCardanoAssetName tokenName))
        <*> pure (toCardanoQuantity quantity)

tokensFromCardanoValue :: C.Value -> Tokens
tokensFromCardanoValue = Tokens . Map.fromList . mapMaybe fromCardanoValue' . Value.valueToList
  where
    fromCardanoValue' :: (C.AssetId, C.Quantity) -> Maybe (AssetId, Quantity)
    fromCardanoValue' (C.AdaAssetId, _) = Nothing
    fromCardanoValue' (C.AssetId policy name, q) =
      Just (AssetId (fromCardanoPolicyId policy) (fromCardanoAssetName name), fromCardanoQuantity q)

tokensFromCardanoMintValue :: forall ctx era. C.TxMintValue era ctx -> Tokens
tokensFromCardanoMintValue = \case
  C.TxMintNone -> Tokens Map.empty
  C.TxMintValue _ mintigMap -> Tokens $ Map.fromList do
    (policy, (C.PolicyAssets assets, _)) <- Map.toList mintigMap
    (name, q) <- Map.toList assets
    pure (AssetId (fromCardanoPolicyId policy) (fromCardanoAssetName name), fromCardanoQuantity q)

assetsToCardanoValue :: TxOutAssets -> Maybe C.Value
assetsToCardanoValue (TxOutAssets (Assets ada tokens)) =
  fold
    <$> sequence
      [ Just $ Value.valueFromList [(C.AdaAssetId, C.lovelaceToQuantity $ toCardanoLovelace ada)]
      , tokensToCardanoValue tokens
      ]

assetsFromCardanoValue :: C.Value -> Assets
assetsFromCardanoValue value =
  Assets
    { ada = fromCardanoLovelace $ C.selectLovelace value
    , tokens = tokensFromCardanoValue value
    }

toCardanoTxOutValue :: C.MaryEraOnwards era -> TxOutAssets -> Maybe (C.TxOutValue era)
toCardanoTxOutValue C.MaryEraOnwardsMary assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraMary . C.toLedgerValue C.MaryEraOnwardsMary <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsAlonzo assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraAlonzo . C.toLedgerValue C.MaryEraOnwardsAlonzo
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsBabbage assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage . C.toLedgerValue C.MaryEraOnwardsBabbage
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsConway assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraConway . C.toLedgerValue C.MaryEraOnwardsConway
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsDijkstra assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraDijkstra . C.toLedgerValue C.MaryEraOnwardsDijkstra
    <$> assetsToCardanoValue assets

fromCardanoTxOutValue :: C.TxOutValue era -> Assets
fromCardanoTxOutValue = \case
  C.TxOutValueByron value -> assetsFromCardanoValue $ C.lovelaceToValue value
  C.TxOutValueShelleyBased era value -> assetsFromCardanoValue $ C.fromLedgerValue era value

toCardanoTxOutDatum
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe Datum
  -> Maybe (C.TxOutDatum C.CtxTx era)
toCardanoTxOutDatum era mDatumHash mDatum = do
  let
    onEra = C.inEonForEra (Just C.TxOutDatumNone) \scriptDataSupported -> case (mDatumHash, mDatum) of
      (Nothing, Nothing) -> Just C.TxOutDatumNone
      (Just hash, Nothing) -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash
      (_, Just datum) -> Just $ C.TxOutSupplementalDatum scriptDataSupported $ unsafeHashableScriptData $ toCardanoScriptData datum
      -- -- FIXME: Inline datum should be an option on the table as well
      -- --        but currently it breaks min ADA adjustment.
      -- (_, Just datum) -> do
      --  babbageEraOnwards <- C.inEonForEra Nothing pure era
      --  Just . C.TxOutDatumInline babbageEraOnwards . unsafeHashableScriptData $ toCardanoScriptData datum
  onEra era

toCardanoTxOutDatum'
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe (C.TxOutDatum ctx era)
toCardanoTxOutDatum' = C.inEonForEra (const $ Just C.TxOutDatumNone) \scriptDataSupported -> \case
  Nothing -> Just C.TxOutDatumNone
  Just hash -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> Maybe (These DatumHash Datum)
fromCardanoTxOutDatum = \case
  C.TxOutDatumNone -> Nothing
  C.TxOutDatumHash _ hash -> Just . This . fromCardanoDatumHash $ hash
  C.TxOutDatumInline _ datum -> Just . That . fromCardanoScriptData $ getScriptData datum
  C.TxOutSupplementalDatum _ datum -> Just do
    let
      datumHash = fromCardanoDatumHash . C.hashScriptDataBytes $ datum
    These datumHash . fromCardanoScriptData $ getScriptData datum

-- When we have only a hash of the datum and no access to the value
-- we distinguish this case from the case where there is no datum at all.
data DatumNotAccessible = DatumNotAccessible

fromCardanoTxOutCtxUTxODatum :: C.TxOutDatum C.CtxUTxO era -> Maybe (Either DatumHash Datum)
fromCardanoTxOutCtxUTxODatum = \case
  C.TxOutDatumNone -> Nothing
  C.TxOutDatumHash _ hash -> Just . Left . fromCardanoDatumHash $ hash
  C.TxOutDatumInline _ datum -> Just $ Right $ fromCardanoScriptData $ getScriptData datum

toCardanoTxOut :: C.MaryEraOnwards era -> TransactionOutput -> Maybe (C.TxOut C.CtxTx era)
toCardanoTxOut era TransactionOutput{..} = do
  printIfNone $
    C.TxOut
      <$> toCardanoAddressInEra (C.toCardanoEra era) address
      <*> toCardanoTxOutValue era assets
      <*> toCardanoTxOutDatum (C.toCardanoEra era) datumHash datum
      <*> pure C.ReferenceScriptNone
  where
    printIfNone = \case
      Nothing -> traceShow TransactionOutput{..} Nothing
      Just a -> Just a

toCardanoTxOut'
  :: C.MaryEraOnwards era
  -> TransactionOutput
  -> Maybe C.ScriptInAnyLang
  -> Maybe (C.TxOut ctx era)
toCardanoTxOut' era TransactionOutput{..} mScript = do
  ref <- case (mScript, era) of
    (Nothing, _) -> Just C.ReferenceScriptNone
    (_, C.MaryEraOnwardsMary) -> Nothing
    (_, C.MaryEraOnwardsAlonzo) -> Nothing
    (Just script, C.MaryEraOnwardsBabbage) -> Just $ C.ReferenceScript C.BabbageEraOnwardsBabbage script
    (Just script, C.MaryEraOnwardsConway) -> Just $ C.ReferenceScript C.BabbageEraOnwardsConway script
    (Just script, C.MaryEraOnwardsDijkstra) -> Just $ C.ReferenceScript C.BabbageEraOnwardsDijkstra script
  C.TxOut
    <$> toCardanoAddressInEra (C.toCardanoEra era) address
    <*> toCardanoTxOutValue era assets
    <*> toCardanoTxOutDatum' (C.toCardanoEra era) datumHash
    <*> pure ref

fromCardanoTxOut
  :: C.CardanoEra era
  -> C.TxOut C.CtxTx era
  -> Maybe TransactionOutput
fromCardanoTxOut era (C.TxOut address value txOutDatum _) = do
  txOutAssets <- mkTxOutAssets (fromCardanoTxOutValue value)
  let
    (hash, datum) = fromMaybe (Nothing, Nothing) $ fromCardanoTxOutDatum txOutDatum >>= \case
      This h -> pure (Just h, Nothing)
      That d -> pure (Nothing, Just d)
      These h d -> pure (Just h, Just d)
  pure $
    TransactionOutput
      (fromCardanoAddressInEra era address)
      txOutAssets
      hash
      datum

fromCardanoTxOutCtxUTxO
  :: C.CardanoEra era
  -> C.TxOut C.CtxUTxO era
  -> Maybe TransactionOutput
fromCardanoTxOutCtxUTxO era (C.TxOut address value txOutDatum _) = do
  txOutAssets <- mkTxOutAssets (fromCardanoTxOutValue value)
  (hash, datum) <- case fromCardanoTxOutCtxUTxODatum txOutDatum of
    Nothing -> Just (Nothing, Nothing)
    Just (Left h) -> Just (Just h, Nothing)
    Just (Right d) -> Just (Nothing, Just d)
  pure $
    TransactionOutput
      (fromCardanoAddressInEra era address)
      txOutAssets
      hash
      datum

cardanoEraToAsType :: C.CardanoEra era -> C.AsType era
cardanoEraToAsType = \case
  C.ByronEra -> C.AsByronEra
  C.ShelleyEra -> C.AsShelleyEra
  C.AllegraEra -> C.AsAllegraEra
  C.MaryEra -> C.AsMaryEra
  C.AlonzoEra -> C.AsAlonzoEra
  C.BabbageEra -> C.AsBabbageEra
  C.ConwayEra -> C.AsConwayEra
  C.DijkstraEra -> C.AsDijkstraEra

toCardanoAddressInEra :: C.CardanoEra era -> Address -> Maybe (C.AddressInEra era)
toCardanoAddressInEra era =
  withCardanoEra era $
    hush . C.deserialiseFromRawBytes (C.AsAddressInEra $ cardanoEraToAsType era) . unAddress

fromCardanoAddressInEra :: C.CardanoEra era -> C.AddressInEra era -> Address
fromCardanoAddressInEra era =
  withCardanoEra era $
    Address . C.serialiseToRawBytes

toCardanoTxIn :: TxOutRef -> Maybe C.TxIn
toCardanoTxIn (TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (toCardanoTxIx txIx)

fromCardanoTxIn :: C.TxIn -> TxOutRef
fromCardanoTxIn (C.TxIn txId txIx) = TxOutRef (fromCardanoTxId txId) (fromCardanoTxIx txIx)

toCardanoTxId :: TxId -> Maybe C.TxId
toCardanoTxId = hush . C.deserialiseFromRawBytes C.AsTxId . unTxId

fromCardanoTxId :: C.TxId -> TxId
fromCardanoTxId = TxId . C.serialiseToRawBytes

toCardanoTxIx :: TxIx -> C.TxIx
toCardanoTxIx = C.TxIx . fromIntegral . unTxIx

fromCardanoTxIx :: C.TxIx -> TxIx
fromCardanoTxIx (C.TxIx txIx) = TxIx $ fromIntegral txIx

fromCardanoTxValidity :: C.TxValidityLowerBound era -> C.TxValidityUpperBound era -> ValidityRange
fromCardanoTxValidity l u = case (l, u) of
  (C.TxValidityNoLowerBound, C.TxValidityUpperBound _ Nothing) -> Unbounded
  (C.TxValidityNoLowerBound, C.TxValidityUpperBound _ (Just upper)) -> MaxBound $ fromCardanoSlotNo upper
  (C.TxValidityLowerBound _ lower, C.TxValidityUpperBound _ Nothing) -> MinBound $ fromCardanoSlotNo lower
  (C.TxValidityLowerBound _ lower, C.TxValidityUpperBound _ (Just upper)) -> MinMaxBound (fromCardanoSlotNo lower) (fromCardanoSlotNo upper)


fromLedgerTxId :: Ledger.TxId -> TxId
fromLedgerTxId = TxId . (\(Ledger.UnsafeHash h) -> fromShort h) . Ledger.extractHash . Ledger.unTxId

fromLedgerTxIn :: Ledger.TxIn -> TxOutRef
fromLedgerTxIn (Ledger.TxIn txId txIx) = TxOutRef (fromLedgerTxId txId) (TxIx $ Ledger.unTxIx txIx)

fromLedgerBinaryData :: Ledger.BinaryData L.ConwayEra -> Datum
fromLedgerBinaryData = fromPlutusData . unData . binaryDataToData

inputsFromCardanoTx :: C.IsCardanoEra era => C.Tx era -> Map.Map TxOutRef (Maybe Redeemer)
inputsFromCardanoTx tx = do
  let
    tx' = Kupo.txFromCardanoTx tx
    ledgerBasedResult = foldr
      (\(i, ref) refs -> (ref, Kupo.spendRedeemer tx' i):refs)
      mempty
      (zip [0 ..] (Set.toAscList $ Kupo.spentInputs tx'))
  Map.fromList $ ledgerBasedResult <&>
    bimap fromLedgerTxIn (fmap (Redeemer . fromLedgerBinaryData))

data TxConversionFailure = FailedToExtractTxInfo { txOutRef :: TxOutRef, reason :: String }
  deriving Show

instance ToJSON TxConversionFailure where
  toJSON (FailedToExtractTxInfo txOutRef reason) = A.object
    [ "txOutRef" .= txOutRef
    , "reason" .= reason
    ]

fromCardanoTransaction :: forall era. C.IsCardanoEra era => C.Tx era -> Either TxConversionFailure Transaction
fromCardanoTransaction tx = do
  let
    txBody@(C.TxBody (C.TxBodyContent {..})) = C.getTxBody tx
    era = C.cardanoEra
    txId = fromCardanoTxId $ C.getTxId txBody
    validityRange = fromCardanoTxValidity txValidityLowerBound txValidityUpperBound
    metadata = case txMetadata of
      C.TxMetadataNone -> TransactionMetadata Map.empty
      C.TxMetadataInEra _ md -> fromCardanoTxMetadata md
    inputs = inputsFromCardanoTx tx
    mintedTokens = tokensFromCardanoMintValue txMintValue
  outputs <- for (zip [0::Int ..] txOuts) \(i, txOut) -> do
    note
      (FailedToExtractTxInfo (TxOutRef txId (TxIx $ fromIntegral i)) "Failed to extract transaction output")
      $ fromCardanoTxOut era txOut
  pure $ Transaction{..}

fromCardanoUTxO
  :: C.IsCardanoEra era
  => C.UTxO era
  -> Maybe UTxOs
fromCardanoUTxO (C.UTxO utxosMap) = do
  let
    era = C.cardanoEra
  items <- for (Map.toList utxosMap) \(txIn, txOut) -> do
    output <- fromCardanoTxOutCtxUTxO era txOut
    pure (fromCardanoTxIn txIn, output)
  pure . UTxOs . Map.fromList $ items

