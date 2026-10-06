{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A local copy of the cardano-cli `Contrib.Cardano.CLI.Run.Friendly`
-- | because I'm not able to use `cardano-cli` lib in our current setup.
module Cardano.Debug (
  friendlyTxOut,
  friendlyTxBS,
  friendlyTxBody,
  friendlyTxBodyContentBS,
  friendlyTxBodyContent,
  friendlyTxBodyBS,
) where

import Prelude

import Cardano.Api as Api
import Cardano.Api.Address (
  Address (ShelleyAddress),
  KeyWitness (ShelleyBootstrapWitness, ShelleyKeyWitness),
  StakeAddress (..),
  StakePoolParameters (..),
  fromShelleyPaymentCredential,
  fromShelleyPoolParams,
  fromShelleyStakeReference,
  toShelleyStakeCredential,
 )
import Cardano.Api.Byron (KeyWitness (ByronKeyWitness))
import Cardano.Api.Ledger (
  ShelleyLedgerEra,
  ShelleyTxCert (..),
 )
import Cardano.Ledger.Coin qualified as Coin
import Cardano.Ledger.Crypto qualified as Crypto
import Cardano.Ledger.Shelley.API qualified as Shelley
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isAlphaNum, isAscii)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Ratio (denominator)
import Data.Text qualified as Text
import Data.Yaml (array)
import Data.Yaml.Pretty (setConfCompare)
import Data.Yaml.Pretty qualified as Yaml
import GHC.Real (numerator)

yamlConfig :: Yaml.Config
yamlConfig = Yaml.defConfig & setConfCompare compare

friendlyTxBS :: CardanoEra era -> Tx era -> BSC.ByteString
friendlyTxBS era = Yaml.encodePretty yamlConfig . object . friendlyTx era

friendlyTx :: CardanoEra era -> Tx era -> [Aeson.Pair]
friendlyTx era (Tx body witnesses) =
  ("witnesses" .= map friendlyKeyWitness witnesses) : friendlyTxBody era body

friendlyKeyWitness :: KeyWitness era -> Aeson.Value
friendlyKeyWitness =
  object
    . \case
      ByronKeyWitness txInWitness -> ["Byron witness" .= textShow txInWitness]
      ShelleyBootstrapWitness _era bootstrapWitness ->
        ["bootstrap witness" .= textShow bootstrapWitness]
      ShelleyKeyWitness _era (Shelley.WitVKey key signature) ->
        ["key" .= textShow key, "signature" .= textShow signature]

friendlyTxBodyBS :: CardanoEra era -> TxBody era -> BSC.ByteString
friendlyTxBodyBS era =
  Yaml.encodePretty yamlConfig . object . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> TxBody era -> [Aeson.Pair]
friendlyTxBody era (TxBody txBodyContent) = friendlyTxBodyContent era txBodyContent

friendlyTxBodyContentBS :: CardanoEra era -> TxBodyContent view era -> BSC.ByteString
friendlyTxBodyContentBS era =
  Yaml.encodePretty yamlConfig . object . friendlyTxBodyContent era

friendlyTxBodyContent :: forall era view. CardanoEra era -> TxBodyContent view era -> [Aeson.Pair]
friendlyTxBodyContent
  era
  ( TxBodyContent
      { txAuxScripts
      , txCertificates
      , txExtraKeyWits
      , txFee
      , txIns
      , txInsCollateral
      , txTotalCollateral
      , txReturnCollateral
      , txMetadata
      , txMintValue
      , txOuts
      , txUpdateProposal
      , txValidityLowerBound
      , txValidityUpperBound
      , txWithdrawals
      }
    ) =
    [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
    , "certificates" .= friendlyCertificates txCertificates
    , "collateral inputs" .= friendlyCollateralInputs txInsCollateral
    , -- , txTotalCollateral = C.TxTotalCollateralNone
      -- , txReturnCollateral = C.TxReturnCollateralNone
      "total collateral" .= friendlyTxTotalCollateral txTotalCollateral
    , "return collateral" .= withCardanoEra era (friendlyTxReturnCollateral txReturnCollateral)
    , "era" .= era
    , "fee" .= friendlyFee txFee
    , "inputs" .= friendlyInputs txIns
    , "metadata" .= friendlyMetadata txMetadata
    , "mint" .= friendlyMintValue txMintValue
    , "outputs" .= map (withCardanoEra era friendlyTxOut) txOuts
    , "required signers (payment key hashes needed for scripts)"
        .= friendlyExtraKeyWits txExtraKeyWits
    , "update proposal" .= friendlyUpdateProposal txUpdateProposal
    , "validity range" .= friendlyValidityRange (txValidityLowerBound, txValidityUpperBound)
    , "withdrawals" .= friendlyWithdrawals txWithdrawals
    ]

friendlyTxTotalCollateral :: TxTotalCollateral era -> Aeson.Value
friendlyTxTotalCollateral = \case
  TxTotalCollateralNone -> Null
  TxTotalCollateral _ lovelace -> friendlyCoinLovelace lovelace

friendlyTxReturnCollateral :: (IsCardanoEra era) => TxReturnCollateral CtxTx era -> Aeson.Value
friendlyTxReturnCollateral = \case
  TxReturnCollateralNone -> Null
  TxReturnCollateral _ txOut -> friendlyTxOut txOut

withCardanoEra :: forall era a. CardanoEra era -> ((IsCardanoEra era) => a) -> a
withCardanoEra = \case
  ByronEra -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra -> id
  AlonzoEra -> id
  BabbageEra -> id
  ConwayEra -> id

friendlyExtraKeyWits :: TxExtraKeyWitnesses era -> Aeson.Value
friendlyExtraKeyWits = \case
  TxExtraKeyWitnessesNone -> Null
  TxExtraKeyWitnesses _supported paymentKeyHashes ->
    toJSON $ map serialiseToRawBytesHexText paymentKeyHashes

friendlyValidityRange
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> Aeson.Value
friendlyValidityRange = \case
  (TxValidityNoLowerBound, TxValidityUpperBound _ (Just ttl)) -> object ["time to live" .= ttl]
  (TxValidityNoLowerBound, TxValidityUpperBound _ Nothing) -> Null
  (TxValidityLowerBound _ lowerBound, TxValidityUpperBound _ upperBound) ->
    object
      [ "lower bound" .= lowerBound
      , "upper bound" .= maybe Null toJSON upperBound
      ]

friendlyWithdrawals :: forall era view. TxWithdrawals view era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object $
        "address" .= serialiseAddress addr
          : "amount" .= friendlyCoinLovelace amount
          : friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [Aeson.Pair]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential cred
  ]

friendlyTxOut :: forall era. (IsCardanoEra era) => TxOut CtxTx era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum script) =
  object $
    case addr of
      AddressInEra ByronAddressInAnyEra byronAdr ->
        [ "address era" .= String "Byron"
        , "address" .= serialiseAddress byronAdr
        , "amount" .= friendlyTxOutValue amount
        ]
      AddressInEra (ShelleyAddressInEra sbe) saddr@(ShelleyAddress net cred stake) ->
        let preAlonzo =
              friendlyPaymentCredential (fromShelleyPaymentCredential cred)
                : [ "address era" .= Aeson.String "Shelley"
                  , "network" .= net
                  , "address" .= serialiseAddress saddr
                  , "amount" .= friendlyTxOutValue amount
                  , "stake reference"
                      .= friendlyStakeReference (fromShelleyStakeReference stake)
                  ]
            datum =
              [ "datum" .= renderDatum mdatum
              | isJust $ inEonForEraMaybe @AlonzoEraOnwards id $ toCardanoEra sbe
              ]
            sinceAlonzo = ["reference script" .= script]
         in preAlonzo ++ datum ++ sinceAlonzo
  where
    renderDatum :: TxOutDatum CtxTx era -> Aeson.Value
    renderDatum TxOutDatumNone = Aeson.Null
    renderDatum (TxOutDatumHash _ h) =
      Aeson.String $ serialiseToRawBytesHexText h
    renderDatum (TxOutDatumInTx _ sData) =
      scriptDataToJson ScriptDataJsonDetailedSchema sData
    renderDatum (TxOutDatumInline _ sData) =
      A.object
        [("inline", scriptDataToJson ScriptDataJsonDetailedSchema sData)]

friendlyStakeReference :: StakeAddressReference -> Aeson.Value
friendlyStakeReference = \case
  NoStakeAddress -> Null
  StakeAddressByPointer ptr -> String (textShow ptr)
  StakeAddressByValue cred -> object [friendlyStakeCredential $ toShelleyStakeCredential cred]

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ (UpdateProposal parameterUpdates epoch) ->
    object
      [ "epoch" .= epoch
      , "updates"
          .= [ object
                 [ "genesis key hash" .= serialiseToRawBytesHexText genesisKeyHash
                 , "update" .= friendlyProtocolParametersUpdate parameterUpdate
                 ]
             | (genesisKeyHash, parameterUpdate) <- Map.assocs parameterUpdates
             ]
      ]

friendlyProtocolParametersUpdate :: ProtocolParametersUpdate -> Aeson.Value
friendlyProtocolParametersUpdate
  ProtocolParametersUpdate
    { protocolUpdateProtocolVersion
    , protocolUpdateDecentralization
    , protocolUpdateExtraPraosEntropy
    , protocolUpdateMaxBlockHeaderSize
    , protocolUpdateMaxBlockBodySize
    , protocolUpdateMaxTxSize
    , protocolUpdateTxFeeFixed
    , protocolUpdateTxFeePerByte
    , protocolUpdateMinUTxOValue
    , protocolUpdateStakeAddressDeposit
    , protocolUpdateStakePoolDeposit
    , protocolUpdateMinPoolCost
    , protocolUpdatePoolRetireMaxEpoch
    , protocolUpdateStakePoolTargetNum
    , protocolUpdatePoolPledgeInfluence
    , protocolUpdateMonetaryExpansion
    , protocolUpdateTreasuryCut
    , protocolUpdateCollateralPercent
    , protocolUpdateMaxBlockExUnits
    , protocolUpdateMaxCollateralInputs
    , protocolUpdateMaxTxExUnits
    , protocolUpdateMaxValueSize
    , protocolUpdatePrices
    , protocolUpdateUTxOCostPerByte
    } =
    object . catMaybes $
      [ protocolUpdateProtocolVersion <&> \(major, minor) ->
          "protocol version" .= (textShow major <> "." <> textShow minor)
      , protocolUpdateDecentralization
          <&> ("decentralization parameter" .=) . friendlyRational
      , protocolUpdateExtraPraosEntropy
          <&> ("extra entropy" .=) . maybe "reset" toJSON
      , protocolUpdateMaxBlockHeaderSize <&> ("max block header size" .=)
      , protocolUpdateMaxBlockBodySize <&> ("max block body size" .=)
      , protocolUpdateMaxTxSize <&> ("max transaction size" .=)
      , protocolUpdateTxFeeFixed <&> ("transaction fee constant" .=)
      , protocolUpdateTxFeePerByte <&> ("transaction fee linear per byte" .=)
      , protocolUpdateMinUTxOValue <&> ("min UTxO value" .=) . friendlyCoinLovelace
      , protocolUpdateStakeAddressDeposit
          <&> ("key registration deposit" .=) . friendlyCoinLovelace
      , protocolUpdateStakePoolDeposit
          <&> ("pool registration deposit" .=) . friendlyCoinLovelace
      , protocolUpdateMinPoolCost <&> ("min pool cost" .=) . friendlyCoinLovelace
      , protocolUpdatePoolRetireMaxEpoch <&> ("pool retirement epoch boundary" .=)
      , protocolUpdateStakePoolTargetNum <&> ("number of pools" .=)
      , protocolUpdatePoolPledgeInfluence
          <&> ("pool influence" .=) . friendlyRational
      , protocolUpdateMonetaryExpansion
          <&> ("monetary expansion" .=) . friendlyRational
      , protocolUpdateTreasuryCut <&> ("treasury expansion" .=) . friendlyRational
      , protocolUpdateCollateralPercent
          <&> ("collateral inputs share" .=) . (<> "%") . textShow
      , protocolUpdateMaxBlockExUnits <&> ("max block execution units" .=)
      , protocolUpdateMaxCollateralInputs <&> ("max collateral inputs" .=)
      , protocolUpdateMaxTxExUnits <&> ("max transaction execution units" .=)
      , protocolUpdateMaxValueSize <&> ("max value size" .=)
      , protocolUpdatePrices <&> ("execution prices" .=) . friendlyPrices
      , protocolUpdateUTxOCostPerByte
          <&> ("UTxO storage cost per byte" .=) . friendlyCoinLovelace
      ]

friendlyPrices :: ExecutionUnitPrices -> Aeson.Value
friendlyPrices ExecutionUnitPrices{priceExecutionMemory, priceExecutionSteps} =
  object
    [ "memory" .= friendlyRational priceExecutionMemory
    , "steps" .= friendlyRational priceExecutionSteps
    ]

friendlyCertificates :: forall era view. TxCertificates view era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone -> Null
  TxCertificates ShelleyBasedEraShelley cs _ -> array $ map friendlyCertificate cs
  TxCertificates ShelleyBasedEraAllegra cs _ -> array $ map friendlyCertificate cs
  TxCertificates ShelleyBasedEraMary cs _ -> array $ map friendlyCertificate cs
  TxCertificates ShelleyBasedEraAlonzo cs _ -> array $ map friendlyCertificate cs
  TxCertificates ShelleyBasedEraBabbage cs _ -> array $ map friendlyCertificate cs
  TxCertificates ShelleyBasedEraConway cs _ -> array $ map friendlyCertificate cs

friendlyCertificate
  :: Certificate era
  -> Aeson.Value
friendlyCertificate =
  object . (: []) . \case
    ShelleyRelatedCertificate _ cert -> case cert of
      ShelleyTxCertDelegCert (Shelley.ShelleyRegCert credential) ->
        "stake address registration" .= object [friendlyStakeCredential credential]
      ShelleyTxCertDelegCert (Shelley.ShelleyUnRegCert credential) ->
        "stake address deregistration" .= object [friendlyStakeCredential credential]
      ShelleyTxCertDelegCert (Shelley.ShelleyDelegCert credential poolId) ->
        "stake address delegation" .= object [friendlyStakeCredential credential, "pool" .= poolId]
      ShelleyTxCertPool (Shelley.RegPool parameters) ->
        "stake pool registration" .= friendlyStakePoolParameters (fromShelleyPoolParams parameters)
      ShelleyTxCertPool (Shelley.RetirePool poolId epochNo) ->
        "stake pool retirement" .= object ["pool" .= poolId, "epoch" .= epochNo]
      ShelleyTxCertGenesisDeleg (Shelley.GenesisDelegCert genesisKeyHash delegateKeyHash vrfKeyHash) ->
        "genesis key delegation"
          .= object
            [ "genesis key hash" .= genesisKeyHash
            , "delegate key hash" .= delegateKeyHash
            , "VRF key hash" .= vrfKeyHash
            ]
      ShelleyTxCertMir (Shelley.MIRCert pot target) ->
        "MIR" .= object ["pot" .= friendlyMirPot pot, friendlyMirTarget target]
    ConwayCertificate ConwayEraOnwardsConway _ -> error "FIXME handle conway certs"

friendlyMirTarget :: (Crypto.Crypto era) => MIRTarget era -> Aeson.Pair
friendlyMirTarget = \case
  StakeAddressesMIR addresses ->
    "target stake addresses"
      .= [ object
             [ friendlyStakeCredential credential
             , "amount" .= friendlyCoinLovelace (Coin.addDeltaCoin (Coin.Coin 0) lovelace)
             ]
         | (credential, lovelace) <- Map.toList addresses
         ]
  SendToOppositePotMIR amount -> "send to reserves" .= friendlyCoinLovelace amount

friendlyStakeCredential :: (Crypto.Crypto era) => Shelley.Credential 'Shelley.Staking era -> Aeson.Pair
friendlyStakeCredential = \case
  Shelley.KeyHashObj keyHash ->
    "stake credential key hash" .= keyHash
  Shelley.ScriptHashObj scriptHash ->
    "stake credential script hash" .= scriptHash

friendlyPaymentCredential :: PaymentCredential -> Aeson.Pair
friendlyPaymentCredential = \case
  PaymentCredentialByKey keyHash ->
    "payment credential key hash" .= serialiseToRawBytesHexText keyHash
  PaymentCredentialByScript scriptHash ->
    "payment credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyMirPot :: Shelley.MIRPot -> Aeson.Value
friendlyMirPot = \case
  Shelley.ReservesMIR -> "reserves"
  Shelley.TreasuryMIR -> "treasury"

friendlyStakePoolParameters :: StakePoolParameters -> Aeson.Value
friendlyStakePoolParameters
  StakePoolParameters
    { stakePoolId
    , stakePoolVRF
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
    object
      [ "pool" .= stakePoolId
      , "VRF key hash" .= serialiseToRawBytesHexText stakePoolVRF
      , "cost" .= friendlyCoinLovelace stakePoolCost
      , "margin" .= friendlyRational stakePoolMargin
      , "reward account" .= object (friendlyStakeAddress stakePoolRewardAccount)
      , "pledge" .= friendlyCoinLovelace stakePoolPledge
      , "owners (stake key hashes)"
          .= map serialiseToRawBytesHexText stakePoolOwners
      , "relays" .= map textShow stakePoolRelays
      , "metadata" .= fmap textShow stakePoolMetadata
      ]

friendlyRational :: Rational -> Aeson.Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
  where
    n = numerator r
    d = denominator r

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeExplicit _ fee -> friendlyCoinLovelace fee

friendlyCoinLovelace :: Coin.Coin -> Aeson.Value
friendlyCoinLovelace (Coin.Coin value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: forall era view. TxMintValue view era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone -> Null
  TxMintValue _ v _ -> friendlyValue v

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutValueByron lovelace -> friendlyCoinLovelace lovelace
  TxOutValueShelleyBased era v -> friendlyValue $ fromLedgerValue era v

friendlyValue :: Api.Value -> Aeson.Value
friendlyValue v =
  object
    [ case bundle of
        ValueNestedBundleAda q -> "lovelace" .= q
        ValueNestedBundle policy assets ->
          Aeson.fromText (friendlyPolicyId policy) .= friendlyAssets assets
    | bundle <- bundles
    ]
  where
    ValueNestedRep bundles = valueToNestedRep v

    friendlyPolicyId = ("policy " <>) . serialiseToRawBytesHexText

    friendlyAssets = Map.mapKeys friendlyAssetName

    friendlyAssetName = \case
      "" -> "default asset"
      name@(AssetName nameBS) ->
        "asset " <> serialiseToRawBytesHexText name <> nameAsciiSuffix
        where
          nameAsciiSuffix
            | nameIsAscii = " (" <> nameAscii <> ")"
            | otherwise = ""
          nameIsAscii = BSC.all (\c -> isAscii c && isAlphaNum c) nameBS
          nameAscii = Text.pack $ BSC.unpack nameBS

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int -> toJSON int
  TxMetaBytes bytes -> String $ textShow bytes
  TxMetaList lst -> array $ map friendlyMetadataValue lst
  TxMetaMap m ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText text -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst

friendlyCollateralInputs :: TxInsCollateral era -> Aeson.Value
friendlyCollateralInputs = \case
  TxInsCollateralNone -> Null
  TxInsCollateral _ txins -> toJSON txins
