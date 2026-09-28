{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Export information for Marlowe contracts and roles.
-- | WARNING!
-- | This module was mechanically migrated (by human :-P)
-- | after the new package arrangement where marlowe-binaries
-- | compiles and outputs the scripts.
-- | It is unclear if we need this module or not.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.CLI.Export (
  MarloweValidator (..),
  PayoutValidator (..),

  buildAddress,
  buildValidatorInfo,
  exportAddress,

  -- * Contract and Transaction
  buildMarlowe,
  exportMarlowe,
  printMarlowe,

  -- * Address
  buildMarloweAddress,
  exportMarloweAddress,

  -- * Validator
  exportMarloweValidator,
  marloweValidatorInfo,

  -- * Datum
  buildMarloweDatum,
  exportDatum,

  -- * Redeemer
  buildRedeemer,
  exportRedeemer,

  -- * Roles Address
  buildRoleAddress,
  exportRoleAddress,

  -- * Role Validator
  exportRoleValidator,
  payoutValidatorInfo,
  openRoleValidatorInfo,

  -- * Role Datum
  buildRoleDatum,
  exportRoleDatum,

  -- * Role Redeemer
  buildRoleRedeemer,
  exportRoleRedeemer,
) where

import Cardano.Api (
  AddressInEra,
  BabbageEraOnwards,
  NetworkId,
  PaymentCredential (..),
  Script (PlutusScript),
  ScriptDataJsonSchema (..),
  SerialiseAsRawBytes (..),
  StakeAddressReference (..),
  ToCardanoEra (toCardanoEra),
  convert,
  cardanoEraConstraints,
  hashScript,
  hashScriptDataBytes,
  makeShelleyAddressInEra,
  scriptDataToJson,
  serialiseAddress,
  unsafeHashableScriptData,
 )
import Cardano.Api (fromPlutusData)
import Control.Monad (join, when)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode)
import Language.Marlowe.CLI.IO (
  decodeFileStrict,
  getMajorProtocolVersion,
  getPV2CostModelParams,
  maybeWriteJson,
  maybeWriteTextEnvelope,
 )
import Language.Marlowe.CLI.Types (
  CliEnv,
  CliError (..),
  DatumInfo (..),
  MarloweInfo (..),
  QueryExecutionContext (..),
  RedeemerInfo (..),
  ValidatorInfo (..),
  askEra,
  asksEra,
  doWithCardanoEra,
  doWithShelleyBasedEra,
  queryContextNetworkId,
  validatorAddress,
  validatorInfo',
 )
import Marlowe.Plutus.Semantics (MarloweData (..), MarloweParams)
import Marlowe.Plutus.Semantics.Types (Contract (..), Input, State (..), Token (Token))
import PlutusLedgerApi.V2 (BuiltinData, Datum (..), Redeemer (..))
import PlutusTx (builtinDataToData, toBuiltinData)
import System.IO (hPutStrLn, stderr)

import Data.ByteString.Lazy qualified as LBS (toStrict)
import Data.ByteString.Lazy.Char8 qualified as LBS8 (unpack)
import Data.Text qualified as T (unpack)

import Cardano.Api qualified as C
import Codec.Serialise (serialise)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Short qualified as SBS
import Marlowe.Plutus.Scripts.Types (marloweTxInputsFromInputs)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (withPlutusScriptVersion)
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.V1 (DatumHash (..), toBuiltin, toData)
import Language.Marlowe.CLI.Scripts (MarloweValidator (..), OpenRolesValidator (..), PayoutValidator (..), RoleValidator (..))

-- | Build comprehensive information about a Marlowe contract and transaction.
buildMarlowe
  :: (MonadIO m)
  => MarloweValidator
  -> MarloweParams
  -> BabbageEraOnwards era
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> [Input]
  -- ^ The contract's input,
  -> m (Either CliError (MarloweInfo C.PlutusScriptV3 era))
  -- ^ The contract and transaction information, or an error message.
buildMarlowe (MarloweValidator marloweValidator) marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    liftIO $ print ("Building marlowe" :: String)
    let
      vi = do
        miValidatorInfo <-
          validatorInfo' marloweValidator Nothing era protocolVersion costModel network stake
        let miDatumInfo = buildMarloweDatum marloweParams contract state
            miRedeemerInfo = buildRedeemer inputs
        pure MarloweInfo{..}
    liftIO $ print vi
    pure vi

-- | Export to a file the comprehensive information about a Marlowe contract and transaction.
exportMarlowe
  :: forall m era
   . (MonadError CliError m, MonadIO m, MonadReader (CliEnv era) m)
  => MarloweValidator
  -> MarloweParams
  -- ^ The Marlowe contract parameters.
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> [FilePath]
  -- ^ The JSON files containing the contract's inputs.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics about the contract.
  -> m ()
  -- ^ Action to export the contract and transaction information to a file.
exportMarlowe marloweValidator marloweParams protocolVersion costModel network stake contractFile stateFile inputFiles outputFile printStats =
  do
    contract :: Contract <- decodeFileStrict contractFile
    state :: State <- decodeFileStrict stateFile
    inputs :: [Input] <- mapM decodeFileStrict inputFiles
    marloweInfo@MarloweInfo{..} <-
      liftEither
        =<< join
          (asksEra \era -> buildMarlowe @m marloweValidator marloweParams era protocolVersion costModel network stake contract state inputs)
    let ValidatorInfo{..} = miValidatorInfo
        DatumInfo{..} = miDatumInfo
        RedeemerInfo{..} = miRedeemerInfo
    doWithShelleyBasedEra $ maybeWriteJson outputFile marloweInfo
    liftIO
      . when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Validator size: " <> show (SBS.length viBytes)
        case viTxIn of
          Just txIn -> hPutStrLn stderr $ "Validator script reference: " <> show txIn
          Nothing -> pure ()
        hPutStrLn stderr $ "Datum size: " <> show diSize
        hPutStrLn stderr $ "Redeemer size: " <> show riSize
        hPutStrLn stderr $ "Total size: " <> show (SBS.length viBytes + diSize + riSize)

-- | Print information about a Marlowe contract and transaction.
printMarlowe
  :: forall m lang era
   . (MonadError CliError m, MonadIO m, C.IsPlutusScriptLanguage lang)
  => MarloweValidator
  -> MarloweParams
  -- ^ The Marlowe contract parameters.
  -> BabbageEraOnwards era
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> [Input]
  -- ^ The contract's input,
  -> m ()
  -- ^ Action to print the contract and transaction information.
printMarlowe marloweValidator marloweParams era protocolVersion costModel network stake contract state inputs =
  do
    MarloweInfo{..} <-
      liftEither =<< buildMarlowe @m marloweValidator marloweParams era protocolVersion costModel network stake contract state inputs
    let ValidatorInfo{..} = miValidatorInfo
        DatumInfo{..} = miDatumInfo
        RedeemerInfo{..} = miRedeemerInfo
    liftIO $
      do
        putStrLn ""
        putStrLn $ "Contract: " <> show contract
        putStrLn ""
        putStrLn $ "State: " <> show state
        putStrLn ""
        putStrLn $ "Inputs: " <> show inputs
        putStrLn ""
        putStrLn $
          "Validator: "
            <> LBS8.unpack
              (withPlutusScriptVersion (C.plutusScriptVersion @lang) $ encode $ C.serialiseToTextEnvelope Nothing viScript)
        putStrLn ""
        putStrLn $
          "Validator address: "
            <> T.unpack (cardanoEraConstraints (toCardanoEra era) $ serialiseAddress (validatorAddress viScript era network stake))
        putStrLn ""
        putStrLn $ "Validator hash: " <> show viHash
        putStrLn ""
        putStrLn $ "Validator size: " <> show (SBS.length viBytes)
        putStrLn ""
        case viTxIn of
          Just txIn -> do
            putStrLn $ "Validator script reference: " <> show txIn
            putStrLn ""
          Nothing -> pure ()
        putStrLn ""
        putStrLn $ "Datum:" <> LBS8.unpack (encode diJson)
        putStrLn ""
        putStrLn $ "Datum hash: " <> show diHash
        putStrLn ""
        putStrLn $ "Datum size: " <> show diSize
        putStrLn ""
        putStrLn $ "Redeemer: " <> LBS8.unpack (encode riJson)
        putStrLn ""
        putStrLn $ "Redeemer size: " <> show riSize
        putStrLn ""
        putStrLn $ "Total size: " <> show (SBS.length viBytes + diSize + riSize)

-- | Compute the address of a validator.
buildAddress
  :: forall lang era
   . (C.IsPlutusScriptLanguage lang)
  => C.PlutusScript lang
  -- ^ The validator.
  -> BabbageEraOnwards era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> AddressInEra era
  -- ^ The script address.
buildAddress script era network stake =
  let viScript = PlutusScript C.plutusScriptVersion script
   in makeShelleyAddressInEra
        (convert era)
        network
        (PaymentCredentialByScript $ hashScript viScript)
        stake

-- | Compute the address of a Marlowe contract.
buildMarloweAddress
  :: forall m era
   . (MonadIO m)
  => MarloweValidator
  -> BabbageEraOnwards era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (AddressInEra era)
  -- ^ The script address.
buildMarloweAddress (MarloweValidator marloweValidator) era network stake = do
  pure $ buildAddress marloweValidator era network stake

-- | Print the address of a validator.
exportAddress
  :: forall era lang m
   . (MonadIO m, MonadReader (CliEnv era) m, C.IsPlutusScriptLanguage lang)
  => C.PlutusScript lang
  -- ^ The validator.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportAddress validator network stake = do
  era <- askEra
  let address = buildAddress validator era network stake
  doWithShelleyBasedEra $ liftIO $ putStrLn $ T.unpack $ serialiseAddress address

-- | Print the address of a Marlowe contract.
exportMarloweAddress
  :: forall m era
   . (MonadIO m, MonadReader (CliEnv era) m)
  => MarloweValidator
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportMarloweAddress (MarloweValidator marloweValidator) network stake = do
  exportAddress marloweValidator network stake

buildValidatorInfo
  :: (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (MonadError CliError m)
  => (C.IsPlutusScriptLanguage lang)
  => QueryExecutionContext era
  -> C.PlutusScript lang
  -> Maybe C.TxIn
  -> StakeAddressReference
  -> m (ValidatorInfo lang era)
buildValidatorInfo queryCtx plutusScript txIn stake = do
  era <- askEra
  costModel <- getPV2CostModelParams queryCtx
  protocolVersion <- getMajorProtocolVersion queryCtx
  validatorInfo' plutusScript txIn era protocolVersion costModel (queryContextNetworkId queryCtx) stake

-- | Export to a file the validator information.
exportValidatorImpl
  :: forall lang era m
   . (MonadError CliError m, MonadReader (CliEnv era) m, C.IsPlutusScriptLanguage lang)
  => (MonadIO m)
  => C.PlutusScript lang
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportValidatorImpl plutusScript protocolVersion costModel network stake outputFile printHash printStats =
  do
    era <- askEra
    ValidatorInfo{..} <- validatorInfo' plutusScript Nothing era protocolVersion costModel network stake
    maybeWriteTextEnvelope outputFile $ C.PlutusScript C.plutusScriptVersion plutusScript
    doWithCardanoEra $
      liftIO $
        do
          hPutStrLn stderr $ T.unpack $ serialiseAddress (validatorAddress viScript era network stake)
          when printHash $
            do
              hPutStrLn stderr ""
              hPutStrLn stderr $ "Validator hash: " <> show viHash
              case viTxIn of
                Just txIn -> do
                  hPutStrLn stderr ""
                  hPutStrLn stderr $ "Validator script reference: " <> show txIn
                Nothing -> pure ()
          when printStats $
            do
              hPutStrLn stderr ""
              hPutStrLn stderr $ "Validator size: " <> show (SBS.length viBytes)

-- | Current Marlowe validator information.
marloweValidatorInfo
  :: (MonadIO m)
  => MarloweValidator
  -> BabbageEraOnwards era
  -- ^ The era to build he validator in.
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo C.PlutusScriptV3 era))
  -- ^ The validator information, or an error message.
marloweValidatorInfo (MarloweValidator marloweValidator) script prot costModel network stake = do
  pure $ validatorInfo' marloweValidator Nothing script prot costModel network stake

-- | Export to a file the validator information about a Marlowe contract.
exportMarloweValidator
  :: forall era m
   . (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => MarloweValidator
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportMarloweValidator (MarloweValidator marloweValidator) prot costModel network stake out printHash printStats = do
  exportValidatorImpl marloweValidator prot costModel network stake out printHash printStats

-- | Build the datum information about a Marlowe transaction.
buildDatumImpl
  :: BuiltinData
  -- ^ The datum.
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildDatumImpl datum =
  let diDatum = Datum datum
      diBytes = SBS.toShort . LBS.toStrict . serialise $ diDatum
      diJson =
        scriptDataToJson ScriptDataJsonDetailedSchema
          . unsafeHashableScriptData
          . fromPlutusData
          $ PlutusTx.builtinDataToData datum
      diHash =
        DatumHash . toBuiltin . serialiseToRawBytes . hashScriptDataBytes . unsafeHashableScriptData . fromPlutusData $
          toData diDatum
      diSize = SBS.length diBytes
   in DatumInfo{..}

-- | Build the datum information about a Marlowe transaction.
buildMarloweDatum
  :: MarloweParams
  -> Contract
  -- ^ The contract.
  -> State
  -- ^ The contract's state.
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildMarloweDatum marloweParams marloweContract marloweState =
  let marloweData = MarloweData{..}
      marloweDatum = PlutusTx.toBuiltinData marloweData
   in buildDatumImpl marloweDatum

-- | Export to a file the datum information about a Marlowe transaction.
exportDatumImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => BuiltinData
  -- ^ The datum
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportDatumImpl datum outputFile printStats =
  do
    let DatumInfo{..} = buildDatumImpl datum
    maybeWriteJson outputFile diJson
    liftIO $
      do
        print diHash
        when printStats $
          do
            hPutStrLn stderr ""
            hPutStrLn stderr $ "Datum size: " <> show diSize

-- | Export to a file the datum information about a Marlowe transaction.
exportDatum
  :: (MonadError CliError m)
  => (MonadIO m)
  => MarloweParams
  -- ^ `MarloweParams` used by the validator.
  -> FilePath
  -- ^ The JSON file containing the contract.
  -> FilePath
  -- ^ The JSON file containing the contract's state.
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportDatum marloweParams contractFile stateFile outputFile printStats =
  do
    marloweContract <- decodeFileStrict contractFile
    marloweState <- decodeFileStrict stateFile
    let marloweData = MarloweData{..}
        marloweDatum = PlutusTx.toBuiltinData marloweData
    exportDatumImpl marloweDatum outputFile printStats

-- | Build the redeemer information about a Marlowe transaction.
buildRedeemerImpl
  :: BuiltinData
  -- ^ The redeemer.
  -> RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRedeemerImpl redeemer =
  let riRedeemer = Redeemer redeemer
      riBytes = SBS.toShort . LBS.toStrict . serialise $ riRedeemer
      riJson =
        scriptDataToJson ScriptDataJsonDetailedSchema
          . unsafeHashableScriptData
          . fromPlutusData
          $ PlutusTx.builtinDataToData redeemer
      riSize = SBS.length riBytes
   in RedeemerInfo{..}

-- | Build the redeemer information about a Marlowe transaction.
buildRedeemer
  :: [Input]
  -- ^ The contract's input,
  -> RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRedeemer = buildRedeemerImpl . PlutusTx.toBuiltinData . marloweTxInputsFromInputs

-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemerImpl
  :: (MonadError CliError m)
  => (MonadIO m)
  => BuiltinData
  -- ^ The redeemer.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRedeemerImpl redeemer outputFile printStats =
  do
    let RedeemerInfo{..} = buildRedeemerImpl redeemer
    maybeWriteJson outputFile riJson
    liftIO
      . when printStats
      $ do
        hPutStrLn stderr ""
        hPutStrLn stderr $ "Redeemer size: " <> show riSize

-- | Export to a file the redeemer information about a Marlowe transaction.
exportRedeemer
  :: (MonadError CliError m)
  => (MonadIO m)
  => [FilePath]
  -- ^ The files containing the contract's inputs.
  -> Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRedeemer inputFiles outputFile printStats =
  do
    inputs <- mapM decodeFileStrict inputFiles
    exportRedeemerImpl (PlutusTx.toBuiltinData (inputs :: [Input])) outputFile printStats

-- -- | Compute the role address of a Marlowe contract.
buildRoleAddress
  :: forall era m
   . (MonadIO m)
  => PayoutValidator
  -> BabbageEraOnwards era
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (AddressInEra era)
  -- ^ The script address.
buildRoleAddress (PayoutValidator payoutValidator) era network stake = do
  pure $ buildAddress payoutValidator era network stake

-- | Print the role address of a Marlowe contract.
exportRoleAddress
  :: forall era m
   . (MonadIO m, MonadReader (CliEnv era) m)
  => RoleValidator
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m ()
  -- ^ Action to print the script address.
exportRoleAddress (RoleValidator roleValidator) network stake = do
  exportAddress roleValidator network stake

-- | Current Marlowe validator information.
payoutValidatorInfo
  :: (MonadIO m)
  => PayoutValidator
  -> BabbageEraOnwards era
  -- ^ The era to build he validator in.
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo C.PlutusScriptV3 era))
  -- ^ The validator information, or an error message.
payoutValidatorInfo (PayoutValidator payoutValidator) era prot cost network stake = do
  pure $ validatorInfo' payoutValidator Nothing era prot cost network stake

-- | Open role validator
openRoleValidatorInfo
  :: (MonadIO m)
  => OpenRolesValidator
  -> BabbageEraOnwards era
  -- ^ The era to build he validator in.
  -> MajorProtocolVersion
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> m (Either CliError (ValidatorInfo C.PlutusScriptV3 era))
  -- ^ The validator information, or an error message.
openRoleValidatorInfo (OpenRolesValidator openRolesValidator) era prot cost network stake = do
  pure $ validatorInfo' openRolesValidator Nothing era prot cost network stake

-- | Export to a file the role validator information about a Marlowe contract.
exportRoleValidator
  :: forall era m
   . (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => PayoutValidator
  -> MajorProtocolVersion
  -- ^ The currency symbol for Marlowe contract roles.
  -> [Integer]
  -- ^ The cost model parameters.
  -> NetworkId
  -- ^ The network ID.
  -> StakeAddressReference
  -- ^ The stake address.
  -> Maybe FilePath
  -- ^ The output JSON file for the validator information.
  -> Bool
  -- ^ Whether to print the validator hash.
  -> Bool
  -- ^ Whether to print statistics about the validator.
  -> m ()
  -- ^ Action to export the validator information to a file.
exportRoleValidator (PayoutValidator payoutValidator) prot cost network stake out printHash printStats = do
  exportValidatorImpl payoutValidator prot cost network stake out printHash printStats

-- | Build the role datum information about a Marlowe transaction.
buildRoleDatum
  :: Token
  -> DatumInfo
  -- ^ Information about the transaction datum.
buildRoleDatum (Token currencySymbol tokenName) = buildDatumImpl $ PlutusTx.toBuiltinData (currencySymbol, tokenName)

-- | Export to a file the role datum information about a Marlowe transaction.
exportRoleDatum
  :: (MonadError CliError m)
  => (MonadIO m)
  => Token
  -- ^ The role token.
  -> Maybe FilePath
  -- ^ The output JSON file for the datum information.
  -> Bool
  -- ^ Whether to print statistics about the datum.
  -> m ()
  -- ^ Action to export the datum information to a file.
exportRoleDatum = exportDatumImpl . PlutusTx.toBuiltinData

-- | Build the role redeemer information about a Marlowe transaction.
buildRoleRedeemer :: RedeemerInfo
  -- ^ Information about the transaction redeemer.
buildRoleRedeemer = buildRedeemerImpl $ PlutusTx.toBuiltinData ()

-- | Export to a file the role redeemer information about a Marlowe transaction.
exportRoleRedeemer
  :: (MonadError CliError m)
  => (MonadIO m)
  => Maybe FilePath
  -- ^ The output JSON file for Marlowe contract information.
  -> Bool
  -- ^ Whether to print statistics on the contract.
  -> m ()
  -- ^ Action to export the redeemer information to a file.
exportRoleRedeemer = exportRedeemerImpl $ PlutusTx.toBuiltinData ()
