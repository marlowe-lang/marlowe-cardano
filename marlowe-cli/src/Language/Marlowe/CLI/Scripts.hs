{-# LANGUAGE OverloadedStrings #-}

-- | Marlowe Plutus script binaries (loaded from external plutus text
-- envelopes) and helpers for resolving the bundle of validator scripts
-- from a combination of @--*-script-file@ overrides and a
-- @--scripts-suite-file@ bundle.
module Language.Marlowe.CLI.Scripts (
  -- * Script newtypes
  MarloweValidator (..),
  PayoutValidator (..),
  OpenRolesValidator (..),
  RoleValidator (..),

  -- * Bundle
  MarloweScripts,
  MarloweScriptsPaths (..),

  -- * Loading
  loadMarloweScriptsPaths,
  mkMarloweScriptsInfo,
  readMarloweScriptsSuite,
  readPlutusScriptV3,
  resolveMarloweScriptsPaths,
) where

import Cardano.Api (
  PlutusScript,
  PlutusScriptV3,
  Script (..),
  readFileTextEnvelope,
 )
import Cardano.Api qualified as C
import Control.Exception (IOException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Yaml as Yaml
import Language.Marlowe.CLI.Types (
  CliError (..),
  MarloweScriptsInfo (..),
  ValidatorInfo (..),
 )
import Marlowe.Plutus.Binaries.Api.Compile (ScriptOutput (scriptFile), ScriptsSuite (..))

-- | A Marlowe validator script bundle (loaded from a Plutus text envelope).
newtype MarloweValidator = MarloweValidator (C.PlutusScript C.PlutusScriptV3)

-- | A Marlowe role-payout validator script bundle (loaded from a Plutus text envelope).
newtype PayoutValidator = PayoutValidator (C.PlutusScript C.PlutusScriptV3)

-- | A role-token minting policy bundle (loaded from a Plutus text envelope).
newtype RoleValidator = RoleValidator (C.PlutusScript C.PlutusScriptV3)

-- | An open-roles validator script bundle (loaded from a Plutus text envelope).
newtype OpenRolesValidator = OpenRolesValidator (C.PlutusScript C.PlutusScriptV3)

-- | The bundle of Marlowe Plutus scripts. The role-token minting policy
-- is optional.
type MarloweScripts = (MarloweValidator, PayoutValidator, OpenRolesValidator, Maybe RoleValidator)

-- | Raw, unresolved file paths for the bundle of Marlowe scripts.
data MarloweScriptsPaths = MarloweScriptsPaths
  { marloweScriptPath :: Maybe FilePath
  -- ^ Path to the marlowe semantics script.
  , payoutScriptPath :: Maybe FilePath
  -- ^ Path to the marlowe role-payout script.
  , openRolesScriptPath :: Maybe FilePath
  -- ^ Path to the open-roles validator.
  , roleTokensScriptPath :: Maybe FilePath
  -- ^ Path to the role-token minting policy, if any.
  }

-- | Read a Plutus text envelope as a Plutus V3 script.
readPlutusScriptV3 :: FilePath -> IO (Either CliError (PlutusScript PlutusScriptV3))
readPlutusScriptV3 filePath = do
  ioResult <-
    try (readFileTextEnvelope (C.File filePath) :: IO (Either (C.FileError C.TextEnvelopeError) (Script PlutusScriptV3)))
  pure $ case ioResult of
    Left (e :: IOException) ->
      Left
        $ CliError
        $ "Could not read Plutus text envelope at "
        <> filePath
        <> ": "
        <> show e
    Right (Left err) ->
      Left
        $ CliError
        $ "Could not decode Plutus text envelope at "
        <> filePath
        <> ": "
        <> show err
    Right (Right (C.PlutusScript _ script)) ->
      Right script

-- | Read a 'ScriptsSuite' document (JSON or YAML).
readMarloweScriptsSuite :: FilePath -> IO (Either CliError ScriptsSuite)
readMarloweScriptsSuite path = do
  contentsResult <- try (LBS.readFile path)
  case contentsResult of
    Left (e :: IOException) ->
      pure
        $ Left
        $ CliError
        $ "Could not read ScriptsSuite file "
        <> path
        <> ": "
        <> show e
    Right contents ->
      pure $ case Aeson.eitherDecode contents of
        Right s -> Right s
        Left jsonErr ->
          case Yaml.decodeEither' (LBS.toStrict contents) of
            Right s -> Right s
            Left yamlErr ->
              Left
                $ CliError
                $ "Could not parse ScriptsSuite at "
                <> path
                <> ". JSON error: "
                <> jsonErr
                <> ". YAML error: "
                <> show yamlErr

-- | Resolve script file paths either from a 'ScriptsSuite' or from
-- individual @--*-script-file@ overrides (the overrides win).
resolveMarloweScriptsPaths :: Maybe ScriptsSuite -> MarloweScriptsPaths -> MarloweScriptsPaths
resolveMarloweScriptsPaths mSuite MarloweScriptsPaths{..} =
  MarloweScriptsPaths
    { marloweScriptPath = pick marloweScriptPath (scriptFile . marloweSemantics <$> mSuite)
    , payoutScriptPath = pick payoutScriptPath (scriptFile . marloweRolePayout <$> mSuite)
    , openRolesScriptPath = pick openRolesScriptPath (scriptFile . openRoles <$> mSuite)
    , roleTokensScriptPath = pick roleTokensScriptPath (scriptFile <$> joinRoleTokens mSuite)
    }
  where
    pick (Just p) _ = Just p
    pick Nothing (Just p) = Just p
    pick Nothing Nothing = Nothing
    joinRoleTokens :: Maybe ScriptsSuite -> Maybe ScriptOutput
    joinRoleTokens Nothing = Nothing
    joinRoleTokens (Just s) = roleTokens s

-- | Load the bundle of Plutus scripts corresponding to a resolved
-- 'MarloweScriptsPaths'. Missing required scripts produce a 'CliError'.
loadMarloweScriptsPaths :: MarloweScriptsPaths -> IO (Either CliError MarloweScripts)
loadMarloweScriptsPaths MarloweScriptsPaths{..} = do
  mv <- loadOne "marlowe" marloweScriptPath MarloweValidator
  pv <- loadOne "payout" payoutScriptPath PayoutValidator
  ov <- loadOne "open-roles" openRolesScriptPath OpenRolesValidator
  case (mv, pv, ov) of
    (Left err, _, _) -> pure (Left err)
    (_, Left err, _) -> pure (Left err)
    (_, _, Left err) -> pure (Left err)
    (Right m, Right p, Right o) -> do
      mr <- case roleTokensScriptPath of
        Nothing -> pure $ Right Nothing
        Just path -> fmap (fmap Just . fmap RoleValidator) (readPlutusScriptV3 path)
      pure $ case mr of
        Left err -> Left err
        Right rt -> Right (m, p, o, rt)
  where
    loadOne
      :: String
      -> Maybe FilePath
      -> (PlutusScript PlutusScriptV3 -> wrapper)
      -> IO (Either CliError wrapper)
    loadOne field Nothing _ =
      pure
        $ Left
        $ CliError
        $ "Missing required script: "
          <> field
          <> ". Provide --scripts-suite-file or --"
          <> field
          <> "-script-file."
    loadOne _ (Just path) wrap = fmap (fmap wrap) (readPlutusScriptV3 path)

-- | Wrap the loaded script bundle into a 'MarloweScriptsInfo' that the
-- Marlowe CLI flow expects. The role-token minting policy is dropped
-- since 'MarloweScriptsInfo' does not carry it.
mkMarloweScriptsInfo
  :: forall era
   . C.NetworkId
  -> C.StakeAddressReference
  -> MarloweScripts
  -> MarloweScriptsInfo PlutusScriptV3 era
mkMarloweScriptsInfo network stake (MarloweValidator marlowe, PayoutValidator payout, OpenRolesValidator openRoles, _) =
  MarloweScriptsInfo
    { msMarloweValidator = mkValidatorInfo marlowe network stake
    , msRolePayoutValidator = mkValidatorInfo payout network stake
    , msOpenRoleValidator = mkValidatorInfo openRoles network stake
    }

-- | Build a 'ValidatorInfo' from a Plutus script and a stake reference.
mkValidatorInfo
  :: forall lang era
   . C.IsPlutusScriptLanguage lang
  => PlutusScript lang
  -> C.NetworkId
  -> C.StakeAddressReference
  -> ValidatorInfo lang era
mkValidatorInfo viScript network stake =
  let C.PlutusScriptSerialised viBytes = viScript
      viHash = C.hashScript (C.PlutusScript (C.plutusScriptVersion @lang) viScript)
      viNetworkId = network
      viStakeCredential = case stake of
        C.NoStakeAddress -> Nothing
        C.StakeAddressByValue cred -> Just cred
        C.StakeAddressByPointer _ -> Nothing
      viTxIn = Nothing
   in ValidatorInfo{..}
