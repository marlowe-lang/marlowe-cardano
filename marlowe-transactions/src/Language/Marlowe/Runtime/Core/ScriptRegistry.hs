{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Core.ScriptRegistry
  ( HelperScript (..)
  , MarloweScripts (..)
  , ReferenceScriptUtxo (..)
  , ReleaseScriptHashes (..)
  , ScriptDetails (..)
  , ScriptInPlutus (..)
  , ScriptRegistry
  , ScriptRegistryError (..)
  , ScriptSuiteName (..)
  , defaultRegistry
  , fromCardanoPlutusScriptV2
  , fromCardanoPlutusScriptV3
  , fromCardanoScriptThrowing
  , getCurrentScripts
  , getMarloweVersion
  , getScriptsForRelease
  , hashScriptInPlutus
  , loadDefaultMarloweScripts
  , loadDefaultScriptRegistry
  , loadScriptRegistry
  , mkScriptDetails
  , mkScriptRegistry
  , pattern ScriptRegistry
  , toCardanoScriptInAnyLang
  )
  where

import Cardano.Api (NetworkId)
import Cardano.Api qualified as C
import Cardano.Api.Monad.Error (throwError)
import Control.Exception (catch, SomeException)
import Control.Monad (guard)
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT))
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.:?), (.=), (.!=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Binary (Binary)
import Data.ByteString.Base16.Aeson qualified as Base16Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as Short
import Data.Foldable ()
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Variations (Variations)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxOutRef (..))
import Language.Marlowe.Runtime.ChainSync.Api qualified as Chain
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Marlowe.Contrib.Foldable (foldMapFlipped)
import Paths_marlowe_transactions qualified as Paths
import PlutusLedgerApi.Common qualified as PLA
import qualified Data.List.NonEmpty as NEList

-- Instead of using `C.ScriptInAnyLang`
-- we can restrict ourselves to Plutus versions
-- which Marlowe was written in so far.
data ScriptInPlutus
  = ScriptInPlutusV2 PLA.SerialisedScript
  | ScriptInPlutusV3 PLA.SerialisedScript
  deriving (Show, Eq, Ord)

instance ToJSON ScriptInPlutus where
  toJSON (ScriptInPlutusV2 bytes) =
    Aeson.object
      [ "version" .= ("V2" :: T.Text)
      , "bytes" .= Base16Aeson.EncodeBase16 (Short.fromShort bytes)
      ]
  toJSON (ScriptInPlutusV3 bytes) =
    Aeson.object
      [ "version" .= ("V3" :: T.Text)
      , "bytes" .= Base16Aeson.EncodeBase16 (Short.fromShort bytes)
      ]

instance FromJSON ScriptInPlutus where
  parseJSON = Aeson.withObject "ScriptInPlutus" $ \o -> do
    version <- o .: "version"
    base16 <- o .: "bytes"
    case version :: T.Text of
      "V2" -> pure (ScriptInPlutusV2 (Short.toShort (Base16Aeson.unBase16 base16)))
      "V3" -> pure (ScriptInPlutusV3 (Short.toShort (Base16Aeson.unBase16 base16)))
      _ -> fail $ "Unknown script version: " <> T.unpack version


hashScriptInPlutus :: ScriptInPlutus -> Chain.ScriptHash
hashScriptInPlutus (ScriptInPlutusV2 bytes) =
  Chain.fromCardanoScriptHash
  . C.hashScript
  . C.PlutusScript C.PlutusScriptV2
  . C.PlutusScriptSerialised
  $ bytes
hashScriptInPlutus (ScriptInPlutusV3 bytes) =
  Chain.fromCardanoScriptHash
  . C.hashScript
  . C.PlutusScript C.PlutusScriptV3
  . C.PlutusScriptSerialised
  $ bytes

toCardanoScriptInAnyLang :: ScriptInPlutus -> C.ScriptInAnyLang
toCardanoScriptInAnyLang (ScriptInPlutusV2 bytes) =
    C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2)
    . C.PlutusScript C.PlutusScriptV2
    . C.PlutusScriptSerialised
    $ bytes
toCardanoScriptInAnyLang (ScriptInPlutusV3 bytes) =
    C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3)
    . C.PlutusScript C.PlutusScriptV3
    . C.PlutusScriptSerialised
    $ bytes

fromCardanoPlutusScriptV2 :: C.PlutusScript C.PlutusScriptV2 -> ScriptInPlutus
fromCardanoPlutusScriptV2 (C.PlutusScriptSerialised bytes) =
    ScriptInPlutusV2 bytes

fromCardanoPlutusScriptV3 :: C.PlutusScript C.PlutusScriptV3 -> ScriptInPlutus
fromCardanoPlutusScriptV3 (C.PlutusScriptSerialised bytes) =
    ScriptInPlutusV3 bytes

fromCardanoScriptInAnyLang :: C.ScriptInAnyLang -> Maybe ScriptInPlutus
fromCardanoScriptInAnyLang (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) (C.PlutusScript C.PlutusScriptV2 (C.PlutusScriptSerialised bytes))) =
    Just $ ScriptInPlutusV2 bytes
fromCardanoScriptInAnyLang (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) (C.PlutusScript C.PlutusScriptV3 (C.PlutusScriptSerialised bytes))) =
    Just $ ScriptInPlutusV3 bytes
fromCardanoScriptInAnyLang _ = Nothing

fromJustThrowing :: String -> Maybe a -> a
fromJustThrowing message = \case
  Just a -> a
  Nothing -> error message

fromCardanoScriptThrowing :: C.ScriptInAnyLang -> ScriptInPlutus
fromCardanoScriptThrowing = fromJustThrowing "Unsupported script language" . fromCardanoScriptInAnyLang

data ReferenceScriptUtxo = ReferenceScriptUtxo
  { txOutRef :: TxOutRef
  , txOut :: Chain.TransactionOutput
  , script :: ScriptInPlutus
  }
  deriving (Show, Eq, Ord)

instance ToJSON ReferenceScriptUtxo where
  toJSON ReferenceScriptUtxo{txOutRef, txOut, script} = do
    Aeson.object
      [ "txOutRef" .= Chain.renderTxOutRef txOutRef
      , "txOut" .= txOut
      , "script" .= script
      ]

instance FromJSON ReferenceScriptUtxo where
  parseJSON = Aeson.withObject "ReferenceScriptUtxo" $ \o -> do
    txOutRefStr <- o .: "txOutRef"
    txOut <- o .: "txOut"
    script <- o .: "script"
    txOutRef <- maybe (fail $ "Invalid txOutRef: " <> T.unpack txOutRefStr) pure $ Chain.parseTxOutRef txOutRefStr
    pure ReferenceScriptUtxo{txOutRef, txOut, script}


-- | A name identifying a single MarloweScripts bundle inside a 'ScriptRegistry'.
-- The full JSON representation (txOutRef + txOut + script + description) is
-- provided by 'Language.Marlowe.Runtime.Core.ScriptRegistry.JSON'.
newtype ScriptSuiteName = ScriptSuiteName { unScriptSuiteName :: T.Text }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance IsString ScriptSuiteName where
  fromString = ScriptSuiteName . T.pack

-- | The full information about a single Marlowe script: the serialised
-- Plutus bytes, its script hash, and the per-network reference script UTxOs
-- where it has been published. This is the middle ground between the
-- fully-decoded 'ValidatorInfo' from @marlowe-cli@ and the flattened
-- @scriptHash@/@scriptUTxOs@ pair that used to live directly on
-- 'MarloweScripts'.
data ScriptDetails = ScriptDetails
  { script :: ScriptInPlutus
  , scriptHash :: ScriptHash
  , scriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  }
  deriving (Show, Eq, Ord)

-- | Build a 'ScriptDetails' value from a Plutus script. The hash is derived
-- from the bytes, so the smart constructor keeps the two in sync.
mkScriptDetails :: ScriptInPlutus -> ScriptDetails
mkScriptDetails script =
  ScriptDetails
    { script
    , scriptHash = hashScriptInPlutus script
    , scriptUTxOs = mempty
    }

instance ToJSON ScriptDetails where
  toJSON ScriptDetails{..} =
    Aeson.object
      [ "script" .= script
      , "scriptHash" .= scriptHash
      , "scriptUTxOs" .= renderRefMap scriptUTxOs
      ]
   where
    renderRefMap m =
      Aeson.object
        [ Key.fromString (T.unpack (renderNetworkKey k)) .= v
        | (k, v) <- Map.toList m
        ]

    renderNetworkKey :: NetworkId -> T.Text
    renderNetworkKey = \case
      C.Mainnet -> "mainnet"
      C.Testnet (C.NetworkMagic n) ->
        "testnet/" <> T.pack (show (toInteger n))

instance FromJSON ScriptDetails where
  parseJSON = Aeson.withObject "ScriptDetails" $ \o -> do
    script <- o .: "script"
    let scriptHash = hashScriptInPlutus script
    scriptUTxOsRaw <- o .:? "scriptUTxOs" .!= mempty :: Parser Aeson.Object
    scriptUTxOs <- parseRefMap scriptUTxOsRaw
    pure ScriptDetails{..}
   where
    parseRefMap :: Aeson.Object -> Parser (Map NetworkId ReferenceScriptUtxo)
    parseRefMap obj = Map.fromList <$> mapM parseEntry (KeyMap.toList obj)
      where
        parseEntry (k, v) = do
          net <- Aeson.parseJSON (Aeson.String (Key.toText k))
          ref <- Aeson.parseJSON v
          pure (net, ref)

-- | A bundle of Marlowe scripts at a particular version.
data MarloweScripts = MarloweScripts
  { description :: Maybe T.Text
  , marloweScript :: ScriptDetails
  , marloweVersion :: SomeMarloweVersion
  , openRolesScript :: Maybe ScriptDetails
  , payoutScript :: ScriptDetails
  }
  deriving (Show, Eq, Ord)

-- | A registry of all known Marlowe script bundles.
data ScriptRegistry = UnsafeScriptRegistry
  { currentRelease :: ScriptSuiteName
  , scripts :: NEMap ScriptSuiteName MarloweScripts
  }
  deriving (Show, Eq)

mkScriptRegistry
  :: ScriptSuiteName
  -> Map ScriptSuiteName MarloweScripts
  -> Maybe ScriptRegistry
mkScriptRegistry currentRelease scriptsMap = do
  guard (Map.member currentRelease scriptsMap)
  scripts <- NEMap.nonEmptyMap scriptsMap
  pure UnsafeScriptRegistry {..}

{-# COMPLETE ScriptRegistry #-}
pattern ScriptRegistry :: ScriptSuiteName -> NEMap ScriptSuiteName MarloweScripts -> ScriptRegistry
pattern ScriptRegistry currentRelease scripts <- UnsafeScriptRegistry { currentRelease, scripts }

instance ToJSON ScriptRegistry where
  toJSON (UnsafeScriptRegistry currentRelease scripts) = Aeson.object
    [ "currentRelease" .= currentRelease
    , ("scripts", Aeson.object
        . map (\(k, v :: MarloweScripts) -> (Key.fromText (unScriptSuiteName k), toJSON v))
        . NEList.toList
        $ NEMap.toList scripts
      )
    ]

instance FromJSON ScriptRegistry where
  parseJSON = Aeson.withObject "ScriptRegistry" $ \o -> do
    let
      parseScripts :: Aeson.Object -> Parser (Map ScriptSuiteName MarloweScripts)
      parseScripts so = do
        let
          parseEntry (k, v) = do
            let
              suiteName = ScriptSuiteName (Key.toText k)
            marloweScripts <- Aeson.parseJSON v
            pure (suiteName, marloweScripts)
        Map.fromList <$> mapM parseEntry (KeyMap.toList so)

    currentRelease <- o .: "currentRelease"
    scriptsRaw <- o .: "scripts" :: Parser Aeson.Object
    scripts <- parseScripts scriptsRaw
    case mkScriptRegistry currentRelease scripts of
      Just registry -> pure registry
      Nothing -> fail $ "Current release " <> T.unpack (unScriptSuiteName currentRelease) <> " not found in scripts."

-- | The 'MarloweScripts' selected as the registry's current release.
-- Errors if the registry is missing that suite.
getCurrentScripts :: ScriptRegistry -> MarloweScripts
getCurrentScripts (ScriptRegistry currentRelease scripts) =
  fromMaybe (error $ "Current release " <> T.unpack (unScriptSuiteName currentRelease) <> " not found in scripts.") $
    NEMap.lookup currentRelease scripts

getScriptsForRelease :: ScriptSuiteName -> ScriptRegistry -> Maybe MarloweScripts
getScriptsForRelease suiteName (ScriptRegistry _currentRelease scripts) =
  NEMap.lookup suiteName scripts

data HelperScript = OpenRoleScript
  deriving stock (Read, Show, Bounded, Enum, Eq, Ord, Generic)
  deriving anyclass (Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey, Variations)

-- | Errors that can occur while loading a 'ScriptRegistry' from disk.
data ScriptRegistryError
  = ScriptRegistryNotFound FilePath
  | ScriptRegistryParseError FilePath T.Text
  deriving (Show)

loadScriptRegistry :: FilePath -> IO (Either ScriptRegistryError ScriptRegistry)
loadScriptRegistry jsonFile = runExceptT do
  content <- ExceptT $ (Right <$> BSL.readFile jsonFile) `catch` \(_e :: SomeException) ->
    pure $ Left (ScriptRegistryNotFound jsonFile)
  case Aeson.eitherDecode content of
    Left err -> throwError (ScriptRegistryParseError jsonFile (T.pack err))
    Right registry -> pure registry

defaultRegistry :: FilePath
defaultRegistry = "script-registry/pre-1.1.0.json"

-- | IO that loads the registry shipped with the 'marlowe-transactions'
-- package (see 'data-files: script-registry/*.json' in the cabal file).
-- Fails if the directory is missing, the registry is empty, or the
-- registry is malformed.
loadDefaultScriptRegistry :: IO (Either ScriptRegistryError ScriptRegistry)
loadDefaultScriptRegistry = Paths.getDataFileName defaultRegistry >>= loadScriptRegistry

loadDefaultMarloweScripts :: IO (Either ScriptRegistryError MarloweScripts)
loadDefaultMarloweScripts = runExceptT do
  registryResult <- ExceptT loadDefaultScriptRegistry
  pure $ getCurrentScripts registryResult

instance ToJSON MarloweScripts where
  toJSON MarloweScripts{..} =
    Aeson.object
      [ "description" .= description
      , "marloweScript" .= marloweScript
      , "marloweVersion" .= marloweVersion
      , "openRolesScript" .= openRolesScript
      , "payoutScript" .= payoutScript
      ]

instance FromJSON MarloweScripts where
  parseJSON = Aeson.withObject "MarloweScripts" $ \o -> do
    description <- o .:? "description"
    marloweScript <- o .: "marloweScript"
    marloweVersion <- o .: "marloweVersion"
    openRolesScript <- o .:? "openRolesScript"
    payoutScript <- o .: "payoutScript"
    pure MarloweScripts{..}

data ReleaseScriptHashes = ReleaseScriptHashes
  { marloweScriptHash :: ScriptHash
  , openRolesScriptHash :: Maybe ScriptHash
  , payoutScriptHash :: ScriptHash
  }

getMarloweVersion :: ScriptRegistry -> ScriptHash -> Maybe (SomeMarloweVersion, ReleaseScriptHashes)
getMarloweVersion (ScriptRegistry _currentRelease scripts) hash = listToMaybe $ foldMapFlipped (Map.toList . NEMap.toMap $ scripts) \(_, MarloweScripts{..}) -> do
  let
    marloweScriptHash = marloweScript.scriptHash
    openRolesScriptHash = (.scriptHash) <$> openRolesScript
    payoutScriptHash = payoutScript.scriptHash
    releaseScriptHashes = ReleaseScriptHashes
      { marloweScriptHash
      , openRolesScriptHash
      , payoutScriptHash
      }
  guard (hash == marloweScriptHash || Just hash == openRolesScriptHash || hash == payoutScriptHash)
  pure (marloweVersion, releaseScriptHashes)

