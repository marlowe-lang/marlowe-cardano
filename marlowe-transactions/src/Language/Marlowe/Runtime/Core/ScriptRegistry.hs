{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Core.ScriptRegistry
  ( HelperScript (..)
  , MarloweScriptHashes (..)
  , MarloweScripts (..)
  , ReferenceScriptUtxo (..)
  , ScriptInPlutus (..)
  , ScriptRegistry
  , ScriptRegistryError (..)
  , ScriptsSuiteName (..)
  , fromCardanoPlutusScriptV2
  , fromCardanoPlutusScriptV3
  , fromCardanoScriptThrowing
  , getMarloweVersion
  , hashScriptInPlutus
  , loadDefaultScriptRegistry
  , loadScriptRegistry
  , mainnetNetworkId
  , mkScriptRegistry
  , pattern ScriptRegistry
  , preprodNetworkId
  , previewNetworkId
  , sanchonetNetworkId
  , toCardanoScriptInAnyLang
  )
  where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
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
import Language.Marlowe.Runtime.ChainSync.Api ( Assets (..), Lovelace (Lovelace), ScriptHash, TxOutRef (..), mkTxOutAssets,)
import Language.Marlowe.Runtime.ChainSync.Api qualified as Chain
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Marlowe.Plutus.Contrib.Data.Foldable (foldMapFlipped)
import Paths_marlowe_transactions qualified as Paths
import PlutusLedgerApi.Common qualified as PLA
import Text.Read qualified as T
import qualified Data.List.NonEmpty as NEList

mainnetNetworkId :: NetworkId
mainnetNetworkId = Mainnet

preprodNetworkId :: NetworkId
preprodNetworkId = Testnet $ NetworkMagic 1

previewNetworkId :: NetworkId
previewNetworkId = Testnet $ NetworkMagic 2

sanchonetNetworkId :: NetworkId
sanchonetNetworkId = Testnet $ NetworkMagic 4

instance Ord NetworkId where
  compare Mainnet Mainnet = EQ
  compare Mainnet _ = LT
  compare _ Mainnet = GT
  compare (Testnet (NetworkMagic a)) (Testnet (NetworkMagic b)) =
    compare a b

networkIdToText :: NetworkId -> T.Text
networkIdToText Mainnet = "mainnet"
networkIdToText (Testnet (NetworkMagic n)) = "testnet/" <> T.pack (show (toInteger n))

networkIdFromText :: T.Text -> Maybe NetworkId
networkIdFromText t = case T.splitOn "/" . T.toLower $ t of
  ["mainnet"] -> Just Mainnet
  ["testnet", m] -> case T.reads (T.unpack m) :: [(Int, String)] of
    [(n, "")] -> Just $ Testnet $ NetworkMagic (toEnum n)
    _ -> Nothing
  _ -> Nothing

-- | 'NetworkId' encodes as @\"mainnet\"@ or @\"testnet/<networkMagic>\"@.
instance ToJSON NetworkId where
  toJSON networkId = Aeson.String $ networkIdToText networkId

instance FromJSON NetworkId where
  parseJSON = Aeson.withText "NetworkId" $ \t ->
    case networkIdFromText t of
      Just nid -> pure nid
      Nothing -> fail $ "Invalid NetworkId: " <> T.unpack t

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
    let
      -- | Render a minimal TransactionOutput: address + lovelace only.
      renderTxOut :: Chain.TransactionOutput -> Aeson.Value
      renderTxOut Chain.TransactionOutput{Chain.address = Chain.Address addr, Chain.assets = Chain.TxOutAssets (Assets (Lovelace l) _)} =
        Aeson.object
          [ "address" .= addr
          , "lovelace" .= l
          ]
    Aeson.object
      [ "txOutRef" .= Chain.renderTxOutRef txOutRef
      , "txOut" .= renderTxOut txOut
      , "script" .= script
      ]

instance FromJSON ReferenceScriptUtxo where
  parseJSON = Aeson.withObject "ReferenceScriptUtxo" $ \o -> do
    txOutRefStr <- o .: "txOutRef"
    txOutObj <- o .: "txOut"
    script <- o .: "script"
    txOutRef <- maybe (fail $ "Invalid txOutRef: " <> T.unpack txOutRefStr) pure $ Chain.parseTxOutRef txOutRefStr
    addrStr <- txOutObj .: "address"
    lovelaceInt <- (txOutObj .: "lovelace") :: Parser Integer
    addr <- maybe (fail $ "Invalid address: " <> T.unpack addrStr) pure
      $ Chain.fromBech32 addrStr
    let assets = fromMaybe mempty $ mkTxOutAssets (Assets (Lovelace lovelaceInt) mempty)
        txOut =
          Chain.TransactionOutput
            { Chain.address = addr
            , Chain.assets = assets
            , Chain.datum = Nothing
            , Chain.datumHash = Nothing
            }
    pure ReferenceScriptUtxo{txOutRef, txOut, script}


-- | A name identifying a single MarloweScripts bundle inside a 'ScriptRegistry'.
-- The full JSON representation (txOutRef + txOut + script + description) is
-- provided by 'Language.Marlowe.Runtime.Core.ScriptRegistry.JSON'.
newtype ScriptsSuiteName = ScriptsSuiteName { unScriptsSuiteName :: T.Text }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance IsString ScriptsSuiteName where
  fromString = ScriptsSuiteName . T.pack

-- | A set of script hashes for a marlowe version.
data MarloweScripts = MarloweScripts
  { description :: Maybe T.Text
  , marloweScript :: ScriptHash
  , marloweScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  , marloweVersion :: SomeMarloweVersion
  , openRolesScript :: Maybe ScriptHash
  , openRolesScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  , payoutScript :: ScriptHash
  , payoutScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  }
  deriving (Show, Eq, Ord)

-- | A registry of all known Marlowe script bundles.
data ScriptRegistry = UnsafeScriptRegistry
  { currentRelease :: ScriptsSuiteName
  , scripts :: NEMap ScriptsSuiteName MarloweScripts
  }
  deriving (Show, Eq)

mkScriptRegistry
  :: ScriptsSuiteName
  -> Map ScriptsSuiteName MarloweScripts
  -> Maybe ScriptRegistry
mkScriptRegistry currentRelease scriptsMap = do
  guard (Map.member currentRelease scriptsMap)
  scripts <- NEMap.nonEmptyMap scriptsMap
  pure UnsafeScriptRegistry {..}

{-# COMPLETE ScriptRegistry #-}
pattern ScriptRegistry :: ScriptsSuiteName -> NEMap ScriptsSuiteName MarloweScripts -> ScriptRegistry
pattern ScriptRegistry currentRelease scripts <- UnsafeScriptRegistry { currentRelease, scripts }

instance ToJSON ScriptRegistry where
  toJSON (UnsafeScriptRegistry currentRelease scripts) = Aeson.object
    [ "currentRelease" .= currentRelease
    , ("scripts", Aeson.object
        . map (\(k, v :: MarloweScripts) -> (Key.fromText (unScriptsSuiteName k), toJSON v))
        . NEList.toList
        $ NEMap.toList scripts
      )
    ]

instance FromJSON ScriptRegistry where
  parseJSON = Aeson.withObject "ScriptRegistry" $ \o -> do
    let
      parseScripts :: Aeson.Object -> Parser (Map ScriptsSuiteName MarloweScripts)
      parseScripts so = do
        let
          parseEntry (k, v) = do
            let
              suiteName = ScriptsSuiteName (Key.toText k)
            marloweScripts <- Aeson.parseJSON v
            pure (suiteName, marloweScripts)
        Map.fromList <$> mapM parseEntry (KeyMap.toList so)

    currentRelease <- o .: "currentRelease"
    scriptsRaw <- o .: "scripts" :: Parser Aeson.Object
    scripts <- parseScripts scriptsRaw
    case mkScriptRegistry currentRelease scripts of
      Just registry -> pure registry
      Nothing -> fail $ "Current release " <> T.unpack (unScriptsSuiteName currentRelease) <> " not found in scripts."

getCurrentRelease :: ScriptRegistry -> MarloweScripts
getCurrentRelease (ScriptRegistry currentRelease scripts) =
  fromMaybe (error $ "Current release " <> T.unpack (unScriptsSuiteName currentRelease) <> " not found in scripts.") $
    NEMap.lookup currentRelease scripts


data HelperScript = OpenRoleScript
  deriving stock (Read, Show, Bounded, Enum, Eq, Ord, Generic)
  deriving anyclass (Binary, FromJSON, FromJSONKey, ToJSON, ToJSONKey, Variations)

-- | Errors that can occur while loading a 'ScriptRegistry' from disk.
data ScriptRegistryError
  = ScriptRegistryFileNotFound FilePath
  | ScriptRegistryParseError FilePath T.Text
  deriving (Show)

loadScriptRegistry :: FilePath -> IO (Either ScriptRegistryError ScriptRegistry)
loadScriptRegistry jsonFile = runExceptT do
  content <- ExceptT $ (Right <$> BSL.readFile jsonFile) `catch` \(_e :: SomeException) ->
    pure $ Left (ScriptRegistryFileNotFound jsonFile)
  case Aeson.eitherDecode content of
    Left err -> throwError (ScriptRegistryParseError jsonFile (T.pack err))
    Right registry -> pure registry

-- | IO that loads the registry shipped with the 'marlowe-transactions'
-- package (see 'data-files: script-registry/*.json' in the cabal file).
-- Fails if the directory is missing, the registry is empty, or the
-- registry is malformed.
loadDefaultScriptRegistry :: IO (Either ScriptRegistryError ScriptRegistry)
loadDefaultScriptRegistry = Paths.getDataFileName "script-registry.json" >>= loadScriptRegistry

instance ToJSON MarloweScripts where
  toJSON MarloweScripts{..} = do
    let
      renderRefMap m =
        Aeson.object [ Key.fromString (T.unpack (renderNetworkKey k)) .= v | (k, v) <- Map.toList m ]

      renderNetworkKey :: NetworkId -> T.Text
      renderNetworkKey Mainnet = "mainnet"
      renderNetworkKey (Testnet (NetworkMagic n)) =
        "testnet/" <> T.pack (show (toInteger n))

    Aeson.object
      [ "description" .= description
      , "marloweScript" .= marloweScript
      , "marloweScriptUTxOs" .= renderRefMap marloweScriptUTxOs
      , "marloweVersion" .= marloweVersion
      , "openRolesScript" .= openRolesScript
      , "openRolesScriptUTxOs" .= renderRefMap openRolesScriptUTxOs
      , "payoutScript" .= payoutScript
      , "payoutScriptUTxOs" .= renderRefMap payoutScriptUTxOs
      ]

instance FromJSON MarloweScripts where
  parseJSON = Aeson.withObject "MarloweScripts" $ \o -> do
    let
      -- | Parse the \"Network -> ReferenceScriptUtxo\" object.
      parseRefMap :: Aeson.Object -> Parser (Map NetworkId ReferenceScriptUtxo)
      parseRefMap obj = Map.fromList <$> mapM parseEntry (KeyMap.toList obj)
        where
          parseEntry (k, v) = do
            net <- Aeson.parseJSON (Aeson.String (Key.toText k))
            ref <- Aeson.parseJSON v
            pure (net, ref)

    description <- o .:? "description"

    marloweScript <- o .: "marloweScript"
    marloweUTxOsRaw <- o .:? "marloweScriptUTxOs" .!= mempty :: Parser Aeson.Object
    marloweScriptUTxOs <- parseRefMap marloweUTxOsRaw
    marloweVersion <- o .: "marloweVersion"

    openRolesScript <- o .:? "openRolesScript"
    openRolesUTxOsRaw <- o .:? "openRolesScriptUTxOs" .!= mempty :: Parser Aeson.Object
    openRolesScriptUTxOs <- parseRefMap openRolesUTxOsRaw

    payoutScript <- o .: "payoutScript"
    payoutUTxOsRaw <- o .:? "payoutScriptUTxOs" .!= mempty :: Parser Aeson.Object
    payoutScriptUTxOs <- parseRefMap payoutUTxOsRaw

    pure MarloweScripts
      { description
      , marloweScript
      , marloweScriptUTxOs
      , marloweVersion
      , openRolesScript
      , openRolesScriptUTxOs
      , payoutScript
      , payoutScriptUTxOs
      }

data MarloweScriptHashes = MarloweScriptHashes
  { marloweScript :: ScriptHash
  , openRolesScript :: Maybe ScriptHash
  , payoutScript :: ScriptHash
  }

getMarloweVersion :: ScriptRegistry -> ScriptHash -> Maybe (SomeMarloweVersion, MarloweScriptHashes)
getMarloweVersion (ScriptRegistry _currentRelease scripts) hash = listToMaybe $ foldMapFlipped (Map.toList . NEMap.toMap $ scripts) \(_, MarloweScripts{..}) -> do
  let
    marloweScriptHashes = MarloweScriptHashes
      { marloweScript = marloweScript
      , openRolesScript = openRolesScript
      , payoutScript = payoutScript
      }
  guard (hash == marloweScript || Just hash == openRolesScript || hash == payoutScript)
  pure (marloweVersion, marloweScriptHashes)

