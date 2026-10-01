module Marlowe.Plutus.Binaries.Api.Compile
  ( ScriptName(..)
  , ScriptVariant(..)
  , ScriptOutput(..)
  , ScriptSuite(..)
  , MessageFormat(..)
  , messageFormatFromText
  , scriptNameToText
  , scriptNameFromText
  ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=), (.:), (.:?), withObject)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), messageFormatFromText)

data ScriptName
  = MarloweSemantics
  | MarloweRolePayout
  | OpenRoles
  | MarloweRoleTokens
  deriving stock (Eq, Show, Generic)

scriptNameToText :: ScriptName -> Text
scriptNameToText = \case
  MarloweSemantics -> "marlowe-semantics"
  MarloweRolePayout -> "marlowe-rolepayout"
  OpenRoles -> "marlowe-openroles"
  MarloweRoleTokens -> "marlowe-roletokens"

scriptNameFromText :: Text -> Maybe ScriptName
scriptNameFromText = \case
  "marlowe-semantics" -> Just MarloweSemantics
  "marlowe-rolepayout" -> Just MarloweRolePayout
  "marlowe-openroles" -> Just OpenRoles
  "marlowe-roletokens" -> Just MarloweRoleTokens
  _ -> Nothing

instance ToJSON ScriptName where
  toJSON = Aeson.String . scriptNameToText

instance FromJSON ScriptName where
  parseJSON = Aeson.withText "ScriptName" $ \txt ->
    case scriptNameFromText txt of
      Just name -> pure name
      Nothing -> fail $ "Expected one of: 'marlowe-semantics', 'marlowe-rolepayout', 'marlowe-openroles', 'marlowe-roletokens'; got: " <> T.unpack txt

data ScriptVariant
  = DevelScripts
  | ProductionScripts
  deriving stock (Eq, Generic)

instance Show ScriptVariant where
  show = \case
    DevelScripts -> "devel"
    ProductionScripts -> "production"

instance ToJSON ScriptVariant where
  toJSON = \case
    DevelScripts -> "devel"
    ProductionScripts -> "production"

instance FromJSON ScriptVariant where
  parseJSON = \case
    "devel" -> pure DevelScripts
    "production" -> pure ProductionScripts
    other -> fail $ "Expected 'devel' or 'production', got: " <> show other

data ScriptOutput = ScriptOutput
  { scriptName :: ScriptName
  , scriptHash :: String
  , scriptFile :: FilePath
  , hashFile :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ScriptOutput where
  toJSON ScriptOutput{scriptName, scriptHash, scriptFile, hashFile} =
    object
      [ "scriptName" .= scriptName
      , "scriptHash" .= scriptHash
      , "scriptFile" .= scriptFile
      , "hashFile" .= hashFile
      ]

instance FromJSON ScriptOutput where
  parseJSON = withObject "ScriptOutput" $ \obj ->
    ScriptOutput
      <$> obj .: "scriptName"
      <*> obj .: "scriptHash"
      <*> obj .: "scriptFile"
      <*> obj .: "hashFile"

data ScriptSuite = ScriptSuite
  { suiteVariant :: ScriptVariant
  , responseOutputDir :: FilePath
  , marloweSemantics :: ScriptOutput
  , marloweRolePayout :: ScriptOutput
  , openRoles :: ScriptOutput
  , roleTokens :: Maybe ScriptOutput
  -- ^ Optional role-token minting policy. Encoded as @null@ when missing
  -- so the public ScriptSuite JSON stays stable regardless of whether
  -- the caller supplied the role-token flags.
  }
  deriving stock (Eq, Show, Generic)


instance ToJSON ScriptSuite where
  toJSON ScriptSuite{suiteVariant, responseOutputDir, marloweSemantics, marloweRolePayout, openRoles, roleTokens} =
    object
      [ "suiteVariant" .= suiteVariant
      , "responseOutputDir" .= responseOutputDir
      , "marloweSemantics" .= marloweSemantics
      , "marloweRolePayout" .= marloweRolePayout
      , "openRoles" .= openRoles
      , "roleTokens" .= roleTokens
      ]

instance FromJSON ScriptSuite where
  parseJSON = withObject "ScriptSuite" $ \obj ->
    ScriptSuite
      <$> obj .: "suiteVariant"
      <*> obj .: "responseOutputDir"
      <*> obj .: "marloweSemantics"
      <*> obj .: "marloweRolePayout"
      <*> obj .: "openRoles"
      <*> obj .:? "roleTokens"
