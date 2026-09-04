module Marlowe.Plutus.Binaries.Api.Compile
  ( ScriptName(..)
  , ScriptVariant(..)
  , ScriptOutput(..)
  , scriptNameToText
  , scriptNameFromText
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.=), (.:), withObject)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

data ScriptName
  = MarloweSemantics
  | MarloweRolePayout
  | OpenRoles
  deriving stock (Eq, Show, Generic)

scriptNameToText :: ScriptName -> Text
scriptNameToText = \case
  MarloweSemantics -> "marlowe-semantics"
  MarloweRolePayout -> "marlowe-rolepayout"
  OpenRoles -> "marlowe-openroles"

scriptNameFromText :: Text -> Maybe ScriptName
scriptNameFromText = \case
  "marlowe-semantics" -> Just MarloweSemantics
  "marlowe-rolepayout" -> Just MarloweRolePayout
  "marlowe-openroles" -> Just OpenRoles
  _ -> Nothing

instance ToJSON ScriptName where
  toJSON = Aeson.String . scriptNameToText

instance FromJSON ScriptName where
  parseJSON = Aeson.withText "ScriptName" $ \txt ->
    case scriptNameFromText txt of
      Just name -> pure name
      Nothing -> fail $ "Expected 'marlowe-semantics' or 'marlowe-rolepayout', got: " <> T.unpack txt

data ScriptVariant
  = DevelScripts
  | ProductionScripts
  deriving stock (Eq, Show, Generic)

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

data ScriptsSuite = ScriptsSuite
  { suiteVariant :: ScriptVariant
  , responseOutputDir :: FilePath
  , marloweSemantics :: ScriptOutput
  , marloweRolePayout :: ScriptOutput
  , openRoles :: ScriptOutput
  }
  deriving stock (Eq, Show, Generic)


instance ToJSON ScriptsSuite where
  toJSON ScriptsSuite{suiteVariant, responseOutputDir, marloweSemantics, marloweRolePayout, openRoles} =
    object
      [ "suiteVariant" .= suiteVariant
      , "responseOutputDir" .= responseOutputDir
      , "marloweSemantics" .= marloweSemantics
      , "marloweRolePayout" .= marloweRolePayout
      , "openRoles" .= openRoles
      ]

instance FromJSON ScriptsSuite where
  parseJSON = withObject "ScriptsSuite" $ \obj ->
    ScriptsSuite
      <$> obj .: "suiteVariant"
      <*> obj .: "responseOutputDir"
      <*> obj .: "marloweSemantics"
      <*> obj .: "marloweRolePayout"
      <*> obj .: "openRoles"
