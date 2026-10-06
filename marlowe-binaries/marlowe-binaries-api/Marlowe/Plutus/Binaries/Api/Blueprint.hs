module Marlowe.Plutus.Binaries.Api.Blueprint
  ( BlueprintOutput (..)
  ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import GHC.Generics (Generic)

-- | Result of generating a CIP-0057 blueprint: the path to the written
-- JSON file. Mirrors the shape of 'ScriptOutput' so the CLI can emit
-- blueprint results in the same text/json/yaml formats.
data BlueprintOutput = BlueprintOutput
  { blueprintFile :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BlueprintOutput where
  toJSON BlueprintOutput{blueprintFile} =
    object
      [ "blueprintFile" .= blueprintFile
      ]

instance FromJSON BlueprintOutput where
  parseJSON = withObject "BlueprintOutput" $ \obj ->
    BlueprintOutput
      <$> obj .: "blueprintFile"