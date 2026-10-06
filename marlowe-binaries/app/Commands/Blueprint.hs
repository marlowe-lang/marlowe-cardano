module Commands.Blueprint
  ( blueprintCommandParser
  , runBlueprintCommand
  ) where

import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Yaml qualified as Y
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), messageFormatParser)
import Marlowe.Plutus.Binaries.Api.Blueprint (BlueprintOutput (..))
import Marlowe.Plutus.Binaries.Blueprint (writeBlueprintToFile)
import Options.Applicative
  ( Parser
  , ParserInfo
  , argument
  , help
  , info
  , long
  , metavar
  , progDesc
  , str
  , switch
  , value
  )
import System.Directory (makeAbsolute)

data BlueprintCommand = BlueprintCommand
  { outputFile :: FilePath
  , messageFormat :: MessageFormat
  , outputAbsolutePaths :: Bool
  }

blueprintCommandParser :: ParserInfo BlueprintCommand
blueprintCommandParser =
  info
    ( BlueprintCommand
        <$> argument
          str
          ( metavar "FILE"
              <> value "plutus.json"
              <> help "Output path for the CIP-0057 blueprint JSON. Default: plutus.json."
          )
        <*> messageFormatParser
        <*> outputAbsolutePathsParser
    )
    (progDesc "Emit a CIP-0057 blueprint for the Marlowe validators.")

outputAbsolutePathsParser :: Parser Bool
outputAbsolutePathsParser =
  switch
    ( long "output-absolute-paths"
        <> help "Emit an absolute path (resolved against the current working directory) in the summary instead of the path as written on the command line."
    )

runBlueprintCommand :: BlueprintCommand -> IO ()
runBlueprintCommand cmd = do
  writeBlueprintToFile (outputFile cmd)
  resolved <- resolveBlueprintPath cmd.outputAbsolutePaths (outputFile cmd)
  let output = BlueprintOutput{blueprintFile = resolved}
  case cmd.messageFormat of
    MessageFormatText -> putStrLn $ "  wrote " <> blueprintFile output
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty output
    MessageFormatYaml -> BS8.putStrLn $ Y.encode output

resolveBlueprintPath :: Bool -> FilePath -> IO FilePath
resolveBlueprintPath False p = pure p
resolveBlueprintPath True p = makeAbsolute p