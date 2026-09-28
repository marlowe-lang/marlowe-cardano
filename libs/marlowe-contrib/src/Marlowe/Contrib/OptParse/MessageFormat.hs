module Marlowe.Contrib.OptParse.MessageFormat
  ( MessageFormat (..)
  , messageFormatFromText
  , messageFormatParser
  , emitError
  , emitJSONError
  , emitResponse
  , emitResponseWith
  , emitResponseWithTyped
  ) where

import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml.Pretty qualified as Y
import GHC.Generics (Generic)
import Options.Applicative
  ( Parser
  , ReadM
  , eitherReader
  , help
  , long
  , metavar
  , option
  , showDefault
  , value
  )
import System.Exit (die)

-- | Output format requested via the @--message-format@ CLI flag.
data MessageFormat = MessageFormatText | MessageFormatJson | MessageFormatYaml
  deriving stock (Eq, Ord, Show, Generic)

instance IsString MessageFormat where
  fromString = messageFormatFromText

instance A.ToJSON MessageFormat where
  toJSON = \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

-- | Parse a textual @MessageFormat@ (case-insensitive). Errors on unknown
-- values; use 'messageFormatParser' to surface a friendly error message
-- during option parsing instead.
messageFormatFromText :: String -> MessageFormat
messageFormatFromText s = case map toLower s of
  "text" -> MessageFormatText
  "json" -> MessageFormatJson
  "yaml" -> MessageFormatYaml
  other -> error $ "messageFormatFromText: unknown format " <> show other

-- | Standard @--message-format@ option parser shared by every Marlowe
-- CLI tool. Defaults to 'MessageFormatText'.
messageFormatParser :: Parser MessageFormat
messageFormatParser =
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

readMessageFormat :: ReadM MessageFormat
readMessageFormat = eitherReader $ \case
  "text" -> Right MessageFormatText
  "json" -> Right MessageFormatJson
  "yaml" -> Right MessageFormatYaml
  other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."

yamlFormattingConfig :: Y.Config
yamlFormattingConfig = Y.setConfCompare compare Y.defConfig

dieBS :: BS.ByteString -> IO a
dieBS = die . T.unpack . T.decodeUtf8

dieLBS :: LBS.ByteString -> IO a
dieLBS = dieBS . LBS.toStrict

type JsonStringifier = A.Value -> LBS.ByteString

emitJSONErrorWith :: MessageFormat -> A.Value -> JsonStringifier -> IO a
emitJSONErrorWith messageFormat json mkMsg =
  case messageFormat of
    MessageFormatText -> dieLBS $ mkMsg json
    MessageFormatJson -> dieLBS $ A.encodePretty json
    MessageFormatYaml -> dieBS $ Y.encodePretty yamlFormattingConfig json

emitJSONError :: MessageFormat -> A.Value -> IO a
emitJSONError messageFormat json = emitJSONErrorWith messageFormat json A.encodePretty

emitErrorWith :: forall a b. A.ToJSON a => MessageFormat -> a -> JsonStringifier -> IO b
emitErrorWith messageFormat err = emitJSONErrorWith messageFormat (A.toJSON err)

emitError :: forall a b. A.ToJSON a => MessageFormat -> a -> IO b
emitError messageFormat err = emitErrorWith messageFormat err (A.encodePretty . A.toJSON)

emitResponseWith :: A.ToJSON a => MessageFormat -> a -> JsonStringifier -> IO ()
emitResponseWith messageFormat response mkMsg =
  let
    json = A.toJSON response
  in
    case messageFormat of
      MessageFormatText -> LBS8.putStrLn (mkMsg json)
      MessageFormatJson -> LBS8.putStrLn (A.encodePretty json)
      MessageFormatYaml -> BS8.putStrLn $ Y.encodePretty yamlFormattingConfig json

emitResponse :: A.ToJSON a => MessageFormat -> a -> IO ()
emitResponse messageFormat response =
  let
    stringifier res = do
      let
        yaml = LBS.fromStrict . Y.encodePretty yamlFormattingConfig . A.toJSON $ res
      "Command finished successfully:\n" <> yaml
  in
    emitResponseWith messageFormat response stringifier

-- | Same as 'emitResponseWith' but accepts a text renderer operating on the
-- typed value rather than its JSON encoding.
emitResponseWithTyped
  :: A.ToJSON a
  => MessageFormat
  -> a
  -> (a -> BS.ByteString)
  -> IO ()
emitResponseWithTyped messageFormat response renderText =
  case messageFormat of
    MessageFormatText -> BS8.putStrLn $ renderText response
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty (A.toJSON response)
    MessageFormatYaml -> BS8.putStrLn $ Y.encodePretty yamlFormattingConfig (A.toJSON response)