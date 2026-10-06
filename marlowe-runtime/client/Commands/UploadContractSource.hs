module Commands.UploadContractSource where

import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitError, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import qualified Data.Yaml as Yaml
import Language.Marlowe.Object.Types (Label (Label), ObjectBundle)
import Language.Marlowe.Runtime.Web.Client (postContractSource)
import Language.Marlowe.Runtime.Web.Contract.API (PostContractSourceResponse)
import Marlowe.Plutus.Semantics.Types (Action)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, help, info, long, metavar, option, progDesc, strOption, value)
import Pipes (each)
import Servant.Client (ClientError)

data UploadContractSourceCommand = UploadContractSourceCommand
  { bundleFile :: FilePath
  , mainLabel :: Label
  , preserveActions :: Set Action
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

labelReader :: ReadM Label
labelReader = eitherReader $ Right . Label . T.pack

labelParser :: Parser Label
labelParser = option labelReader
  ( long "main"
    <> metavar "LABEL"
    <> help "The label of the top-level contract object in the bundle."
  )

preserveActionsReader :: ReadM (Set Action)
preserveActionsReader = eitherReader $ \raw ->
  case Aeson.eitherDecodeStrict (T.encodeUtf8 (T.pack raw)) of
    Right actions -> Right (Set.fromList (actions :: [Action]))
    Left err -> Left $ "Could not decode preserve-actions JSON: " <> err

preserveActionsParser :: Parser (Set Action)
preserveActionsParser = option preserveActionsReader
  ( long "preserve-actions"
    <> metavar "JSON"
    <> value Set.empty
    <> help "JSON-encoded array of Action values to preserve during merkleization (omit to merkleize everything)."
  )

decodeBundleFile :: MonadIO m => MessageFormat -> FilePath -> m ObjectBundle
decodeBundleFile msgFormat filePath = do
  result <- liftIO $ Yaml.decodeFileEither filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the bundle file. Details: " <> Yaml.prettyPrintParseException err
    Right v -> pure v

mkUploadContractSourceCommandParser :: IO (ParserInfo UploadContractSourceCommand)
mkUploadContractSourceCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Upload a bundle of marlowe objects as a contract source."
    prs =
      UploadContractSourceCommand
        <$> strOption
          ( long "bundle-file"
              <> metavar "BUNDLE_FILE"
              <> help "JSON input file for the object bundle (an array of labelled objects)."
          )
        <*> labelParser
        <*> preserveActionsParser
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runUploadContractSourceCommand :: UploadContractSourceCommand -> IO ()
runUploadContractSourceCommand cmd = do
  bundle <- decodeBundleFile cmd.messageFormat cmd.bundleFile
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
    bundles = each [bundle]
  (result :: Either ClientError PostContractSourceResponse) <- runWebClient $ postContractSource cmd.preserveActions cmd.mainLabel bundles
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right response -> emitResponse cmd.messageFormat response
