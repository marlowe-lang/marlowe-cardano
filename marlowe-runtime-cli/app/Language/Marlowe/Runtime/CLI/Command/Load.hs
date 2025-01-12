{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Marlowe.Runtime.CLI.Command.Load (
  LoadCommand (..),
  loadCommandParser,
  runLoadCommand,
) where

import Control.Monad (replicateM, unless, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Marlowe.Class (runMarloweTransferClient)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Language.Marlowe.Object.Archive (ReadArchiveError (..), unpackArchive)
import Language.Marlowe.Object.Types (
  Label (..),
  LabelledObject (..),
  ObjectBundle (..),
  ObjectType (ContractType),
  SomeObjectType (..),
  fromCoreContract,
 )
import Language.Marlowe.Protocol.Transfer.Types (ImportError (..))
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Client (importBundle, importIncremental)
import Language.Marlowe.Runtime.Client.Transfer (BundlePart (..))
import Options.Applicative (ParserInfo, flag, help, info, long, metavar, progDesc, short, strArgument)
import Pipes (Producer, next, yield, (>->))
import System.Exit (die)
import UnliftIO (liftIO)
import UnliftIO.Directory (doesFileExist)

data LoadCommand = LoadCommand
  { readJSON :: Bool
  , archivePath :: FilePath
  }

data LoadError
  = LoadReadArchiveError ReadArchiveError
  | LoadImportError ImportError

loadCommandParser :: ParserInfo LoadCommand
loadCommandParser = info parser $ progDesc "Load a contract into the runtime"
  where
    parser =
      LoadCommand
        <$> readJSONFlag
        <*> bundleArchiveOption
    readJSONFlag =
      flag False True $
        mconcat
          [ long "read-json"
          , short 'j'
          , help "Whether to read a Marlowe core JSON file instead of a bundle archive."
          ]
    bundleArchiveOption =
      strArgument $
        mconcat
          [ metavar "FILE_PATH"
          , help "A Marlowe contract archive, as a zip archive."
          ]

runLoadCommand :: LoadCommand -> CLI ()
runLoadCommand LoadCommand{..} = do
  exists <- liftIO $ doesFileExist archivePath
  liftIO $ unless exists $ die "Bundle archive file does not exist"
  result <-
    if readJSON
      then do
        bundle <- readJSONFile archivePath
        result <- runMarloweTransferClient $ importBundle bundle
        case result of
          Left err -> pure $ Left $ LoadImportError err
          Right hashes -> pure $ pure $ Map.lookup "main" hashes
      else
        nestArchiveError <$> unpackArchive archivePath \mainIs readObject -> do
          result <-
            runProducer $
              parts mainIs readObject >-> (first LoadImportError <$> runMarloweTransferClient importIncremental)
          case result of
            Left err -> pure $ Left err
            Right hashes -> pure $ pure $ Map.lookup mainIs hashes
  liftIO case result of
    Left err -> die $ formatLoadError err
    Right Nothing -> die "Error: main not linked. This is a bug, please report it with the archive you were trying to load attached."
    Right (Just mainHash) -> putStrLn $ T.unpack $ encodeBase16 $ unDatumHash mainHash

nestArchiveError :: Either ReadArchiveError (Either LoadError a) -> Either LoadError a
nestArchiveError = either (Left . LoadReadArchiveError) id

runProducer :: (Monad m) => Producer a m r -> m r
runProducer = either pure (runProducer . snd) <=< next

readJSONFile :: FilePath -> CLI ObjectBundle
readJSONFile path = do
  result <- liftIO $ eitherDecodeFileStrict path
  case result of
    Left err -> liftIO $ die $ "Error: bad contract file: " <> err
    Right contract -> do
      pure $ ObjectBundle $ pure $ LabelledObject "main" ContractType $ fromCoreContract contract

parts :: Label -> CLI (Maybe LabelledObject) -> Producer BundlePart CLI (Either LoadError a)
parts mainIs readObject = do
  objects <- lift $ catMaybes <$> replicateM 512 readObject
  case objects of
    [] -> pure $ Left $ LoadReadArchiveError MissingMain
    _ -> do
      yield
        if any ((== mainIs) . _label) objects
          then FinalPart $ ObjectBundle objects
          else IntermediatePart $ ObjectBundle objects
      parts mainIs readObject

formatLoadError :: LoadError -> String
formatLoadError = \case
  LoadReadArchiveError err -> formatReadArchiveError err
  LoadImportError err -> formatImportError err

formatImportError :: ImportError -> String
formatImportError = \case
  ContinuationNotInStore hash -> "Referenced continuation hash not found in store: " <> show hash
  LinkError err -> "Linking failed: " <> show err

formatReadArchiveError :: ReadArchiveError -> String
formatReadArchiveError = \case
  ArchiveNotFound -> "Archive file not found"
  MissingManifest -> "Archive does not contain a manifest.json"
  InvalidManifest e -> "Archive contains an invalid manifest.json: " <> e
  MissingObjectFile path -> "Manifest lists a missing object file path: " <> path
  InvalidObjectFile path err ->
    "Bundle contains an invalid object file. Path: " <> path <> "; Error:" <> err
  MissingMain -> "Manifest specifies a main object that is not listed as an object"
  WrongMainType (UnsafeSomeObjectType actual) -> "Manifest specifies a main object that is of a non-contract type: " <> show actual
