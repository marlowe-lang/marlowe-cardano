{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Marlowe.Runtime.CLI.Command.Export (
  ExportCommand (..),
  exportCommandParser,
  runExportCommand,
) where

import Control.Monad (forever, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Marlowe.Class (runMarloweTransferClient)
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (traverse_)
import Language.Marlowe.Object.Archive (packArchive)
import Language.Marlowe.Object.Types (Label (..), ObjectBundle (..))
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Client (exportIncremental)
import Options.Applicative (ParserInfo, help, info, long, metavar, progDesc, short, strArgument, strOption, value)
import Pipes (await, (>->))
import qualified Pipes as P
import System.Exit (die)
import UnliftIO (liftIO)

data ExportCommand = ExportCommand
  { outPath :: FilePath
  , contractHash :: DatumHash
  }

exportCommandParser :: ParserInfo ExportCommand
exportCommandParser = info parser $ progDesc "Export a contract from the contract store of the runtime"
  where
    parser =
      ExportCommand
        <$> outPathOption
        <*> contractHashArgument
    outPathOption =
      strOption $
        mconcat
          [ metavar "FILE_PATH"
          , long "out-path"
          , short 'o'
          , value "out.zip"
          , help "A filepath to which to write the resulting archive file (as a zip archive)."
          ]
    contractHashArgument =
      strArgument $
        mconcat
          [ metavar "CONTRACT_HASH"
          , help "The hash of the contract to export from the contract store, as a hex string."
          ]

runExportCommand :: ExportCommand -> CLI ()
runExportCommand ExportCommand{..} = do
  found <- packArchive outPath (Label $ encodeBase16 $ unDatumHash contractHash) \writeObject ->
    P.runEffect $
      runMarloweTransferClient (exportIncremental 50 contractHash)
        >-> forever (lift . traverse_ writeObject . getObjects =<< await)
  liftIO $ unless found $ die "Contract not found."
