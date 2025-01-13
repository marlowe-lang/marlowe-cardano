module Language.Marlowe.Runtime.CLI.Command.Query.Store where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Foldable (traverse_)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import Language.Marlowe.Runtime.CLI.Monad
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Client (runContractQueryClient)
import Language.Marlowe.Runtime.Contract.Api
import Options.Applicative
import System.IO (hPutStrLn, stderr)

data StoreQueryCommand
  = QueryContract DatumHash
  | QueryClosure DatumHash
  | QueryAdjacency DatumHash

storeQueryCommandParser :: ParserInfo StoreQueryCommand
storeQueryCommandParser = info parser $ progDesc "Query the contract store"
  where
    parser =
      hsubparser $
        mconcat
          [ command "contract" $ QueryContract <$> info contractHashParser contractInfo
          , command "closure" $ QueryClosure <$> info contractHashParser closureInfo
          , command "adjacency" $ QueryAdjacency <$> info contractHashParser adjacencyInfo
          ]
    contractInfo = progDesc "Query the (merkleized) source of a contract in the contract store"
    closureInfo = progDesc "Query the deep dependency closure of a contract in the store."
    adjacencyInfo = progDesc "Query the immediate dependencies of a contract in the store."
    contractHashParser =
      strArgument $
        mconcat
          [ metavar "CONTRACT_HASH"
          , help "The hash of the contract to query in the store."
          ]

runStoreQueryCommand :: StoreQueryCommand -> CLI ()
runStoreQueryCommand = \case
  QueryContract hash -> runQueryContract hash
  QueryClosure hash -> runQueryClosure hash
  QueryAdjacency hash -> runQueryAdjacency hash

runQueryContract :: DatumHash -> CLI ()
runQueryContract hash = runGetContractCommand hash \ContractWithAdjacency{..} -> do
  liftIO $ T.putStrLn $ T.toLazyText $ encodePrettyToTextBuilder contract

runQueryClosure :: DatumHash -> CLI ()
runQueryClosure hash = runGetContractCommand hash \ContractWithAdjacency{..} -> do
  liftIO $ traverse_ (putStrLn . read . show) closure

runQueryAdjacency :: DatumHash -> CLI ()
runQueryAdjacency hash = runGetContractCommand hash \ContractWithAdjacency{..} -> do
  liftIO $ traverse_ (putStrLn . read . show) adjacency

runGetContractCommand :: DatumHash -> (ContractWithAdjacency -> CLI ()) -> CLI ()
runGetContractCommand hash f = do
  mContract <- runContractQueryClient $ getContract hash
  case mContract of
    Nothing -> liftIO $ hPutStrLn stderr "Contract not found."
    Just contract -> f contract
