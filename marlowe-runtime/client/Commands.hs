module Commands where

import Data.Foldable (fold)
import Options.Applicative (
  Parser,
  command,
  hsubparser,
  info,
  progDesc,
 )
import Commands.ApplyInputs (mkApplyInputsCommandParser, runApplyInputsCommand)
import Commands.Init (mkInitCommandParser, runInitCommand)
import Commands.GetContract (mkGetContractCommandParser, runGetContractCommand)
import Commands.GetTransactions (mkGetTransactionsCommandParser, runGetTransactionsCommand)
import Commands.Next (mkNextCommandParser, runNextCommand)
import Commands.UploadContractSource (mkUploadContractSourceCommandParser, runUploadContractSourceCommand)
import Commands.GetContractSource (mkGetContractSourceCommandParser, runGetContractSourceCommand)
import Commands.GetContractSourceAdjacency (mkGetContractSourceAdjacencyCommandParser, runGetContractSourceAdjacencyCommand)
import Commands.GetContractSourceClosure (mkGetContractSourceClosureCommandParser, runGetContractSourceClosureCommand)

mkCommandParser :: IO (Parser (IO ()))
mkCommandParser = do
  contractCommandParser <- mkContractCommandParser
  storeCommandParser <- mkStoreCommandParser
  pure . hsubparser . fold $
    [ command "contract" $ info contractCommandParser $ progDesc "Contract operations"
    , command "store" $ info storeCommandParser $ progDesc "Contract store operations"
    ]

mkContractCommandParser :: IO (Parser (IO ()))
mkContractCommandParser = do
  initCommandParser <- mkInitCommandParser
  getContractParser <- mkGetContractCommandParser
  getTransactionsParser <- mkGetTransactionsCommandParser
  nextCommandParser <- mkNextCommandParser
  applyInputsCommandParser <- mkApplyInputsCommandParser
  pure . hsubparser . fold $
    [ command "init" $ runInitCommand <$> initCommandParser
    , command "get" $ runGetContractCommand <$> getContractParser
    , command "transactions" $ runGetTransactionsCommand <$> getTransactionsParser
    , command "next" $ runNextCommand <$> nextCommandParser
    , command "apply-inputs" $ runApplyInputsCommand <$> applyInputsCommandParser
    ]

mkStoreCommandParser :: IO (Parser (IO ()))
mkStoreCommandParser = do
  uploadParser <- mkUploadContractSourceCommandParser
  getParser <- mkGetContractSourceCommandParser
  adjacencyParser <- mkGetContractSourceAdjacencyCommandParser
  closureParser <- mkGetContractSourceClosureCommandParser
  pure . hsubparser . fold $
    [ command "upload" $ runUploadContractSourceCommand <$> uploadParser
    , command "get" $ runGetContractSourceCommand <$> getParser
    , command "adjacency" $ runGetContractSourceAdjacencyCommand <$> adjacencyParser
    , command "closure" $ runGetContractSourceClosureCommand <$> closureParser
    ]
