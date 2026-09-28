module Commands.GetTransactions where

import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.ContractId (contractIdParser)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (ServantClientRunner (ServantClientRunner), mkServantClientRunnerParser)
import Control.Monad (forM)
import Language.Marlowe.Runtime.Web.Client (getContractTransaction, getContractTransactions)
import Language.Marlowe.Runtime.Web.Contract.API (ContractId)
import Language.Marlowe.Runtime.Web.Tx.API (Tx (..), TxHeader (..))
import Options.Applicative (ParserInfo, info, progDesc, switch, long, help)
import Servant.Client (ClientError)

data GetTransactionsCommand = GetTransactionsCommand
  { contractId :: ContractId
  , full :: Bool
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

mkGetTransactionsCommandParser :: IO (ParserInfo GetTransactionsCommand)
mkGetTransactionsCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get the transactions of a Marlowe contract from the web server."
    prs =
      GetTransactionsCommand
        <$> contractIdParser
        <*> switch do
          long "full"
            <> help "Fetch the full details of every transaction instead of just the headers."
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runGetTransactionsCommand :: GetTransactionsCommand -> IO ()
runGetTransactionsCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (headersResult :: Either ClientError [TxHeader]) <- runWebClient $ getContractTransactions cmd.contractId
  case headersResult of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right headers
      | cmd.full -> do
          (detailsResult :: Either ClientError [Tx]) <-
            runWebClient $ forM headers $ \h ->
              getContractTransaction cmd.contractId h.transactionId
          case detailsResult of
            Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
            Right txs -> emitResponse cmd.messageFormat txs
      | otherwise -> emitResponse cmd.messageFormat headers
