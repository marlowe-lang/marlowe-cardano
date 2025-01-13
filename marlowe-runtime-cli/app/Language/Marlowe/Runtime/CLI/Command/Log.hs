{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.CLI.Command.Log where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.ByteString.Base16
import Data.Foldable (for_, traverse_)
import qualified Data.Text as T
import Language.Marlowe (pretty)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Protocol.Sync.Client
import Language.Marlowe.Runtime.CLI.Env (Env (..))
import Language.Marlowe.Runtime.CLI.Monad (CLI, askEnv, runCLIExceptT)
import Language.Marlowe.Runtime.CLI.Option (contractIdArgument)
import Language.Marlowe.Runtime.ChainSync.Api (
  AssetId (AssetId),
  BlockHeader (..),
  BlockNo (..),
  PolicyId (PolicyId),
  SlotNo (..),
  TokenName (TokenName),
  TxId (..),
  TxOutRef (..),
  toBech32,
  unBlockHeaderHash,
 )
import qualified Language.Marlowe.Runtime.Client as Client
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  renderContractId,
 )
import Language.Marlowe.Runtime.History.Api (ContractStep (..), CreateStep (..), RedeemStep (..))
import Options.Applicative
import System.Console.ANSI
import Text.PrettyPrint.Leijen (Doc, indent, putDoc)
import Prelude hiding (tail)

data LogCommand = LogCommand
  { tail :: Bool
  , showContract :: Bool
  , contractId :: ContractId
  }

logCommandParser :: ParserInfo LogCommand
logCommandParser = info parser $ progDesc "Display the history of a contract"
  where
    parser =
      LogCommand
        <$> tailOption
        <*> showContractOption
        <*> contractIdArgument "The ID of the contract to log"
    showContractOption =
      flag False True $
        mconcat
          [ long "show-contract"
          , short 'c'
          , help "Show the contract at each step"
          ]
    tailOption =
      flag False True $
        mconcat
          [ long "tail"
          , short 't'
          , help "Await new events from the contract and print them as they occur"
          ]

runLogCommand :: LogCommand -> CLI ()
runLogCommand LogCommand{..} =
  runCLIExceptT $
    ExceptT $
      Client.runMarloweSyncClient $
        MarloweSyncClient $
          pure $
            SendMsgFollowContract
              contractId
              ClientStFollow
                { recvMsgContractNotFound = pure $ Left @String "Contract not found"
                , recvMsgContractFound = \block version create -> do
                    liftIO $ showCreateStep showContract contractId block version create
                    pure $ requestNext version
                }
  where
    requestNext :: MarloweVersion v -> ClientStIdle v CLI (Either String ())
    requestNext = SendMsgRequestNext . next

    next :: MarloweVersion v -> ClientStNext v CLI (Either String ())
    next version =
      ClientStNext
        { recvMsgRollBackCreation = pure $ Left "Creation transaction was rolled back"
        , recvMsgRollBackward = \block -> do
            liftIO $ showRollback block
            pure $ requestNext version
        , recvMsgRollForward = \block steps -> do
            liftIO $ traverse_ (showStep showContract contractId block version) steps
            pure $ requestNext version
        , recvMsgWait = wait version
        }

    wait :: MarloweVersion v -> CLI (ClientStWait v CLI (Either String ()))
    wait version = do
      Env{..} <- askEnv
      delay <- liftIO $ newDelay 500_000 -- poll every 500 ms
      keepGoing <-
        liftIO $
          atomically $
            asum
              [ False <$ guard (not tail)
              , False <$ sigInt
              , True <$ waitDelay delay
              ]
      pure
        if keepGoing
          then SendMsgPoll $ next version
          else SendMsgCancel $ SendMsgDone $ Right ()

-- TODO allow output format to be specified via command line argument (e.g.
-- JSON)
showStep :: Bool -> ContractId -> BlockHeader -> MarloweVersion v -> ContractStep v -> IO ()
showStep showContract contractId BlockHeader{..} version step = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "transaction "
  case step of
    ApplyTransaction Transaction{transactionId, output = TransactionOutput{..}} -> do
      putStr $ T.unpack $ encodeBase16 $ unTxId transactionId
      putStrLn case scriptOutput of
        Nothing -> " (close)"
        _ -> ""
    RedeemPayout RedeemStep{..} -> do
      putStr $ T.unpack $ encodeBase16 $ unTxId redeemingTx
      putStrLn " (redeem)"
  setSGR [Reset]
  putStr "ContractId: "
  putStrLn $ T.unpack $ renderContractId contractId
  putStr "SlotNo:     "
  print $ unSlotNo slotNo
  putStr "BlockNo:    "
  print $ unBlockNo blockNo
  putStr "BlockId:    "
  putStrLn $ T.unpack $ encodeBase16 $ unBlockHeaderHash headerHash
  case step of
    ApplyTransaction Transaction{inputs, output} -> do
      putStr "Inputs:     "
      putStrLn case version of
        MarloweV1 -> show inputs
      putStrLn ""
      when showContract do
        let TransactionOutput{..} = output
        case scriptOutput of
          Just TransactionScriptOutput{..} -> do
            let contractDoc :: Doc
                contractDoc = indent 4 case version of
                  MarloweV1 -> pretty $ V1.marloweContract datum
            putDoc contractDoc
            putStrLn ""
          _ -> pure ()
      putStrLn ""
    RedeemPayout RedeemStep{..} -> case version of
      MarloweV1 -> do
        let AssetId (PolicyId policyId) (TokenName tokenName) = datum
        putStr "PolicyId:    "
        putStrLn . T.unpack . encodeBase16 $ policyId
        putStr "RoleToken:    "
        putStrLn . T.unpack . encodeBase16 $ tokenName

showCreateStep :: Bool -> ContractId -> BlockHeader -> MarloweVersion v -> CreateStep v -> IO ()
showCreateStep showContract contractId BlockHeader{..} version CreateStep{..} = do
  let TransactionScriptOutput scriptAddress _ _ datum = createOutput
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "transaction "
  putStr $ T.unpack $ encodeBase16 $ unTxId $ txId $ unContractId contractId
  putStrLn " (creation)"
  setSGR [Reset]
  putStr "ContractId:      "
  putStrLn $ T.unpack $ renderContractId contractId
  putStr "SlotNo:          "
  print $ unSlotNo slotNo
  putStr "BlockNo:         "
  print $ unBlockNo blockNo
  putStr "BlockId:         "
  putStrLn $ T.unpack $ encodeBase16 $ unBlockHeaderHash headerHash
  for_ (toBech32 scriptAddress) \addr -> do
    putStr "ScriptAddress:   "
    putStrLn $ T.unpack addr
  putStr "Marlowe Version: "
  putStrLn case version of
    MarloweV1 -> "1"
  let contractDoc :: Doc
      contractDoc = indent 4 case version of
        MarloweV1 -> pretty $ V1.marloweContract datum
  putStrLn ""
  when showContract do
    putDoc contractDoc
  putStrLn ""
  putStrLn ""

showRollback :: BlockHeader -> IO ()
showRollback BlockHeader{..} = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "rollback"
  setSGR [Reset]
  putStr "SlotNo:     "
  print $ unSlotNo slotNo
  putStr "BlockNo:    "
  print $ unBlockNo blockNo
  putStr "BlockId:    "
  putStrLn $ T.unpack $ encodeBase16 $ unBlockHeaderHash headerHash
