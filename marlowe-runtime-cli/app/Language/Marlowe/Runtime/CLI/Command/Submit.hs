{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.CLI.Command.Submit where

import Cardano.Api (AsType (..), File (..), FromSomeType (..), Tx)
import qualified Cardano.Api as C
import Cardano.Api.Shelley (BabbageEraOnwards (..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Aeson (toJSON)
import qualified Data.Aeson as A
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Base16 (encodeBase16)
import Language.Marlowe.Runtime.CLI.Env (Env (..))
import Language.Marlowe.Runtime.CLI.Monad (CLI, asksEnv, runCLIExceptT)
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (BlockHeader),
  BlockHeaderHash (unBlockHeaderHash),
  BlockNo (unBlockNo),
  SlotNo (unSlotNo),
 )
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand (Submit), SubmitError)
import Network.Protocol.Job.Client (
  ClientStAwait (SendMsgDetach, SendMsgPoll),
  ClientStCmd (..),
  ClientStInit (SendMsgExec),
  JobClient (JobClient),
 )
import Options.Applicative

newtype SubmitCommand = SubmitCommand
  { txFile :: FilePath
  }

submitCommandParser :: ParserInfo SubmitCommand
submitCommandParser =
  info parser $
    progDesc "Submit a signed transaction to the Cardano node. Expects the CBOR bytes of the signed Tx from stdin."
  where
    parser =
      SubmitCommand
        <$> txFileParser
    txFileParser =
      strArgument $
        mconcat
          [ metavar "FILE_PATH"
          , help "A file containing the CBOR bytes of the signed transaction to submit."
          ]

data ClientSubmitError
  = SubmitFailed SubmitError
  | SubmitInterrupted
  | TransactionDecodingFailed
  deriving (Show)

data TxInEraWithReferenceScripts where
  TxInEraWithReferenceScripts
    :: BabbageEraOnwards era -> Tx era -> TxInEraWithReferenceScripts

runSubmitCommand :: SubmitCommand -> CLI ()
runSubmitCommand SubmitCommand{txFile} = runCLIExceptT do
  TxInEraWithReferenceScripts era tx <-
    ExceptT $
      liftIO $
        first (const TransactionDecodingFailed)
          <$> C.readFileTextEnvelopeAnyOf
            [ FromSomeType (AsTx AsBabbageEra) $ TxInEraWithReferenceScripts BabbageEraOnwardsBabbage
            , FromSomeType (AsTx AsConwayEra) $ TxInEraWithReferenceScripts BabbageEraOnwardsConway
            ]
            (File txFile)
  let cmd = Submit era tx
      next =
        ClientStCmd
          { recvMsgAwait = \_ _ -> do
              delay <- liftIO $ newDelay 500_000 -- poll every 500 ms
              sigIntVar <- asksEnv sigInt
              keepGoing <-
                liftIO $
                  atomically $
                    False <$ sigIntVar <|> True <$ waitDelay delay
              pure
                if keepGoing
                  then SendMsgPoll next
                  else SendMsgDetach (Left SubmitInterrupted)
          , recvMsgFail = \err -> do
              liftIO $ putStrLn "recvMsgFailed"
              liftIO $ print err
              pure . Left . SubmitFailed $ err
          , recvMsgSucceed = pure . Right
          }
      jobClient = JobClient . pure . SendMsgExec cmd $ next
  BlockHeader slotNo hash blockNo <- ExceptT $ runMarloweTxClient jobClient
  let res =
        A.object
          [ ("slotNo", toJSON $ unSlotNo slotNo)
          , ("blockHeaderHash", toJSON . encodeBase16 $ unBlockHeaderHash hash)
          , ("blockNo", toJSON $ unBlockNo blockNo)
          ]
  liftIO . print $ A.encode res
