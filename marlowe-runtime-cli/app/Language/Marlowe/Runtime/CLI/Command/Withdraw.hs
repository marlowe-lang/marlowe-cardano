{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.CLI.Command.Withdraw where

import qualified Cardano.Api as C
import Cardano.Api.Shelley (BabbageEraOnwards (..), File (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Aeson (toJSON)
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.CLI.Command.Tx (SigningMethod (Manual), TxCommand (..), txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLIExceptT)
import Language.Marlowe.Runtime.CLI.Option (marloweVersionParser, txOutRefParser)
import Language.Marlowe.Runtime.ChainSync.Api (TxOutRef)
import Language.Marlowe.Runtime.Client (withdraw)
import Language.Marlowe.Runtime.Core.Api (
  MarloweVersion (MarloweV1),
  SomeMarloweVersion (SomeMarloweVersion),
 )
import Language.Marlowe.Runtime.Transaction.Api (WithdrawError, WithdrawTx (..), WithdrawTxInEra (..))
import Options.Applicative

data WithdrawCommand = WithdrawCommand
  { marloweVersion :: SomeMarloweVersion
  , payouts :: Set TxOutRef
  }

data WithdrawCommandError
  = WithdrawFailed WithdrawError
  | TransactionFileWriteFailed (C.FileError ())
  deriving (Show)

withdrawCommandParser :: ParserInfo (TxCommand WithdrawCommand)
withdrawCommandParser = info (txCommandParser False parser) $ progDesc "Withdraw funds paid roles in contracts"
  where
    parser = WithdrawCommand <$> marloweVersionParser <*> payoutsParser
    payoutsParser =
      fmap Set.fromList $
        some $
          argument txOutRefParser $
            mconcat
              [ metavar "TX_ID#TX_IX"
              , help "A payout output to withdraw."
              ]

runWithdrawCommand :: TxCommand WithdrawCommand -> CLI ()
runWithdrawCommand TxCommand{walletAddresses, signingMethod, subCommand = WithdrawCommand{..}} = case marloweVersion of
  SomeMarloweVersion MarloweV1 -> runCLIExceptT do
    WithdrawTx era WithdrawTxInEra{..} <-
      ExceptT $ first WithdrawFailed <$> withdraw MarloweV1 walletAddresses payouts
    case signingMethod of
      Manual outputFile -> do
        ExceptT @_ @_ @() $
          liftIO $
            first TransactionFileWriteFailed <$> case era of
              BabbageEraOnwardsBabbage -> C.writeFileTextEnvelope (File outputFile) Nothing txBody
              BabbageEraOnwardsConway -> C.writeFileTextEnvelope (File outputFile) Nothing txBody
        let txId = C.getTxId txBody
            res =
              A.object
                [("txId", toJSON txId)]
        liftIO . print $ A.encode res
