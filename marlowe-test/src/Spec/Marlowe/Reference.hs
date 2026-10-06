-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Function to generate all valid transactions for contracts in JSON files.
module Spec.Marlowe.Reference (
  -- * Types
  ReferencePath (..),
  ReferenceTransaction (..),

  -- * Analysis
  processContract,

  -- * Testing
  arbitraryReferenceTransaction,
  readReferenceContracts,
  readReferencePaths,
  referenceFolder,
) where

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bifunctor (first)
import Data.List (isSuffixOf)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput (..), computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State (..), ada, mkRoleByteString)
import Language.Marlowe.FindInputs (getAllInputs)
import Paths_marlowe_test (getDataDir)
import PlutusLedgerApi.V2 (POSIXTime)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import Language.Marlowe.Analysis.FSSemantics (SlotLength (..))
import qualified PlutusTx.AssocMap as AM (empty, singleton)

referenceFolder :: FilePath
referenceFolder = "reference" </> "data"

readReferenceContracts :: IO [(FilePath, Contract)]
readReferenceContracts = readReferenceContracts' . (</> referenceFolder) =<< getDataDir

readReferenceContracts' :: FilePath -> IO [(FilePath, Contract)]
readReferenceContracts' folder =
  do
    contractFiles <- fmap (folder </>) . filter (".contract" `isSuffixOf`) <$> listDirectory folder
    forM contractFiles $
      \contractFile ->
        eitherDecodeFileStrict contractFile
          >>= \case
            Right contract -> pure (contractFile, contract)
            Left msg -> error $ "Failed parsing " <> contractFile <> ": " <> msg <> "."

readReferencePaths :: IO [ReferencePath]
readReferencePaths =
  do
    pathFiles <- fmap (referenceFolder </>) . filter (".paths" `isSuffixOf`) <$> listDirectory referenceFolder
    fmap concat
      . forM pathFiles
      $ \pathFile ->
        eitherDecodeFileStrict pathFile
          >>= \case
            Right paths -> pure $ filter (not . null . transactions) paths
            Left msg -> error $ "Failed parsing " <> pathFile <> ": " <> msg <> "."

arbitraryReferenceTransaction :: [ReferencePath] -> Gen GoldenTransaction
arbitraryReferenceTransaction paths =
  do
    ReferencePath{..} <- elements paths
    if length transactions > 1
      then do
        (ReferenceTransaction _ prior, ReferenceTransaction{..}) <- elements $ zip (init transactions) (tail transactions)
        pure (txOutState prior, txOutContract prior, input, output)
      else
        let ReferenceTransaction{..} = head transactions
         in pure (state, contract, input, output)

data ReferencePath = ReferencePath
  { contract :: Contract
  , state :: State
  , transactions :: [ReferenceTransaction]
  }
  deriving (Generic, Show)

instance FromJSON ReferencePath

instance ToJSON ReferencePath

data ReferenceTransaction = ReferenceTransaction
  { input :: TransactionInput
  , output :: TransactionOutput
  }
  deriving (Generic, Show)

instance FromJSON ReferenceTransaction

instance ToJSON ReferenceTransaction

processContract
  :: FilePath
  -> FilePath
  -> ExceptT String IO ()
processContract contractFile pathsFile =
  do
    let slotLength = SlotLength 1_000
    contract <- ExceptT $ first show <$> eitherDecodeFileStrict contractFile
    traces <- ExceptT $ first show <$> getAllInputs slotLength contract Nothing
    paths <- runTransactions contract `mapM` traces
    lift $ encodeFile pathsFile paths

runTransactions
  :: Contract
  -> (POSIXTime, [TransactionInput])
  -> ExceptT String IO ReferencePath
runTransactions contract (startTime, inputs) =
  do
    let state = makeState startTime
    transactions <- runTransaction contract state inputs
    pure ReferencePath{..}

runTransaction
  :: Contract
  -> State
  -> [TransactionInput]
  -> ExceptT String IO [ReferenceTransaction]
runTransaction _ _ [] = pure []
runTransaction contract state (input : inputs) =
  case computeTransaction input state contract of
    Error err -> throwError $ show err
    output@TransactionOutput{..} -> (ReferenceTransaction{..} :) <$> runTransaction txOutContract txOutState inputs

makeState
  :: POSIXTime
  -> State
makeState minTime =
  let accounts = AM.singleton (mkRoleByteString "", ada) 30_000_000 -- Note that 30 ada exceeds min-UTxO for current protocol parameters.
      choices = AM.empty
      boundValues = AM.empty
   in State{..}
