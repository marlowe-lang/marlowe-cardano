{-# LANGUAGE StrictData #-}

-- FIXME: We should move most of this module  to the `**.Indexer.Api`
module Language.Marlowe.Runtime.Indexer.MarloweBlock where

import Data.Aeson (ToJSON)
import Data.Binary (Binary)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Variations (Variations)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api
    ( BlockHeader,
      TxId,
      TxIx,
      TxOutRef(..)
    )
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.History.Api (
  MarloweApplyInputsTransaction (..),
  SomeCreateStep (..),
  MarloweWithdrawTransaction (..),
  UnspentContractOutput (..), ExtractMarloweTransactionError,
 )

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: NonEmpty MarloweTransaction
  }
  deriving (Eq, Show, Generic)

-- Those cases would indicate a **critical** problem in the
-- validator itself as minting policy should guarantee that
-- these are never possible.
data ExtractCreationError
  = ByronAddress
  | NoInitDatum
  | InvalidInitDatum
  | NonScriptAddress
  | InvalidScriptHash
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

-- FIXME [review]: Left for reference. Do we use it?
-- instance Arbitrary ExtractCreationError where
--   arbitrary =
--     elements
--       [ TxIxNotFound
--       , ByronAddress
--       , NonScriptAddress
--       , InvalidScriptHash
--       , NoInitDatum
--       , InvalidInitDatum
--       , NotCreationTransaction
--       ]
--   shrink = genericShrink

data MarloweInvalidCreateTransaction = MarloweInvalidCreateTransaction
  { txId :: TxId
  , errors :: Map TxIx ExtractCreationError
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data MarloweCreateTransaction = MarloweCreateTransaction
  { txId :: TxId
  , newContracts :: Map TxIx SomeCreateStep
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

data MarloweTransaction
  = ApplyInputsTransaction MarloweApplyInputsTransaction
  | CreateTransaction MarloweCreateTransaction
  | InvalidApplyInputsTransaction TxId (Set TxOutRef) ExtractMarloweTransactionError
  | InvalidCreateTransaction MarloweInvalidCreateTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  deriving (Eq, Show, Generic)

-- | The global Marlowe UTxO set
data MarloweUTxO = MarloweUTxO
  { unspentContractOutputs :: Map ContractId UnspentContractOutput
  -- ^ The UTxO set to the marlowe validators keyed by the contract ID
  , unspentPayoutOutputs :: Map ContractId (Set TxOutRef)
  -- ^ The UTxO set to the payout validators keyed by the contract Id
  }
  deriving (Eq, Show, Generic)

instance ToJSON MarloweUTxO

