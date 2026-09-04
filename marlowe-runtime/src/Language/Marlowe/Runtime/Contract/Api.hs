{-# LANGUAGE StrictData #-}

-- | The contract-store data types that are shared between the runtime, the
-- web server, and the indexer. Mirrors the original
-- `Language.Marlowe.Runtime.Contract.Api` from the `marlowe-cardano` runtime
-- (see `.external-references/marlowe-cardano/marlowe-runtime/contract-api/Language/Marlowe/Runtime/Contract/Api.hs:127-140`).
module Language.Marlowe.Runtime.Contract.Api
  ( ContractWithAdjacency (..)
  , MerkleizeInputsError (..)
  ) where

import Data.Set ()
import GHC.Generics (Generic)
import Language.Marlowe.Object.Types (ContractHash)
import Language.Marlowe.Runtime.Core.Api (ContractWithAdjacency (..))
import Marlowe.Plutus.Semantics.Types (Input, IntervalError)
import Data.Binary (Binary)
import Data.Variations (Variations)

data MerkleizeInputsError
  = MerkleizeInputsContractNotFound ContractHash
  | MerkleizeInputsApplyNoMatch Input
  | MerkleizeInputsApplyAmbiguousInterval Input
  | MerkleizeInputsReduceAmbiguousInterval Input
  | MerkleizeInputsIntervalError IntervalError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, Variations)
