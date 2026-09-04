module Language.Marlowe.Runtime.Contract.Store where

import Data.Set (Set)
import Language.Marlowe.Runtime.Contract.Api (MerkleizeInputsError)
import Language.Marlowe.Runtime.Core.Api (ContractWithAdjacency, MarloweVersionTag (V1))
import Marlowe.Plutus.Semantics.Types (Contract, State)
import Marlowe.Plutus.Semantics (TransactionInput)
import Language.Marlowe.Object.Types (ContractHash)

data ContractStore m = ContractStore
  { createContractStagingArea :: m (ContractStagingArea m)
  , getContract :: ContractHash -> m (Maybe (ContractWithAdjacency 'V1))
  , merkleizeInputs :: Contract -> State -> TransactionInput -> m (Either MerkleizeInputsError TransactionInput)
  , setGCRoots :: Set ContractHash -> m ()
  }

hoistContractStore
  :: (Functor m)
  => (forall x. m x -> n x)
  -> ContractStore m
  -> ContractStore n
hoistContractStore f ContractStore{..} =
  ContractStore
    { createContractStagingArea = f $ hoistContractStagingArea f <$> createContractStagingArea
    , getContract = f . getContract
    , merkleizeInputs = (fmap . fmap) f . merkleizeInputs
    , setGCRoots = f . setGCRoots
    }

data ContractStagingArea m = ContractStagingArea
  { stageContract :: Contract -> m ContractHash
  , flush :: m (Set ContractHash)
  , commit :: m (Set ContractHash)
  , discard :: m ()
  , doesContractExist :: ContractHash -> m Bool
  }

hoistContractStagingArea
  :: (forall x. m x -> n x)
  -> ContractStagingArea m
  -> ContractStagingArea n
hoistContractStagingArea f ContractStagingArea{..} =
  ContractStagingArea
    { stageContract = f . stageContract
    , flush = f flush
    , commit = f commit
    , discard = f discard
    , doesContractExist = f . doesContractExist
    }
