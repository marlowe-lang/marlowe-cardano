module Language.Marlowe.Runtime.Contract.Store where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Variations (Variations)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Link (LinkError (TypeMismatch), linkBundle')
import Language.Marlowe.Object.Link qualified as O
import Language.Marlowe.Object.Types
  ( ContractHash (ContractHash)
  , Label
  , ObjectBundle (ObjectBundle)
  , LabelledObject (LabelledObject)
  , ObjectType (ContractType)
  , pattern SomeObjectType
  )
import Language.Marlowe.Object.Types qualified as O
import Language.Marlowe.Runtime.Contract.Api (MerkleizeInputsError)
import Language.Marlowe.Runtime.Core.Api
  (ContractWithAdjacency, MarloweVersionTag (V1))
import Marlowe.Plutus.Semantics (TransactionInput)
import Marlowe.Plutus.Semantics.Types (Action, Contract, State)
import Marlowe.Plutus.Semantics.Types qualified as Core
import Pipes (Pipe, await, yield, void)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

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

data ImportError
  = ContinuationNotInStore ContractHash
  | LinkError LinkError
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

-- | Domain-level type for the set of `Action` values that should be kept
-- readable on-chain during merkleization. The merkleizer keeps any `Case`
-- whose `Action` is in this set as a plain `Case` instead of rewriting it
-- as a `MerkleizedCase`.
newtype PreserveActions = PreserveActions {actions :: Set Action}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

newtype MainLabel = MainLabel {label :: Label}
  deriving (Show, Eq, Ord)

-- data Proxy a' a b' b m r
---- A Proxy is a monad transformer that receives and sends information on both an upstream and downstream interface.
----
---- The type variables signify:
----
---- a' and a - The upstream interface, where (a')s go out and (a)s come in
---- b' and b - The downstream interface, where (b')s go out and (b')s come in
---- m - The base monad
---- r - The return value
----
-- type Pipe a b = Proxy () a () b
-- type Producer b = Proxy X () () b
newtype BundleImporter m = BundleImporter
  { runBundleImporter
      :: MainLabel
      -> PreserveActions
      -> Pipe ObjectBundle (Map Label ContractHash) m (Either ImportError (Map Label ContractHash))
  }

mkBundleImporter
  :: Monad m
  => ContractStagingArea m
  -> BundleImporter m
mkBundleImporter stage@ContractStagingArea{..} = BundleImporter $ \(MainLabel main) preserve ->
  let
    preserveActions = preserve.actions
    loop objects = do
      bundle@(ObjectBundle bundleObjects) <- await
      case find (\LabelledObject {_label} -> _label == main) bundleObjects of
        Nothing ->
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, objects') -> do
              yield hashes
              loop objects'
        Just (LabelledObject _ ContractType _) -> do
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, _) -> do
              traceM "COMMITTING"
              lift $ void commit
              pure (Right hashes)
        Just (LabelledObject _ actual _) ->
          pure $ Left $ LinkError $ TypeMismatch (SomeObjectType ContractType) (SomeObjectType actual)

    step objects bundle = lift $ runExceptT $ do
      linked <- linkBundle' bundle (merkleizeAndStoreContracts preserveActions stage) objects
      case linked of
        Left linkError ->
          throwE $ LinkError linkError
        Right (hashPairs, objects') -> do
          lift $ void flush
          pure (Map.fromList (mapMaybe sequence hashPairs), objects')
   in loop mempty

merkleizeAndStoreContracts
  :: (Monad m)
  => Set Action
  -> ContractStagingArea m
  -> O.LinkedObject
  -> ExceptT ImportError m (O.LinkedObject, Maybe ContractHash)
merkleizeAndStoreContracts preserveActions stage = \case
  O.LinkedContract contract -> do
    merkleizedContract <- merkleizeAndStore preserveActions stage contract
    hash <- lift $ stageContract stage merkleizedContract
    pure (O.LinkedContract merkleizedContract, Just hash)
  obj -> pure (obj, Nothing)

merkleizeAndStore
  :: (Monad m)
  => Set Action
  -> ContractStagingArea m
  -> Core.Contract
  -> ExceptT ImportError m Core.Contract
merkleizeAndStore preserveActions stage = \case
  Core.Close -> pure Core.Close
  Core.Pay account payee token value contract ->
    Core.Pay account payee token value <$> merkleizeAndStore preserveActions stage contract
  Core.If obs c1 c2 ->
    Core.If obs <$> merkleizeAndStore preserveActions stage c1 <*> merkleizeAndStore preserveActions stage c2
  Core.When cases timeout contract ->
    Core.When
      <$> traverse (merkleizeAndStoreCase preserveActions stage) cases
      <*> pure timeout
      <*> merkleizeAndStore preserveActions stage contract
  Core.Let valueId value contract ->
    Core.Let valueId value <$> merkleizeAndStore preserveActions stage contract
  Core.Assert obs contract ->
    Core.Assert obs <$> merkleizeAndStore preserveActions stage contract

-- | Test whether the given `Action` should be kept readable on-chain
-- (i.e. its enclosing `Case` is preserved as `Case` rather than rewritten
-- as `MerkleizedCase`).
actionPreserved :: Set Action -> Core.Action -> Bool
actionPreserved preserveActions action =
  Set.member action preserveActions

merkleizeAndStoreCase
  :: (Monad m)
  => Set Action
  -> ContractStagingArea m
  -> Core.Case Core.Contract
  -> ExceptT ImportError m (Core.Case Core.Contract)
merkleizeAndStoreCase preserveActions stage@ContractStagingArea{..} = \case
  Core.Case action contract | actionPreserved preserveActions action ->
    -- Preserve the action: keep the `Case` constructor (the action stays
    -- readable on-chain) but recursively process the continuation so the
    -- rest of the contract is still merkleized.
    Core.Case action <$> merkleizeAndStore preserveActions stage contract
  Core.Case action Core.Close -> pure $ Core.Case action Core.Close
  Core.Case action contract -> do
    contract' <- merkleizeAndStore preserveActions stage contract
    ContractHash hash <- lift $ stageContract contract'
    pure $ Core.MerkleizedCase action $ BuiltinByteString hash
  Core.MerkleizedCase action hash -> do
    exists <- lift $ doesContractExist $ ContractHash (PV2.fromBuiltin @PV2.BuiltinByteString hash)
    if exists
      then pure $ Core.MerkleizedCase action hash
      else throwE $ ContinuationNotInStore $ O.fromCoreContractHash hash
