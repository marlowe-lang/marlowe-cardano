{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.TransferServer (
  ImportBundle,
  ImportError (..),
  MainLabel (..),
  mkImportBundle,
  merkleizeAndStoreContracts,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson qualified as A
import Data.Binary (Binary)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Variations (Variations)
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Link (LinkError(TypeMismatch), linkBundle')
import Language.Marlowe.Object.Link qualified as O
import Language.Marlowe.Object.Types (ContractHash (ContractHash), Label, ObjectBundle (ObjectBundle), LabelledObject (LabelledObject), ObjectType (ContractType), pattern SomeObjectType)
import Language.Marlowe.Object.Types qualified as O
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..))
import Marlowe.Plutus.Semantics.Types qualified as Core
import Pipes (Pipe, await, yield, void)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

data ImportError
  = ContinuationNotInStore ContractHash
  | LinkError LinkError
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (A.FromJSON, A.ToJSON, Binary, Variations)

merkleizeAndStoreContracts
  :: (Monad m)
  => Set.Set A.Value
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
  => Set.Set A.Value
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

-- | Compare an `Action` against the preserve-set by JSON shape.
-- FIXME: paluh: a more robust check would canonicalize both sides (e.g.
-- alphabetically-sort object keys, drop `null`s) so that minor formatting
-- differences don't defeat the match.
actionPreserved :: Set.Set A.Value -> Core.Action -> Bool
actionPreserved preserveActions action =
  Set.member (A.toJSON action) preserveActions

merkleizeAndStoreCase
  :: (Monad m)
  => Set.Set A.Value
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

newtype MainLabel = MainLabel {label :: Label}
  deriving (Show, Eq, Ord)


-- data Proxy a' a b' b m r
-- A Proxy is a monad transformer that receives and sends information on both an upstream and downstream interface.
--
-- The type variables signify:
--
-- a' and a - The upstream interface, where (a')s go out and (a)s come in
-- b' and b - The downstream interface, where (b')s go out and (b')s come in
-- m - The base monad
-- r - The return value
--
-- type Pipe a b = Proxy () a () b
-- type Producer b = Proxy X () () b
--
type ImportBundle m
  = MainLabel
  -> Set.Set A.Value
  -> Pipe ObjectBundle (Map Label ContractHash) m (Either ImportError (Map Label ContractHash))

mkImportBundle
  :: Monad m
  => ContractStagingArea m
  -> ImportBundle m
mkImportBundle stage@ContractStagingArea{..} (MainLabel main) preserveActions =
  loop mempty
  where
    loop objects = do
      bundle@(ObjectBundle bundleObjects) <- await
      case find (\LabelledObject { _label } -> _label == main) bundleObjects of
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

