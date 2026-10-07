module Language.Marlowe.Runtime.Contract.Store.Memory where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Conc (throwSTM)
import GHC.IO (mkUserError)
import Language.Marlowe.Runtime.Contract.Api (MerkleizeInputsError (MerkleizeInputsIntervalError, MerkleizeInputsContractNotFound, MerkleizeInputsApplyNoMatch, MerkleizeInputsReduceAmbiguousInterval))
import Language.Marlowe.Runtime.Core.Api (ContractWithAdjacency (..), MarloweVersionTag (V1))
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..), ContractStore (..))
import Language.Marlowe.Object.Types (ContractHash(ContractHash))
-- import Language.Marlowe.Runtime.ChainSync.Api (ContractHash (..))
import Marlowe.Plutus.Merkle (dataHash)
import Marlowe.Plutus.Plate (Extract (extractAll))
import Marlowe.Plutus.Semantics.Types (Case (..), Contract (..), State, IntervalResult (IntervalTrimmed, IntervalError), Environment, Input (NormalInput, MerkleizedInput), InputContent, getAction)
import PlutusLedgerApi.V2 qualified as PV2
import UnliftIO.STM (STM, TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Marlowe.Plutus.Semantics (TransactionInput(..), ReduceResult (ContractQuiescent, RRAmbiguousTimeIntervalError), reduceContractUntilQuiescent, ApplyAction (AppliedAction, NotAppliedAction), applyAction, fixInterval)
import Control.Monad.Trans.Except (except, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (void)
import Debug.Trace (traceM)

-- | An in-memory implementation of ContractStore for use in testing and as a
-- model implementation.
createContractStoreInMemory :: STM (ContractStore STM)
createContractStoreInMemory = do
  store <- newTVar mempty
  pure
    ContractStore
      { createContractStagingArea = createContractStagingAreaInMemory store
      , getContract = getContract store
      , merkleizeInputs =
          merkleizeInputsDefault $
            (fmap . fmap) (\ContractWithAdjacency{..} -> contract)
              . getContract store
      , setGCRoots = const $ pure ()
      }
  where
    createContractStagingAreaInMemory :: TVar (Map ContractHash Contract) -> STM (ContractStagingArea STM)
    createContractStagingAreaInMemory store = do
      stagingArea <- newTVar (mempty :: Map ContractHash Contract)
      buffer <- newTVar (mempty :: Map ContractHash Contract)
      open <- newTVar True

      let close = writeTVar open False

          openedRequired :: forall a. STM a -> STM a
          openedRequired m =
            readTVar open >>= \case
              False -> throwSTM $ mkUserError "Contract staging area is no longer open"
              True -> m

          whenOpen :: forall a. STM a -> STM (Maybe a)
          whenOpen m =
            readTVar open >>= \case
              False -> pure Nothing
              True -> Just <$> m

          flush :: STM (Set ContractHash)
          flush = openedRequired do
            buffered <- readTVar buffer
            staged <- readTVar stagingArea
            writeTVar stagingArea $ Map.union staged buffered
            writeTVar buffer mempty
            pure $ Map.keysSet $ Map.difference staged buffered
      pure
        ContractStagingArea
          { stageContract = \contract -> openedRequired do
              let hash = ContractHash $ PV2.fromBuiltin $ dataHash contract
              modifyTVar buffer $ Map.insert hash contract
              pure hash
          , flush
          , commit = openedRequired do
              _ <- flush
              staged <- readTVar stagingArea
              stored <- readTVar store
              let
                updated = seq staged $ Map.union staged stored
              traceM $ "COMMITTING CONTRACTS: " <> show (Map.keys staged)
              writeTVar store updated
              writeTVar stagingArea mempty
              close
              pure $ Map.keysSet $ Map.difference stored staged
          , discard = void $ whenOpen do
              writeTVar buffer mempty
              writeTVar stagingArea mempty
              close
          , doesContractExist = \hash -> openedRequired do
              buffered <- readTVar buffer
              if Map.member hash buffered
                then pure True
                else do
                  staged <- readTVar stagingArea
                  if Map.member hash staged
                    then pure True
                    else do
                      stored <- readTVar store
                      pure $ Map.member hash stored
          }

    getContract :: TVar (Map ContractHash Contract) -> ContractHash -> STM (Maybe (ContractWithAdjacency 'V1))
    getContract store = \hash ->
      if hash == closeHash
        then
          pure $
            Just
              ContractWithAdjacency
                { contract = Close
                , contractHash = closeHash
                , adjacency = Set.empty
                , closure = Set.singleton closeHash
                }
        else runMaybeT (go hash)
      where
        go hash = do
          contract <- MaybeT $ Map.lookup hash <$> readTVar store
          adjacentContracts <- fmap Set.fromList $ traverse go $ Set.toList $ computeAdjacency contract
          pure
            ContractWithAdjacency
              { adjacency = Set.map contractHash adjacentContracts
              , closure = Set.insert hash $ foldMap closure adjacentContracts
              , contractHash = hash
              , ..
              }


computeAdjacency :: Contract -> Set ContractHash
computeAdjacency = foldMap getHash . extractAll
  where
    getHash :: Case Contract -> Set ContractHash
    getHash = \case
      MerkleizedCase _ hash -> Set.singleton $ ContractHash $ PV2.fromBuiltin hash
      _ -> mempty

-- | Static hash for the close contract.
closeHash :: ContractHash
closeHash = ContractHash $ PV2.fromBuiltin $ dataHash Close

merkleizeInputsDefault
  :: (Monad m)
  => (ContractHash -> m (Maybe Contract))
  -> Contract
  -> State
  -> TransactionInput
  -> m (Either MerkleizeInputsError TransactionInput)
merkleizeInputsDefault getContract' initialContract initialState TransactionInput{..} = runExceptT do
  case fixInterval txInterval initialState of
    IntervalTrimmed env state' -> go env state' txInputs initialContract []
    IntervalError err -> throwE $ MerkleizeInputsIntervalError err
  where
    applyInputCases
      :: State
      -> Environment
      -> Input
      -> [Case Contract]
      -> Either MerkleizeInputsError (InputContent, Either Contract PV2.BuiltinByteString, State)
    applyInputCases state env input = \case
      [] -> Left $ MerkleizeInputsApplyNoMatch input
      c : cs ->
        let action = getAction c
            continuation = case c of
              Case _ contract -> Left contract
              MerkleizedCase _ hash -> Right hash
         in case input of
              NormalInput content -> case applyAction env state content action of
                AppliedAction _ state' -> pure (content, continuation, state')
                NotAppliedAction -> applyInputCases state env input cs
              MerkleizedInput content hash' contract -> case continuation of
                Right hash
                  | hash == hash' && dataHash contract == hash -> case applyAction env state content action of
                      AppliedAction _ state' -> pure (content, continuation, state')
                      NotAppliedAction -> applyInputCases state env input cs
                _ -> Left $ MerkleizeInputsApplyNoMatch input

    applyInput'
      :: State
      -> Environment
      -> Input
      -> Contract
      -> Either MerkleizeInputsError (InputContent, Either Contract PV2.BuiltinByteString, State)
    applyInput' state env input = \case
      When cases _ _ -> applyInputCases state env input cases
      _ -> Left $ MerkleizeInputsApplyNoMatch input

    getContractExcept hash =
      lift (getContract' hash) >>= \case
        Nothing -> throwE $ MerkleizeInputsContractNotFound hash
        Just c -> pure c

    go env state inputs contract acc = case inputs of
      [] -> pure $ TransactionInput txInterval $ reverse acc
      input : inputs' -> case reduceContractUntilQuiescent env state contract of
        ContractQuiescent _ _ _ state' contract' -> do
          (content, continuation, state'') <- except $ applyInput' state' env input contract'
          case continuation of
            Left contract'' ->
              go env state'' inputs' contract'' (NormalInput content : acc)
            Right hash -> do
              contract'' <- getContractExcept $ ContractHash $ PV2.fromBuiltin hash
              go env state'' inputs' contract'' (MerkleizedInput content hash contract'' : acc)
        RRAmbiguousTimeIntervalError -> throwE $ MerkleizeInputsReduceAmbiguousInterval input

