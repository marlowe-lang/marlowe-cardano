-----------------------------------------------------------------------------
--
-- Module      :  Language.Marlowe.Core.V1.Merkle
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- | Merkleize Marlowe contracts.
module Language.Marlowe.Core.V1.Merkle (
  -- * Types
  Continuations,
  MerkleizedContract (..),

  -- * Merkleization
  deepMerkleize,
  merkleizedContract,
  shallowMerkleize,

  -- * Demerkleization
  deepDemerkleize,
  demerkleizeContract,
  shallowDemerkleize,

  -- * Inputs
  merkleizeInput,
  merkleizeInputs,
) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fix (fix)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.String (IsString (..))
import Language.Marlowe.Core.V1.Semantics (
  ApplyAction (..),
  ReduceResult (..),
  TransactionInput (..),
  applyAction,
  fixInterval,
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  Case (..),
  Contract (..),
  Input (..),
  IntervalResult (..),
  State,
  TimeInterval,
 )
import PlutusLedgerApi.V1 (DatumHash (..), toBuiltin, toData)

import Cardano.Api (SerialiseAsRawBytes (..), hashScriptDataBytes, unsafeHashableScriptData)
import Cardano.Api.Plutus (fromPlutusData)
import qualified Data.Map.Strict as M (Map, lookup, singleton)

-- | Hashed continuations of a Marlowe contract.
type Continuations = M.Map DatumHash Contract

-- | A merkleized contract.
data MerkleizedContract = MerkleizedContract
  { mcContract :: Contract
  , mcContinuations :: Continuations
  }
  deriving (Show)

-- | Bundle the merkleized contract.
merkleizedContract
  :: Writer Continuations Contract
  -> MerkleizedContract
merkleizedContract = uncurry MerkleizedContract . runWriter

-- | Unbundled the merkleized contract.
demerkleizeContract
  :: Continuations
  -> ReaderT Continuations m Contract
  -> m Contract
demerkleizeContract = flip runReaderT

-- | Merkleize any top-level case statements in a contract.
shallowMerkleize
  :: Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for the merkleized contract.
shallowMerkleize = merkleize pure

-- | Merkleize all case statements in a contract.
deepMerkleize
  :: Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for the merkleized contract.
deepMerkleize = fix merkleize

-- | Merkleize selected case statements in a contract.
merkleize
  :: (Contract -> Writer Continuations Contract)
  -- ^ Action to continue merkleization.
  -> Contract
  -- ^ The contract.
  -> Writer Continuations Contract
  -- ^ Action for merkleizing the selected case statements.
merkleize _ Close = pure Close
merkleize f (Pay accountId payee token value contract) = Pay accountId payee token value <$> merkleize f contract
merkleize f (If observation thenContract elseContract) = If observation <$> merkleize f thenContract <*> merkleize f elseContract
merkleize f (Let valueId value contract) = Let valueId value <$> merkleize f contract
merkleize f (Assert observation contract) = Assert observation <$> merkleize f contract
merkleize f (When cases timeout contract) = When <$> mapM merkleizeCase cases <*> pure timeout <*> merkleize f contract
  where
    merkleizeCase c@(Case _ Close) = pure c
    merkleizeCase (Case action continuation) =
      do
        continuation' <- f continuation
        let mh =
              toBuiltin $ serialiseToRawBytes $ hashScriptDataBytes $ unsafeHashableScriptData $ fromPlutusData $ toData continuation'
        tell $ DatumHash mh `M.singleton` continuation'
        pure $ MerkleizedCase action mh
    merkleizeCase mc = pure mc

-- | Demerkleize any top-level case statements in a contract.
shallowDemerkleize
  :: (IsString e)
  => (MonadError e m)
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for the demerkleized contract.
shallowDemerkleize = flip demerkleize pure

-- | Demerkleize all case statements in a contract.
deepDemerkleize
  :: (IsString e)
  => (MonadError e m)
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for the demerkleized contract.
deepDemerkleize = fix . demerkleize

-- | Demerkleize selected case statements in a contract.
demerkleize
  :: forall e m
   . (IsString e)
  => (MonadError e m)
  => Bool
  -- ^ Throw an error if all required continuations are not present.
  -> (Contract -> ReaderT Continuations m Contract)
  -- ^ Action to continue demerkleization.
  -> Contract
  -- ^ The contract.
  -> ReaderT Continuations m Contract
  -- ^ Action for demerkleized the selected case statements.
demerkleize _ _ Close = pure Close
demerkleize requireContinuations f (Pay accountId payee token value contract) = Pay accountId payee token value <$> demerkleize requireContinuations f contract
demerkleize requireContinuations f (If observation thenContract elseContract) =
  If observation <$> demerkleize requireContinuations f thenContract <*> demerkleize requireContinuations f elseContract
demerkleize requireContinuations f (Let valueId value contract) = Let valueId value <$> demerkleize requireContinuations f contract
demerkleize requireContinuations f (Assert observation contract) = Assert observation <$> demerkleize requireContinuations f contract
demerkleize requireContinuations f (When cases timeout contract) = When <$> mapM demerkleizeCase cases <*> pure timeout <*> demerkleize requireContinuations f contract
  where
    demerkleizeCase :: Case Contract -> ReaderT Continuations m (Case Contract)
    demerkleizeCase mc@(MerkleizedCase action mh) =
      do
        continuation' <- asks . M.lookup $ DatumHash mh
        case continuation' of
          Just continuation ->
            fmap (Case action) . demerkleize requireContinuations f =<< f continuation
          Nothing ->
            if requireContinuations
              then throwError . fromString $ "Missing continuation for hash " <> show mh <> "."
              else pure mc
    demerkleizeCase (Case action continuation) = Case action <$> demerkleize requireContinuations f continuation

-- | Merkleize whatever inputs need merkleization before application to a contract.
merkleizeInputs
  :: (IsString e)
  => (MonadError e m)
  => MerkleizedContract
  -- ^ The contract information.
  -> State
  -- ^ The prior state of the contract.
  -> TransactionInput
  -- ^ The input to the contract.
  -> m TransactionInput
  -- ^ Action for the merkleized input to the contract.
merkleizeInputs MerkleizedContract{..} state TransactionInput{..} =
  TransactionInput txInterval
    . snd
    <$> foldM (merkleizeInput txInterval mcContinuations) ((state, mcContract), []) txInputs

-- | Merkleize an input if needed before application to a contract.
merkleizeInput
  :: (IsString e)
  => (MonadError e m)
  => TimeInterval
  -- ^ The validity interval.
  -> Continuations
  -- ^ The available continuations.
  -> ((State, Contract), [Input])
  -- ^ The current state and contract, along with the prior inputs.
  -> Input
  -- ^ The input.
  -> m ((State, Contract), [Input])
  -- ^ The new state and contract, along with the prior inputs.
merkleizeInput txInterval continuations ((state, contract), inputs) input =
  let inputContent =
        case input of
          NormalInput inputContent' -> inputContent'
          MerkleizedInput inputContent' _ _ -> inputContent'
   in case fixInterval txInterval state of
        IntervalTrimmed env fixState ->
          case reduceContractUntilQuiescent env fixState contract of
            ContractQuiescent _ _ _ curState cont ->
              case cont of
                (When cases _ _) ->
                  let match [] = throwError $ fromString "No match when merkleizing input."
                      match (Case action continuation : cases') =
                        case applyAction env curState inputContent action of
                          AppliedAction _ newState -> pure ((newState, continuation), inputs ++ [NormalInput inputContent])
                          NotAppliedAction -> match cases'
                      match (MerkleizedCase action mh : cases') =
                        -- Note that this does not handle the situation where two `Case` statements have the same action,
                        -- but different continuation hashes.
                        case applyAction env curState inputContent action of
                          AppliedAction _ newState ->
                            case M.lookup (DatumHash mh) continuations of
                              Just continuation -> pure ((newState, continuation), inputs ++ [MerkleizedInput inputContent mh continuation])
                              Nothing -> throwError $ fromString "No continuation when merkleizing input."
                          NotAppliedAction -> match cases'
                   in match cases
                _ -> throwError $ fromString "No quiescence when merkleizing input."
            _ -> throwError $ fromString "Reduction failure when merkelizing input."
        _ -> throwError $ fromString "Bad interval when merkleizing input."
