{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | Marlowe open-roles validator.
module Marlowe.Plutus.OpenRoles (
  -- * Validation
  mkOpenRoleValidator,
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose, ScriptContext,
 )
import qualified PlutusTx.AssocMap as AssocMap

import PlutusTx.Prelude as PlutusTxPrelude (
  Bool (False),
  BuiltinData,
 )

import qualified PlutusTx
import qualified Prelude as Haskell
import PlutusLedgerApi.Data.V2 (TxInInfo)

-- By decoding only the part of the script context I was able
-- to bring down the size of the validator from 4928 to 4540 bytes.
data SubScriptContext = SubScriptContext
  { subScriptContextTxInfo :: SubTxInfo
  , subScriptContextPurpose :: ScriptPurpose
  }
  deriving (Generic, Haskell.Eq, Haskell.Show)

data SubTxInfo = SubTxInfo
  { subTxInfoInputs :: [TxInInfo]
  , subTxInfoReferenceInputs :: BuiltinData
  , subTxInfoOutputs :: BuiltinData
  , subTxInfoFee :: BuiltinData
  , subTxInfoMint :: BuiltinData
  , subTxInfoDCert :: BuiltinData
  , subTxInfoWdrl :: BuiltinData
  , subTxInfoValidRange :: BuiltinData
  , subTxInfoSignatories :: BuiltinData
  , subTxInfoRedeemers :: AssocMap.Map ScriptPurpose Redeemer
  , subTxInfoData :: BuiltinData
  , subTxInfoId :: BuiltinData
  }
  deriving (Generic, Haskell.Show, Haskell.Eq)


{- FIXME: paluh - migrate -}
{- Open Role validator - it releases role token(s) (you can put more coins of the same role token to it) based on a few conditions:

   1. Value should contain only min. ada and a specific role token(s).
   2. Transaction should spend Marlowe output which contains corresponding thread token (the same currency symbol as
   role and token name as defined in the inlined datum).
   3. The list of Marlowe inputs should contain at least one action either `IDeposit` or `IChoice` dedicated for the same role
   party as the locked role token.

   In the current version we assume that thread token has the same currency as the role token, even though this might actually be undesired.

   Error codes:

   "1" - Own input (never happen) or Marlowe input not found.
   "2" - Invalid own value - we expect only the role token(s) and min ADA.
   "3" - Invalid Marlowe redeemer.
   "4" - Missing thread token.
-}
{-# INLINEABLE mkOpenRoleValidator #-}
mkOpenRoleValidator
  :: ScriptHash
  -- ^ The hash of the corresponding Marlowe validator.
  -- -> Semantics.TokenName
  -- -- ^ Datum should be a thread token name.
  -- - _
  -- -- ^ Redeemer
  -> ScriptContext -- SubScriptContext
  -- ^ The script context.
  -> Bool
-- mkOpenRoleValidator
--   marloweValidatorHash
--   threadTokenName
--   SubScriptContext
--     { subScriptContextTxInfo = SubTxInfo{subTxInfoInputs, subTxInfoRedeemers}
--     , subScriptContextPurpose = Spending txOutRef
--     } = do
--     let -- Performance:
--         -- In the case of three inputs `find` seems to be faster than custom single pass over the list.
--         -- Inlined pattern matching over `Maybe` in both cases also seems to be faster than separate helper function.
--         ownInput = case List.find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) subTxInfoInputs of
--           Just input -> input
--           Nothing -> traceError "1" -- Own input not found.
--         marloweInput = case List.find
--           ( \TxInInfo{txInInfoResolved} -> addressCredential (txOutAddress txInInfoResolved) == ScriptCredential marloweValidatorHash
--           )
--           subTxInfoInputs of
--           Just input -> input
--           Nothing -> traceError "1" -- Marlowe input not found.
--         TxInInfo{txInInfoResolved = TxOut{txOutValue = ownValue}} = ownInput
-- 
--         -- Extract role token information from the own input `Value`.
--         (currencySymbol, roleName) = do
--           let valuesList = AssocMap.toList $ getValue ownValue
--           -- Value should contain only min. ADA and a specific role token(s) (we can have few coins of the same role
--           -- token - they are all released).
--           -- Performance: `find` performs here clearly worse.
--           case valuesList of
--             [(possibleAdaSymbol, _), (currencySymbol, AssocMap.toList -> [(roleName, _)])]
--               | possibleAdaSymbol PlutusTxPrelude.== adaSymbol -> (currencySymbol, roleName)
--             [(currencySymbol, AssocMap.toList -> [(roleName, _)]), _] -> (currencySymbol, roleName)
--             _ -> traceError "2" -- Invalid value - we expect only the role token(s).
-- 
--         -- In order to release the role token we have to encounter an action which uses/unlocks the role.
--         -- All the other actions will be checked by Marlowe validator itself.
--         marloweRedeemerOk = do
--           let TxInInfo{txInInfoOutRef = marloweTxOutRef} = marloweInput
--               inputContentUsesRole (Semantics.IDeposit _ (Semantics.Role role) _ _) = role PlutusTxPrelude.== roleName
--               inputContentUsesRole (Semantics.IChoice (Semantics.ChoiceId _ (Semantics.Role role)) _) = role PlutusTxPrelude.== roleName
--               inputContentUsesRole _ = False
-- 
--               inputUsesRole (MerkleizedTxInput inputContent _) = inputContentUsesRole inputContent
--               inputUsesRole (Input inputContent) = inputContentUsesRole inputContent
-- 
--               inputs :: MarloweInput
--               inputs = case AssocMap.lookup (Spending marloweTxOutRef) subTxInfoRedeemers of
--                 Nothing -> traceError "3" -- Invalid Marlowe redeemer
--                 Just (Redeemer bytes) -> case fromBuiltinData bytes of
--                   Just inputs -> inputs
--                   _ -> traceError "3" -- Invalid Marlowe redeemer
--           isJust $ List.find inputUsesRole inputs
-- 
--         -- Check the Marlowe input `Value` for the thread token.
--         threadTokenOk = do
--           let marloweValue = txOutValue $ txInInfoResolved marloweInput
--           traceIfFalse "4" (valueOf marloweValue currencySymbol threadTokenName > 0)
--     marloweRedeemerOk && threadTokenOk
mkOpenRoleValidator _ _ = False

PlutusTx.makeLift ''SubTxInfo
PlutusTx.makeIsDataIndexed ''SubTxInfo [('SubTxInfo, 0)]

PlutusTx.makeLift ''SubScriptContext
PlutusTx.makeIsDataIndexed ''SubScriptContext [('SubScriptContext, 0)]

