{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-full-laziness #-}
{-# OPTIONS -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fno-omit-interface-pragmas #-}
{-# OPTIONS -fno-spec-constr #-}
{-# OPTIONS -fno-specialise #-}
{-# OPTIONS -fno-strictness #-}
{-# OPTIONS -fno-unbox-small-strict-fields #-}
{-# OPTIONS -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fobject-code #-}

module Marlowe.Plutus.Binaries.Devel where

import Marlowe.Plutus.Binaries.Core (applyArg, hashScript)
import Marlowe.Plutus.Scripts (mkMarloweValidator, mkRolePayoutValidator)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusLedgerApi.Data.V1 (ScriptHash)
import PlutusTx (CompiledCode)
import PlutusTx.Blueprint.PlutusVersion (PlutusVersion (PlutusV3))
import PlutusTx.Prelude (BuiltinData, BuiltinUnit, check, ($))
import PlutusTx qualified
import qualified PlutusLedgerApi.V3 as PV3
import Marlowe.Plutus.RoleTokens (RoleTokens, mkRoleTokensHash, mkPolicy, wrapMintingPolicy)
import qualified Prelude as Haskell

{-# INLINEABLE rolePayoutValidator #-}
rolePayoutValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
rolePayoutValidator =
  $$(PlutusTx.compile [||rolePayoutValidator'||])
 where
  rolePayoutValidator' :: BuiltinData -> BuiltinUnit
  rolePayoutValidator' ctx =
    check $ mkRolePayoutValidator (unsafeFromBuiltinData ctx)

rolePayoutValidatorHash :: ScriptHash
rolePayoutValidatorHash = hashScript PlutusV3 rolePayoutValidator

rolePayoutValidatorBytes :: SerialisedScript
rolePayoutValidatorBytes = serialiseCompiledCode rolePayoutValidator

{-# INLINEABLE marloweValidator #-}
marloweValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
marloweValidator =
  $$(PlutusTx.compile [||marloweValidator'||])
    `applyArg` rolePayoutValidatorHash
 where
  marloweValidator' :: ScriptHash -> BuiltinData -> BuiltinUnit
  marloweValidator' rolePayoutHash ctx =
    check $ mkMarloweValidator rolePayoutHash (unsafeFromBuiltinData ctx)

marloweValidatorHash :: ScriptHash
marloweValidatorHash = hashScript PlutusV3 marloweValidator

marloweValidatorBytes :: SerialisedScript
marloweValidatorBytes = serialiseCompiledCode marloweValidator

mkRoleTokensPolicy :: RoleTokens -> PV3.TxOutRef -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
mkRoleTokensPolicy roleTokens txOutRef =
  let roleTokensHash = mkRoleTokensHash roleTokens
      errorOrApplied =
        $$(PlutusTx.compile [||\rs seed -> wrapMintingPolicy (mkPolicy rs seed)||])
          `PlutusTx.applyCode` PlutusTx.liftCodeDef roleTokensHash
          Haskell.>>= (`PlutusTx.applyCode` PlutusTx.liftCodeDef txOutRef)
   in case errorOrApplied of
        Haskell.Left err -> Haskell.error $ "Application of arguments to minting validator failed." Haskell.<> err
        Haskell.Right applied -> applied

mkRoleTokensPolicyHash :: RoleTokens -> PV3.TxOutRef -> ScriptHash
mkRoleTokensPolicyHash roleTokens1 txOutRef = hashScript PlutusV3 (mkRoleTokensPolicy roleTokens1 txOutRef)

mkRoleTokensPolicyBytes :: RoleTokens -> PV3.TxOutRef -> SerialisedScript
mkRoleTokensPolicyBytes roleTokens2 txOutRef = serialiseCompiledCode (mkRoleTokensPolicy roleTokens2 txOutRef)

{-# INLINEABLE openRolesValidator #-}
openRolesValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
openRolesValidator =
  $$(PlutusTx.compile [||openRolesValidator'||])
    `applyArg` marloweValidatorHash
 where
  openRolesValidator' :: ScriptHash -> BuiltinData -> BuiltinUnit
  openRolesValidator' marloweValidatorHash' ctx =
    check $ mkMarloweValidator marloweValidatorHash' (unsafeFromBuiltinData ctx)

openRolesValidatorHash :: ScriptHash
openRolesValidatorHash = hashScript PlutusV3 openRolesValidator

openRolesValidatorBytes :: SerialisedScript
openRolesValidatorBytes = serialiseCompiledCode openRolesValidator

-- This type of helper is missing because it requires a pretty heavy cardano-api dependency.
--
-- mkRoleTokensPolicyScript :: RoleTokens -> PV3.TxOutRef -> C.PlutusScript C.PlutusScriptV3
-- mkRoleTokensPolicyScript roleTokens3 txOutRef = do
--   let
--     bytes = mkRoleTokensPolicyBytes roleTokens3 txOutRef
--   PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes)


