{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Ongoing migration towards "Parse don't validate" or rather "Validate don't check" in our nomenclature ;-)
-- | We want to finally enhance checking functions from `Safety.Ledger` so they build some `SafeReport` instead of
-- | just checking and returning `[SafetyError]`.
module Language.Marlowe.Analysis.Safety.Ledger.Validators (
  -- * Types
  SafetyValidator,
  SafeAddress (SafeAddress),
  SafeCurrencySymbol (SafeCurrencySymbol),
  SafeRoleName (SafeRoleName),
  SafeRolesCurrency (RolesCurrency, NoRolesCurrency),
  SafeToken (SafeToken),
  SafeTokenName (SafeTokenName),
  SafeRoleUsage (SafeRolesUsage, SafeNoRoleUsage),
  SafetyValidatorContext (SafetyValidatorContext, svcMarloweData, svcContinuations, svcWarnUnusedRolesCurrency),

  -- * Validators
  addressValidator,
  currencySymbolValidator,
  roleNameValidator,
  roleNamesValidator,
  rolesCurrencyValidator,
  tokenNameValidator,
  tokenValidator,
) where

import Control.Category ((>>>))
import Data.Either.Validation (Validation (..))
import Data.Functor ((<&>))
import qualified Data.Set as S
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..))
import Language.Marlowe.Analysis.Safety.Types.Validator (
  Validator,
  fromFn,
  fromFnEither,
  fromFnValidation,
  runValidator,
  validatorTraverse,
 )
import qualified Language.Marlowe.Analysis.Safety.Types.Validator as Validator
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Plate (extractRoleNames)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32, serialiseAddressBech32)
import qualified PlutusLedgerApi.V1.Value as Val
import PlutusLedgerApi.V2 (
  CurrencySymbol (..),
  TokenName (..),
 )
import qualified PlutusLedgerApi.V2 as P (Address (..))
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import qualified PlutusTx.Prelude as P (lengthOfByteString)

-- | We slowly migrate towards "Parse don't validate" approach instead of check list.
type SafetyValidator i o = Validator [SafetyError] i o

newtype SafeAddress = SafeAddress_ P.Address

{-# COMPLETE SafeAddress #-}
pattern SafeAddress :: P.Address -> SafeAddress
pattern SafeAddress address <- SafeAddress_ address

addressValidator :: SafetyValidator P.Address SafeAddress
addressValidator = Validator.fromFnEither \address -> case deserialiseAddressBech32 $ serialiseAddressBech32 False address of
  Nothing -> Left [IllegalAddress address]
  Just _ -> pure $ SafeAddress_ address

newtype SafeCurrencySymbol = SafeCurrencySymbol_ CurrencySymbol

{-# COMPLETE SafeCurrencySymbol #-}
pattern SafeCurrencySymbol :: CurrencySymbol -> SafeCurrencySymbol
pattern SafeCurrencySymbol currencySymbol <- SafeCurrencySymbol_ currencySymbol

currencySymbolValidator :: SafetyValidator CurrencySymbol SafeCurrencySymbol
currencySymbolValidator = Validator.fromFnEither \currencySymbol -> case unCurrencySymbol currencySymbol of
  "" -> pure . SafeCurrencySymbol_ . Val.CurrencySymbol . stringToBuiltinByteStringHex $ ""
  byteString | P.lengthOfByteString byteString /= 28 -> Left [InvalidCurrencySymbol currencySymbol]
  _ -> pure $ SafeCurrencySymbol_ currencySymbol

data SafeRolesCurrency
  = RolesCurrency_ SafeCurrencySymbol
  | NoRolesCurrency_

{-# COMPLETE RolesCurrency, NoRolesCurrency #-}
pattern RolesCurrency :: CurrencySymbol -> SafeRolesCurrency
pattern RolesCurrency currencySymbol <- RolesCurrency_ (SafeCurrencySymbol currencySymbol)

pattern NoRolesCurrency :: SafeRolesCurrency
pattern NoRolesCurrency <- NoRolesCurrency_

rolesCurrencyValidator :: SafetyValidator CurrencySymbol SafeRolesCurrency
rolesCurrencyValidator =
  currencySymbolValidator >>> Validator.fromFn \c@(SafeCurrencySymbol (CurrencySymbol{..})) -> case unCurrencySymbol of
    "" -> NoRolesCurrency_
    _ -> RolesCurrency_ c

newtype SafeTokenName = SafeTokenName_ TokenName

{-# COMPLETE SafeTokenName #-}
pattern SafeTokenName :: TokenName -> SafeTokenName
pattern SafeTokenName tokenName <- SafeTokenName_ tokenName

tokenNameValidator :: SafetyValidator TokenName SafeTokenName
tokenNameValidator = Validator.fromFnEither \tokenName -> case unTokenName tokenName of
  byteString | P.lengthOfByteString byteString > 32 -> Left [TokenNameTooLong tokenName]
  _ -> pure $ SafeTokenName_ tokenName

newtype SafeRoleName = SafeRoleName_ SafeTokenName

{-# COMPLETE SafeRoleName #-}
pattern SafeRoleName :: TokenName -> SafeRoleName
pattern SafeRoleName tokenName <- SafeRoleName_ (SafeTokenName tokenName)

roleNameValidator :: SafetyValidator TokenName SafeRoleName
roleNameValidator =
  fromFnValidation $
    runValidator tokenNameValidator >>> \case
      Failure errs ->
        Failure $
          errs <&> \case
            TokenNameTooLong tokenName -> RoleNameTooLong tokenName
            err -> err
      Success safeTokenName -> Success $ SafeRoleName_ safeTokenName

data SafeToken = SafeToken_ SafeTokenName SafeCurrencySymbol

{-# COMPLETE SafeToken #-}
pattern SafeToken :: TokenName -> CurrencySymbol -> SafeToken
pattern SafeToken tokenName currencySymbol <- SafeToken_ (SafeTokenName tokenName) (SafeCurrencySymbol currencySymbol)

tokenValidator :: SafetyValidator (CurrencySymbol, TokenName) SafeToken
tokenValidator =
  SafeToken_
    <$> (fromFn snd >>> tokenNameValidator)
    <*> (fromFn fst >>> currencySymbolValidator)

data SafetyValidatorContext = SafetyValidatorContext
  { svcMarloweData :: V1.MarloweData
  , svcContinuations :: Continuations
  , svcWarnUnusedRolesCurrency :: Bool
  }

data SafeRoleUsage
  = SafeRolesUsage_ SafeCurrencySymbol [SafeRoleName]
  | SafeNoRoleUsage_ (Maybe SafeCurrencySymbol)

{-# COMPLETE SafeRolesUsage, SafeNoRoleUsage #-}
pattern SafeRolesUsage :: SafeCurrencySymbol -> [SafeRoleName] -> SafeRoleUsage
pattern SafeRolesUsage currencySymbol tokenNames <- SafeRolesUsage_ currencySymbol tokenNames

pattern SafeNoRoleUsage :: Maybe SafeCurrencySymbol -> SafeRoleUsage
pattern SafeNoRoleUsage currencySymbol <- SafeNoRoleUsage_ currencySymbol

roleNamesValidator :: SafetyValidator SafetyValidatorContext SafeRoleUsage
roleNamesValidator = do
  let extractRolesCurrency = fromFn \case
        SafetyValidatorContext{svcMarloweData = V1.MarloweData{marloweParams = V1.MarloweParams{rolesCurrency}}} -> rolesCurrency
      extractRoles = fromFn \case
        SafetyValidatorContext{svcMarloweData = V1.MarloweData{marloweState, marloweContract}, svcContinuations} ->
          S.toList $ extractRoleNames marloweState marloweContract svcContinuations
      extractWarnFlag = fromFn \case
        SafetyValidatorContext{svcWarnUnusedRolesCurrency} -> svcWarnUnusedRolesCurrency
  ( (,,)
      <$> (extractRolesCurrency >>> rolesCurrencyValidator)
      <*> (extractRoles >>> validatorTraverse roleNameValidator)
      <*> extractWarnFlag
    )
    >>> fromFnEither \case
      (NoRolesCurrency_, _ : _, _) -> Left [MissingRolesCurrency]
      (NoRolesCurrency_, [], _) -> pure $ SafeNoRoleUsage_ Nothing
      (RolesCurrency_ rolesCurrency, rs@(_ : _), _) -> pure $ SafeRolesUsage_ rolesCurrency rs
      (RolesCurrency_ _, [], warn) | warn -> Left [ContractHasNoRoles]
      (RolesCurrency_ rolesCurrency, [], _) -> pure $ SafeNoRoleUsage_ $ Just rolesCurrency
