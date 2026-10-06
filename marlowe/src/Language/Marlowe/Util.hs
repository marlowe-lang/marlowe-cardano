{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Util (
  addAccountsDiff,
  emptyAccountsDiff,
  extractNonMerkleizedContractRoles,
  foldMapContract,
  foldMapNonMerkleizedContract,
  getAccountsDiff,
  isEmptyAccountsDiff,
  merkleizedCase,
  dataHash,
) where

import Cardano.Api (SerialiseAsRawBytes (..), hashScriptDataBytes, unsafeHashableScriptData)
import Cardano.Api.Plutus (fromPlutusData)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import qualified PlutusLedgerApi.V1.Value as Val
import PlutusLedgerApi.V2 (ToData, toBuiltin, toData)
import qualified PlutusTx
import qualified PlutusTx.Prelude as P

instance IsString Party where
  fromString s = case deserialiseAddressBech32 $ T.pack s of
    Just (network, address) -> Address network address
    Nothing -> Role . Val.TokenName . toBuiltin $ TE.encodeUtf8 . T.pack $ s

type AccountsDiff = Map Party Money

emptyAccountsDiff :: AccountsDiff
emptyAccountsDiff = Map.empty

isEmptyAccountsDiff :: AccountsDiff -> Bool
isEmptyAccountsDiff = all Val.isZero

-- Adds a value to the map of outcomes
addAccountsDiff :: Party -> Money -> AccountsDiff -> AccountsDiff
addAccountsDiff party diffValue trOut =
  let newValue = case Map.lookup party trOut of
        Just value -> value P.+ diffValue
        Nothing -> diffValue
   in Map.insert party newValue trOut

-- | Extract total outcomes from transaction inputs and outputs
getAccountsDiff :: [Payment] -> [Input] -> AccountsDiff
getAccountsDiff payments inputs =
  foldl' (\acc (p, m) -> addAccountsDiff p m acc) emptyAccountsDiff (incomes ++ outcomes)
  where
    incomes = [(p, Val.singleton cur tok m) | IDeposit _ p (Token cur tok) m <- map getInputContent inputs]
    outcomes = [(p, P.negate $ paymentMoney payment) | payment@(Payment _ (Party p) _ _) <- payments]

foldMapContract
  :: (Monoid m)
  => (P.BuiltinByteString -> Maybe Contract)
  -> (Contract -> m)
  -> (Case Contract -> m)
  -> (Observation -> m)
  -> (Value Observation -> m)
  -> Contract
  -> m
foldMapContract funmerk fcont fcase fobs fvalue contract =
  fcont contract <> case contract of
    Close -> mempty
    Pay _ _ _ value cont -> fvalue' value <> go cont
    If obs cont1 cont2 -> fobs' obs <> go cont1 <> go cont2
    When cases _ cont -> foldMap fcase' cases <> go cont
    Let _ value cont -> fvalue value <> go cont
    Assert obs cont -> fobs' obs <> go cont
  where
    go = foldMapContract funmerk fcont fcase fobs fvalue
    fcase' cs =
      fcase cs <> case cs of
        Case _ cont -> go cont
        MerkleizedCase _ chash -> maybe mempty go (funmerk chash)
    fobs' obs =
      fobs obs <> case obs of
        AndObs a b -> fobs' a <> fobs' b
        OrObs a b -> fobs' a <> fobs' b
        NotObs a -> fobs' a
        ValueGE a b -> fvalue' a <> fvalue' b
        ValueGT a b -> fvalue' a <> fvalue' b
        ValueLT a b -> fvalue' a <> fvalue' b
        ValueLE a b -> fvalue' a <> fvalue' b
        ValueEQ a b -> fvalue' a <> fvalue' b
        _ -> mempty
    fvalue' v =
      fvalue v <> case v of
        NegValue val -> fvalue' val
        AddValue a b -> fvalue' a <> fvalue' b
        SubValue a b -> fvalue' a <> fvalue' b
        MulValue a b -> fvalue' a <> fvalue' b
        DivValue a b -> fvalue' a <> fvalue' b
        Cond obs a b -> fobs' obs <> fvalue' a <> fvalue' b
        _ -> mempty

foldMapNonMerkleizedContract
  :: (Monoid m)
  => (Contract -> m)
  -> (Case Contract -> m)
  -> (Observation -> m)
  -> (Value Observation -> m)
  -> Contract
  -> m
foldMapNonMerkleizedContract = foldMapContract (const Nothing)

extractNonMerkleizedContractRoles :: Contract -> Set Val.TokenName
extractNonMerkleizedContractRoles = foldMapNonMerkleizedContract extract extractCase (const mempty) (const mempty)
  where
    extract (Pay from payee _ _ _) = fromParty from <> fromPayee payee
    extract _ = mempty

    extractCase (Case (Deposit acc party _ _) _) = fromParty acc <> fromParty party
    extractCase (Case (Choice (ChoiceId _ party) _) _) = fromParty party
    extractCase _ = mempty

    fromParty (Role name) = Set.singleton name
    fromParty _ = mempty

    fromPayee (Party party) = fromParty party
    fromPayee (Account party) = fromParty party

merkleizedCase :: Action -> Contract -> Case Contract
merkleizedCase action continuation =
  let hash = dataHash (PlutusTx.toBuiltinData continuation)
   in MerkleizedCase action hash

dataHash :: (ToData a) => a -> P.BuiltinByteString
dataHash = toBuiltin . serialiseToRawBytes . hashScriptDataBytes . unsafeHashableScriptData . fromPlutusData . toData
