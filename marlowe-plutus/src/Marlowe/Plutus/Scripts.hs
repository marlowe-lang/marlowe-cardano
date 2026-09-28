{- FOURMOLU_DISABLE -}

{-# LANGUAGE CPP #-}

-- Plinth options
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- Recommended extensions and flags
-- https://plutus.cardano.intersectmbo.org/docs/using-plinth/extensions-flags-pragmas
-- https://plutus.cardano.intersectmbo.org/docs/auction-smart-contract/on-chain-code#4-script-context
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

-- Custom warning suppression
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Marlowe semantics validator.
module Marlowe.Plutus.Scripts (
  -- * Validation
  mkRolePayoutValidator,
  mkMarloweValidator,
  mkThreadTokenName,
  MarloweInput,
  MarloweTxInput (..),
  MintingRedeemer (..),
) where

-- import Marlowe.Plutus.Contrib.PlutusTx.Debugging.Trace (
--   traceError,
--   traceIfFalse,
--   )
import PlutusTx.Prelude (
  traceError,
  traceIfFalse,
  )
import Marlowe.Plutus.Scripts.Types (MarloweInput, MarloweTxInput (..))
import Marlowe.Plutus.Semantics as Semantics (
  MarloweData (..),
    MarloweParams (MarloweParams, rolesCurrency),
  Payment (..),
  TransactionError (
    TEAmbiguousTimeIntervalError,
    TEApplyNoMatchError,
    TEHashMismatch,
    TEIntervalError,
    TEUselessTransaction
  ),
  TransactionInput (TransactionInput, txInputs, txInterval),
  TransactionOutput (
    Error,
    TransactionOutput,
    txOutContract,
    txOutPayments,
    txOutState
  ),
  computeTransaction,
  totalBalance,
  foldMapList,
 )
import Marlowe.Plutus.Semantics.Types as Semantics (
  ChoiceId (ChoiceId),
  Contract (Close),
  Input (..),
  InputContent (..),
  IntervalError (IntervalInPastError, InvalidInterval),
  Party (..),
  Payee (Account, Party),
  State (..),
  Token (Token),
  getInputContent, Accounts,
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  CurrencySymbol(..),
  Datum (Datum),
  DatumHash (DatumHash),
  Extended (..),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  POSIXTimeRange,
  ScriptContext (ScriptContext, scriptContextScriptInfo, scriptContextRedeemer, scriptContextTxInfo),
  ScriptHash (..),
  ScriptInfo (SpendingScript, MintingScript),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoInputs, txInfoOutputs, txInfoValidRange, txInfoMint),
  UnsafeFromData (..),
  UpperBound (..),
  ToData (toBuiltinData), BuiltinData, mintValueToMap, TxOutRef (TxOutRef), TxId (TxId), MintValue
 )
import PlutusLedgerApi.V3.Contexts (findDatum, findDatumHash, txSignedBy, valueSpent)
import PlutusLedgerApi.V2.Tx (OutputDatum (OutputDatumHash), TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))

import PlutusLedgerApi.V1.Address as Address (scriptHashAddress)
import PlutusTx.Prelude as PlutusTxPrelude (
  AdditiveGroup ((-)),
  AdditiveSemigroup ((+)),
  Bool (..),
  Eq (..),
  Functor (fmap),
  Integer,
  Maybe (..),
  Ordering(EQ, LT),
  Ord ((>), compare),
  otherwise,
  ($),
  (&&),
  (+),
  (.),
  (>=),
  (/=),
  (||), (<>), not, BuiltinByteString, integerToByteString, sha2_256, isJust, trace,
 )

import qualified PlutusTx.List as List

import qualified PlutusLedgerApi.V1.Value as Val
import qualified PlutusLedgerApi.V2 as Ledger (Address (Address))
import qualified PlutusTx.AssocMap as AssocMap
import Prelude qualified as H
import PlutusLedgerApi.V2 (Redeemer(Redeemer))
import PlutusTx.Builtins (equalsData, ByteOrder (BigEndian), mkB)
import qualified PlutusLedgerApi.V3 as P

{-# INLINEABLE closeInterval #-}
-- | Convert a Plutus POSIX time range into the closed interval needed by Marlowe semantics.
closeInterval :: POSIXTimeRange -> Maybe (POSIXTime, POSIXTime)
closeInterval (Interval (LowerBound (Finite (POSIXTime l)) lc) (UpperBound (Finite (POSIXTime h)) hc)) =
  Just
    ( POSIXTime $ if lc then l else l + 1 -- Add one millisecond if the interval was open.
    , POSIXTime $ if hc then h else h - 1-- Subtract one millisecond if the interval was open.
    )
closeInterval _ = Nothing

{-# INLINEABLE mkRolePayoutValidator #-}
-- | The Marlowe payout validator.
mkRolePayoutValidator
  :: ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
-- mkRolePayoutValidator ScriptContext{scriptContextTxInfo, scriptContextScriptInfo} =
--   let
--     (currency, role) = case scriptContextScriptInfo of
--       SpendingScript _ (H.Just (Datum d)) -> unsafeFromBuiltinData d
--       _ -> traceError "q"
--   in
--     -- The role token for the correct currency must be present.
--     -- [Marlowe-Cardano Specification: "17. Payment authorized".]
--     Val.singleton currency role 1 `Val.leq` valueSpent scriptContextTxInfo
-- 
mkRolePayoutValidator ScriptContext{scriptContextTxInfo, scriptContextScriptInfo} =
  case scriptContextScriptInfo of
    SpendingScript _ (H.Just (Datum d)) ->
      let (currency, role) = unsafeFromBuiltinData d
      in Val.singleton currency role 1 `Val.leq` valueSpent scriptContextTxInfo
    _ -> traceError "q"

type Seed = TxOutRef

data MintingRedeemer = Minting Seed | Burning

emptyBuiltinByteString :: BuiltinData
emptyBuiltinByteString = mkB ("" :: BuiltinByteString)

instance ToData MintingRedeemer where
  toBuiltinData (Minting seed) = toBuiltinData seed
  toBuiltinData Burning = emptyBuiltinByteString

instance UnsafeFromData MintingRedeemer where
  unsafeFromBuiltinData d = if equalsData d emptyBuiltinByteString
    then Burning
    else Minting (unsafeFromBuiltinData d)

-- In order to avoid validating other outputs values
-- for the thread token absence we count the number of
-- own outputs and use that to validate the minting value.
-- Please note that we reject own inputs which
-- is compatible with [Marlowe Cardano Specification: Constraint 4. No output to script on close].
type MarloweOutputsNumber = Integer

type MarloweOutputIndex = Integer

type SeedConsumed = Bool

type TxOutIdx = Integer

{-# INLINEABLE txOutIdxToByteString #-}
-- We use constant length output index encoding
-- to avoid any overlaps between parts of the
-- thread token preimage parts.
-- We restrict the output indices which can be
-- used as parts of the seed to be lower than 256.
-- It is rather rare to have such a large transactions
-- so it should not be a practical problem.
txOutIdxToByteString :: TxOutIdx -> BuiltinByteString
txOutIdxToByteString idx = do
  if idx >= 256
    then traceError "2"
    else integerToByteString BigEndian (2 :: Integer) idx

{-# INLINEABLE mkThreadTokenName #-}
mkThreadTokenName :: Seed -> MarloweOutputIndex -> Val.TokenName
mkThreadTokenName (TxOutRef { txOutRefId = TxId txId, txOutRefIdx }) = do
  let
    threadTokenPreimage = txId <> txOutIdxToByteString txOutRefIdx
  \marloweOutputIndex -> do
    let
      preimage = threadTokenPreimage <> txOutIdxToByteString marloweOutputIndex
    Val.TokenName . sha2_256 $ preimage

-- We optimize below for the small binary size by extracting helper functions.
-- That was our primary bottleneck which could block us from publishing the script.

{-# INLINEABLE countAll #-}
countAll :: (a -> Bool) -> [a] -> Maybe Integer
countAll pred = do
  let
    go acc [] = Just acc
    go acc (x:xs) | pred x = go (acc + 1) xs
    go _acc _xs = Nothing
  go 0

type MintTokenAmount = Integer

mintTokenAmount :: MintTokenAmount
mintTokenAmount = 1

burnTokenAmount :: MintTokenAmount
burnTokenAmount = -1

{-# INLINEABLE countMintedTokens #-}
countMintedTokens :: CurrencySymbol -> MintTokenAmount -> MintValue -> Maybe Integer
countMintedTokens ownCurrency expectedAmount = do
  let
    pred (_tokenName, amount) = amount == expectedAmount
  \mintValue -> do
    let
      mintMap = mintValueToMap mintValue
    case AssocMap.lookup ownCurrency mintMap of
      Just (AssocMap.toList -> mintedTokens) -> countAll pred mintedTokens
      Nothing -> Just 0

{-# INLINEABLE isOwnOutput #-}
isOwnOutput :: BuiltinByteString -> TxOut -> Bool
isOwnOutput ownHash TxOut{txOutAddress = Ledger.Address (ScriptCredential (ScriptHash vh)) _} | vh == ownHash = True
isOwnOutput _ownHash _txOut = False

{-# INLINEABLE isOwnInput #-}
isOwnInput :: BuiltinByteString -> TxInInfo -> Bool
isOwnInput ownHash = do
  let
    isOwnOutput' = isOwnOutput ownHash
  \TxInInfo{txInInfoResolved} -> isOwnOutput' txInInfoResolved

{-# INLINEABLE mkMarloweValidator #-}
-- | The Marlowe semantics validator.
mkMarloweValidator
  :: ScriptHash
  -- ^ The hash of the corresponding Marlowe payout validator.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated or minting was allowed.
mkMarloweValidator
  (ScriptHash rolePayoutValidatorHash)
  ScriptContext{scriptContextTxInfo, scriptContextRedeemer=Redeemer redeemer, scriptContextScriptInfo=MintingScript ownCurrency@(CurrencySymbol ownHash)} = case unsafeFromBuiltinData redeemer of
-- I. Minting:
--  1. Inputs:
--    1.1. There should be no input at the own hash address to avoid accounting for extra tokens and analysis of token interaction.
--    1.2. The seed input should be consumed.
--  2. Minting: The minting value should be equal to the number of outputs at the own hash address.
--  3. Outputs: Validate outputs at the own hash only.
--    3.1 All the datums are initialized with correct hash.
--    3.2 The accounts value should be covered by the output value and contain Marlowe thread token.
--    3.3 The accounts contain only positive values.
--    3.4 The accounts should be sorted and unique.
--    3.5 The roles currency in the Marlowe parameters should be equal to the role payout validator hash or empty.
    Minting seed -> do
      let
        TxInfo {txInfoOutputs, txInfoMint, txInfoInputs } = scriptContextTxInfo

        -- 1. Inputs validation
        inputsValid = do
          let
            isNotOwnInput = not . isOwnInput ownHash

            checkInputs :: SeedConsumed -> [TxInInfo] -> Bool
            checkInputs seedConsumed [] = traceIfFalse "1.2" seedConsumed
            checkInputs seedConsumed (txInInfo:remainingInputs) = do
              let
                TxInInfo{txInInfoOutRef} = txInInfo
                notOwnInput = traceIfFalse "1.1" (isNotOwnInput txInInfo)
                seedConsumed' = seedConsumed || txInInfoOutRef == seed
              notOwnInput
                && checkInputs seedConsumed' remainingInputs
          checkInputs False txInfoInputs

        -- 2. Minting validation:
        -- * We check that no more tokens are minted beside the thread tokens.
        -- * This validation implies outputs were already validated.
        ownMintsNumber = countMintedTokens ownCurrency mintTokenAmount txInfoMint

        -- 3. Outputs validation: we check that every output contains *at least* thread token
        -- Create singleton value with a unique thread token name
        mkThreadTokenValue :: MarloweOutputIndex -> Val.Value
        mkThreadTokenValue = do
          let
            mkThreadTokenName' = mkThreadTokenName seed
          \marloweOutputIndex -> do
            let
              threadTokenName = mkThreadTokenName' marloweOutputIndex
            Val.singleton ownCurrency threadTokenName 1

        isValueValid :: MarloweOutputIndex -> Val.Value -> Accounts -> Bool
        isValueValid marloweOutputIndex outValue outAccounts = do
          let
            threadTokenValue = mkThreadTokenValue marloweOutputIndex
            -- We check that the output contains **at least** one thread token.
            -- In theory it could contain more own currency tokens but the minting
            -- level check will only allow a one token per output in total.
            expectedValue = totalBalance outAccounts <> threadTokenValue
          traceIfFalse "3.2" (expectedValue == outValue)

        -- The actual validation of the outputs at the own hash address.
        checkAndCountMarloweOutputs :: MarloweOutputsNumber -> [TxOut] -> Maybe MarloweOutputsNumber
        checkAndCountMarloweOutputs marloweOutputsNumberSoFar [] = Just marloweOutputsNumberSoFar
        checkAndCountMarloweOutputs marloweOutputsNumberSoFar (TxOut{txOutAddress, txOutValue, txOutDatum }:remainingTxOuts) = do
          let
            isDatumValid :: BuiltinData -> MarloweParams -> POSIXTime -> Accounts -> Contract -> Bool
            isDatumValid outDatum outMarloweParams@(MarloweParams (CurrencySymbol outRoleCurrencyHash)) outMinTime outAccounts outContract = do
              let
                isAccountKeySmaller firstAccountKey secondAccountKey = do
                  let
                    cmpAccountKey (partyA, tokenA) (partyB, tokenB) = case compare partyA partyB of
                      EQ -> compare tokenA tokenB
                      ordering -> ordering
                  cmpAccountKey firstAccountKey secondAccountKey == LT

                areAccountsValid [] = True
                areAccountsValid ((firstAccountKey, firstAmount): remaining) = do
                  let
                    go _prevAccountKey [] = True
                    go prevAccountKey ((accountKey, amount):remaining) = do
                      let
                        isSorted = isAccountKeySmaller prevAccountKey accountKey
                        isPositive = amount > 0
                      traceIfFalse "3.3" isPositive
                        && traceIfFalse "3.4" isSorted
                        && go accountKey remaining

                  traceIfFalse "3.3" (firstAmount > 0)
                    && go firstAccountKey remaining

                accountsValid = areAccountsValid (AssocMap.toList outAccounts)

                expectedState = State
                  { accounts = outAccounts
                  , choices = AssocMap.empty
                  , boundValues = AssocMap.empty
                  , minTime = outMinTime
                  }
                expectedDatum = toBuiltinData $ MarloweData
                  { marloweParams = outMarloweParams
                  , marloweState = expectedState
                  , marloweContract = outContract
                  }
                datumContentValid = traceIfFalse "3.1" (equalsData expectedDatum outDatum)
                outRoleCurrencyValid = traceIfFalse "3.5" (outRoleCurrencyHash == "" || outRoleCurrencyHash == rolePayoutValidatorHash)
              datumContentValid
                && accountsValid
                && outRoleCurrencyValid

          case txOutAddress of
            (P.Address (ScriptCredential (ScriptHash vh)) _) | vh == ownHash -> do
              case txOutDatum of
                OutputDatumHash svh -> case findDatum svh scriptContextTxInfo of
                  Just (Datum serialisedDatum) -> do
                    let
                      MarloweData { marloweParams, marloweState = State { accounts, minTime }, marloweContract } = unsafeFromBuiltinData serialisedDatum
                    if isDatumValid serialisedDatum marloweParams minTime accounts marloweContract
                      && isValueValid marloweOutputsNumberSoFar txOutValue accounts
                      then checkAndCountMarloweOutputs (marloweOutputsNumberSoFar + 1) remainingTxOuts
                      else Nothing
                  _ -> trace "3.0" Nothing
                _ -> trace "3.0" Nothing
            _ -> checkAndCountMarloweOutputs marloweOutputsNumberSoFar remainingTxOuts

      inputsValid
          && case (checkAndCountMarloweOutputs 0 txInfoOutputs, ownMintsNumber) of
            (Just ownOutputsNumber, Just ownMintsNumber') -> traceIfFalse "2" (ownMintsNumber' == ownOutputsNumber)
            _ -> False
-- II. Burning:
--  1. Inputs: We allow any outputs and we just count the number of inputs at the own hash address.
--  2. Minting:
--    2.1 All the tokens in the own minting info should be burned (quantity = -1).
--    2.2 Absolute tokens amount which are burned should be equal to the number of all own inputs.
--  3. Outputs: There should be no outputs at the own hash address.
    Burning -> do
      let
        TxInfo {txInfoMint, txInfoInputs, txInfoOutputs} = scriptContextTxInfo
        ownInputsNumber :: Integer
        ownInputsNumber = do
          let
            isOwnInput' = isOwnInput ownHash

            step txInInfo acc | isOwnInput' txInInfo = acc + 1
            step _ acc = acc
          List.foldr step 0 txInfoInputs

        noOutputsAtOwnAddress = do
          traceIfFalse "3" (not $ List.any (isOwnOutput ownHash) txInfoOutputs)

        ownBurnsNumber = countMintedTokens ownCurrency burnTokenAmount txInfoMint

      noOutputsAtOwnAddress && case ownBurnsNumber of
        Just ownBurnsNumber' -> traceIfFalse "2.2" (ownBurnsNumber' == ownInputsNumber)
        Nothing -> False

-- III. Spending: Spending spec is defined separately.
mkMarloweValidator
  rolePayoutValidatorHash
  ScriptContext{scriptContextTxInfo, scriptContextRedeemer=Redeemer redeemer, scriptContextScriptInfo=SpendingScript txOutRef datum} = do
    let marloweTxInputs = unsafeFromBuiltinData redeemer
    let TxInfo { txInfoMint } = scriptContextTxInfo
    let interval =
          -- Marlowe semantics require a closed interval, so we might adjust by one millisecond.
          case closeInterval $ txInfoValidRange scriptContextTxInfo of
            Just interval' -> interval'
            Nothing -> traceError "a"

    -- Find Contract continuation in TxInfo datums by hash or fail with error.
    let inputs = fmap marloweTxInputToInput marloweTxInputs

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.  This gives huge flexibility by allowing
        parties to provide multiple inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputContents = fmap getInputContent inputs

    -- Check that the required signatures and role tokens are present.
    -- [Marlowe-Cardano Specification: "Constraint 14. Inputs authorized".]
    let inputsOk = allInputsAreAuthorized inputContents

    -- We decided to drop this redundancy:
    -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
    -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
    -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
    -- [Marlowe-Cardano Specification: "Constraint 0. Input to semantics".]
    -- Package the inputs to be applied in the semantics.
    let txInput =
          TransactionInput
            { txInterval = interval
            , txInputs = inputs
            }

    -- [Marlowe-Cardano Specification: "Constraint 7. Input state".]
    -- [Marlowe-Cardano Specification: "Constraint 8. Input contract".]
    -- The semantics computation operates on the state and contract from
    -- the incoming datum.
    let ownOutputs = List.filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) allOutputs

    case computeTransaction txInput marloweState marloweContract of
      TransactionOutput
        { txOutPayments = computedTxOutPayments
        , txOutState = computedTxOutState
        , txOutContract = computedTxOutContract
        } -> do
        -- [Marlowe-Cardano Specification: "Constraint 9. Marlowe parameters".]
        -- [Marlowe-Cardano Specification: "Constraint 10. Output state".]
        -- [Marlowe-Cardano Specification: "Constraint 11. Output contract."]
        -- The output datum maintains the parameters and uses the state
        -- and contract resulting from the semantics computation.
        let computedMarloweDatum =
              MarloweData
                { marloweParams = marloweParams
                , marloweContract = computedTxOutContract
                , marloweState = computedTxOutState
                }

            -- Each party must receive as least as much value as the semantics specify.
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient payment."]
            payoutsByParty = AssocMap.toList $ foldMapList payoutByParty computedTxOutPayments
            payoutsOk = payoutConstraints payoutsByParty

            continuationOk =
              case computedTxOutContract of
                -- [Marlowe-Cardano Specification: "Constraint 4. No output to script on close".]
                Close -> do
                  let
                    -- Check for any output to the script address.
                    hasNoOutputToOwnScript :: Bool
                    hasNoOutputToOwnScript = case ownOutputs of
                      [] -> True
                      _ -> False

                    -- Delegate thread token management to the minting policy
                    ownMintingExecuted :: Bool
                    ownMintingExecuted = isJust . AssocMap.lookup (CurrencySymbol ownHash) . mintValueToMap $ txInfoMint

                  traceIfFalse "c" hasNoOutputToOwnScript
                    && traceIfFalse "e" ownMintingExecuted
                _ -> do
                  -- [Marlowe-Cardano Specification: "Constraint 3. Single Marlowe output".]
                  -- [Marlowe-Cardano Specification: "Constraint 6. Output value to script."]
                  -- Check that the single Marlowe output has the correct datum and value.
                  -- NOTE: We consider all the other checks which verify the correctness of the interpreter
                  -- result to be redundant. If that would be the case then it is a critical issue
                  -- and we do not have any real mitigation to it.
                  -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
                  -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
                  -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
                  let
                    -- We hash the datum through searching for the exact expected value which should be cheaper
                    -- than serializing and hashing the datum.
                    expectedDatumHash = findDatumHash' computedMarloweDatum

                    TxOut{txOutValue = continuingValue, txOutDatum = continuingDatum } = case ownOutputs of
                      [out] -> out
                      _ -> traceError "o" -- No continuation or multiple Marlowe contract outputs is forbidden.

                    continuingDatumHash = case continuingDatum of
                      OutputDatumHash h -> h
                      _ -> traceError "o" -- The output datum must be a hash.

                    -- TODO: Measure if we should rather filter out own currency token from the continuing value.
                    -- We could do the above without actually checking the token name under the assumption of a single
                    -- own input.
                    -- TODO: Test BulitinValue.
                    --
                    -- Construct thread token singleton value by fetching the token name info from the own input.
                    threadTokenValue = do
                      let
                        ownCurrencySymbol = CurrencySymbol ownHash
                        go [] = traceError "2"
                        go ((currCurrencySymbol, tokensMap):_) | currCurrencySymbol == ownCurrencySymbol = case AssocMap.toList tokensMap of
                          [(tokenName, amount)] | amount == 1 -> Val.singleton ownCurrencySymbol tokenName 1
                          _ -> traceError "2"
                        go (_:remainingValues) = go remainingValues
                      go $ AssocMap.toList (Val.getValue scriptInValue)


                    -- The final value of the output should be equal to the sum of the Marlowe accounts plus the thread token.
                    expectedValue = totalBalance computedMarloweDatum.marloweState.accounts + threadTokenValue

                  traceIfFalse "d" $
                    Just continuingDatumHash == expectedDatumHash
                      && continuingValue == expectedValue

        inputsOk
          && payoutsOk
          && continuationOk
          -- [Marlowe-Cardano Specification: "20. Single satisfaction".]
          -- Either there must be no payouts, or there must be no other validators.
          && traceIfFalse "z" (List.null payoutsByParty || noOthers)
      Error TEAmbiguousTimeIntervalError -> traceError "i"
      Error TEApplyNoMatchError -> traceError "n"
      Error (TEIntervalError (InvalidInterval _)) -> traceError "j"
      Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "k"
      Error TEUselessTransaction -> traceError "u"
      Error TEHashMismatch -> traceError "m"
    where
      TxInfo{txInfoInputs, txInfoOutputs} = scriptContextTxInfo

      MarloweData {..} = case datum of
        H.Just (Datum d) -> unsafeFromBuiltinData d
        _ -> traceError "f"

      -- The roles currency is in the Marlowe parameters.
      MarloweParams{rolesCurrency} = marloweParams

      TxInInfo{txInInfoResolved = TxOut{txOutAddress = ownAddress, txOutValue = scriptInValue}} = case List.find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs of
        Just self -> self
        _ -> traceError "x" -- Input to be validated was not found.

      ownHash = case ownAddress of
        (P.Address (ScriptCredential (ScriptHash ownHash)) _) -> ownHash
        _ -> traceError "x"

      -- [Marlowe-Cardano Specification: "2. Single Marlowe script input".]
      -- The inputs being spent by this script, and whether other validators are present.
      noOthers :: Bool
      noOthers = do
        let
          isOtherScriptInput TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential (ScriptHash vh)) _}} = vh /= ownHash
          isOtherScriptInput _ = False
          othersFound = List.any isOtherScriptInput txInfoInputs
        not othersFound

      -- Look up the Datum hash for specific data.
      findDatumHash' :: (ToData o) => o -> Maybe DatumHash
      findDatumHash' datum = findDatumHash (Datum $ toBuiltinData datum) scriptContextTxInfo

      -- Check that the correct datum and value is being output to the script.
      -- All of the script outputs.
      allOutputs :: [TxOut]
      allOutputs = txInfoOutputs

      -- Check merkleization and transform transaction input to semantics input.
      marloweTxInputToInput :: MarloweTxInput -> Input
      marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
          Just (Datum d) ->
            let continuation = unsafeFromBuiltinData d
             in MerkleizedInput input hash continuation
          Nothing -> traceError "h"
      marloweTxInputToInput (Input input) = NormalInput input

      -- Check that inputs are authorized.
      allInputsAreAuthorized :: [InputContent] -> Bool
      allInputsAreAuthorized = List.all validateInputWitness
        where
          totalSpent = valueSpent scriptContextTxInfo

          validateInputWitness :: InputContent -> Bool
          validateInputWitness input =
            case input of
              IDeposit _ party _ _ -> validatePartyWitness party -- The party must witness a deposit.
              IChoice (ChoiceId _ party) _ -> validatePartyWitness party -- The party must witness a choice.
              INotify -> True -- No witness is needed for a notify.
            where
              validatePartyWitness :: Party -> Bool
              validatePartyWitness (Address _ address) = traceIfFalse "s" $ txSignedByAddress address -- The key must have signed.
              validatePartyWitness (Role role) =
                -- FIXME: Optimize this expensive folding and check away by collecting own spent tokens names
                traceIfFalse "t"
                  $ Val.singleton rolesCurrency role 1 -- The role token must be present.
                  `Val.leq` totalSpent

      -- Extract the payout to a party.
      payoutByParty :: Payment -> AssocMap.Map Party Val.Value
      payoutByParty (Payment _ (Party party) (Token cur tok) amount)
        | amount > 0 = AssocMap.singleton party $ Val.singleton cur tok amount
        | otherwise = AssocMap.empty -- NOTE: Perhaps required because semantics may make zero payments
        -- (though this passes the test suite), but removing this function's
        -- guard reduces the validator size by 20 bytes.
      payoutByParty (Payment _ (Account _) _ _) = AssocMap.empty

      -- -- Check outgoing payments.
      payoutConstraints :: [(Party, Val.Value)] -> Bool
      payoutConstraints = List.all payoutToTxOut
        where
          payoutToTxOut :: (Party, Val.Value) -> Bool
          payoutToTxOut (party, value) = case party of
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient Payment".]
            -- SCP-5128: Note that the payment to an address may be split into several outputs but the payment to a role must be
            -- a single output. The flexibility of multiple outputs accommodates wallet-related practicalities such as the change and
            -- the return of the role token being in separate UTxOs in situations where a contract is also paying to the address
            -- where that change and that role token are sent.
            Address _ address -> traceIfFalse "p" $ valuePaidToAddress address `geqSingleton` value -- At least sufficient value paid.
            Role role -> do
              let hsh = findDatumHash' (rolesCurrency, role)
                  addr = Address.scriptHashAddress rolePayoutValidatorHash

              -- Some output must have the correct value and datum to the role-payout address.
              traceIfFalse "r" $ List.any (checkScriptOutput geqSingleton addr hsh value) allOutputs

      -- Check that address, value, and datum match the specified.
      checkScriptOutput :: (Val.Value -> Val.Value -> Bool) -> Ledger.Address -> Maybe DatumHash -> Val.Value -> TxOut -> Bool
      checkScriptOutput comparison addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum = OutputDatumHash svh} =
        txOutValue `comparison` value && hsh == Just svh && txOutAddress == addr
      checkScriptOutput _ _ _ _ _ = False

      -- Check that a multiasset value is not less than another value, optimized for singletons.
      -- Note that the semantics of this function differ from `geq` in cases where the first
      -- argument contains negative quantities; however, this never occurs in values from UTxOs.
      geqSingleton :: Val.Value -> Val.Value -> Bool
      geqSingleton multi single =
        case Val.flattenValue single of
          [(c, t, a)] -> Val.valueOf multi c t >= a  -- The second value was a singleton.
          _ -> multi `Val.geq` single                -- The second value wasn't a singleton.

      -- The key for the address must have signed.
      txSignedByAddress :: Ledger.Address -> Bool
      txSignedByAddress (Ledger.Address (PubKeyCredential pkh) _) = scriptContextTxInfo `txSignedBy` pkh
      txSignedByAddress _ = False

      -- Tally the value paid to an address.
      valuePaidToAddress :: Ledger.Address -> Val.Value
      valuePaidToAddress address = foldMapList txOutValue $ List.filter ((== address) . txOutAddress) allOutputs
mkMarloweValidator
  _rolePayoutValidatorHash
  _ = traceError "g" -- The script context must be a spending or minting script.
