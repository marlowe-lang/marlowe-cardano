-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary and semi-arbitrary instances for the Marlowe DSL.
module Spec.Marlowe.Semantics.Arbitrary (
  -- * Types
  Context,
  IsValid (..),
  SemiArbitrary (..),
  ValidContractStructure (..),

  -- * Generators
  arbitraryAssocMap,
  arbitraryChoiceName,
  arbitraryContractWeighted,
  arbitraryFibonacci,
  arbitraryGoldenTransaction,
  arbitraryNonnegativeInteger,
  arbitraryPositiveInteger,
  arbitraryTimeIntervalAround,
  arbitraryValidInput,
  arbitraryValidInputs,
  arbitraryValidStep,
  choiceInBoundsIfNonempty,
  choiceNotInBounds,
  goldenContract,
  interestingInput,

  -- * Weighting factors for arbitrary contracts
  assertContractWeights,
  closeContractWeights,
  defaultContractWeights,
  ifContractWeights,
  letContractWeights,
  payContractWeights,
  whenContractWeights,
) where

import Control.Monad (replicateM)
import Data.ByteString (pack)
import Data.Function (on)
import Data.List (nub, nubBy)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..), Transaction (..))
import Language.Marlowe.Core.V1.Merkle (MerkleizedContract (..), merkleizeInputs, shallowMerkleize)
import Language.Marlowe.Core.V1.Semantics (
  Payment (..),
  TransactionError (..),
  TransactionInput (..),
  TransactionOutput (..),
  TransactionWarning (..),
  computeTransaction,
  evalObservation,
  evalValue,
 )
import Language.Marlowe.Core.V1.Semantics.Types (
  Accounts,
  Action (..),
  Bound (..),
  Case (..),
  ChoiceId (..),
  ChoiceName,
  ChosenNum,
  Contract (..),
  Environment (..),
  Input (..),
  InputContent (..),
  IntervalError (..),
  Observation (..),
  Party (..),
  Payee (..),
  State (..),
  TimeInterval,
  Token (..),
  Value (..),
  ValueId (..),
  getAction,
  mkTokenNameByteString,
  unsafeMkCurrencySymbolHex,
 )
import Language.Marlowe.Core.V1.Semantics.Types.Address (testnet)
import PlutusLedgerApi.V2 (
  Credential (..),
  CurrencySymbol (..),
  DatumHash (..),
  ExBudget (..),
  POSIXTime (..),
  PubKeyHash (..),
  ScriptHash (..),
  StakingCredential (StakingHash),
  TokenName (..),
  adaSymbol,
  adaToken,
  toBuiltin,
 )
import PlutusTx.Builtins (BuiltinByteString, appendByteString, lengthOfByteString, sliceByteString)
import Spec.Marlowe.Semantics.Golden (GoldenTransaction, goldenContracts, goldenTransactions)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  chooseInt,
  chooseInteger,
  elements,
  frequency,
  listOf,
  oneof,
  resize,
  shrinkList,
  sized,
  suchThat,
  vectorOf,
 )

import Data.Functor ((<&>))

import Control.Monad.Writer (runWriter)
import qualified Data.Aeson as A
import Language.Marlowe.Util (dataHash)
import qualified PlutusLedgerApi.V2 as Ledger (Address (..))
import qualified PlutusTx.AssocMap as AM (Map, delete, empty, keys, toList, unsafeFromList)
import qualified PlutusTx.Eq as P (Eq)

-- | Part of the Fibonacci sequence.
fibonaccis :: (Num a) => [a]
fibonaccis = [2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584]

-- | Inverse-Fibonacci frequencies.
fibonacciFrequencies :: (Integral a) => [a]
fibonacciFrequencies = (1000000 `div`) <$> fibonaccis

-- | Select an element of a list with probability proportional to inverse-Fibonacci weights.
arbitraryFibonacci :: [a] -> Gen a
arbitraryFibonacci = frequency . zip fibonacciFrequencies . fmap pure

-- | Shrink a byte string.
shrinkByteString :: (a -> BuiltinByteString) -> [a] -> a -> [a]
shrinkByteString f universe selected =
  filter
    (\candidate -> lengthOfByteString (f candidate) > 0 && lengthOfByteString (f candidate) < lengthOfByteString (f selected))
    universe

-- | Select an element of a list with high probability, or create a non-element at random with low probability.
perturb
  :: Gen a
  -- ^ The generator for a random item.
  -> [a]
  -- ^ The list of pre-defined items.
  -> Gen a
  -- ^ Generator for an item
perturb gen [] = gen
perturb gen xs = frequency [(20, gen), (80, elements xs)]

-- | Class for arbitrary values with respect to a context.
class (Arbitrary a) => SemiArbitrary a where
  -- | Generate an arbitrary value within a context.
  semiArbitrary :: Context -> Gen a
  semiArbitrary context =
    case fromContext context of
      [] -> arbitrary
      xs -> perturb arbitrary xs

  -- | Shrink a contextually arbitrary value.
  semiShrink :: [a] -> [[a]]
  semiShrink = shrink

  -- | Report values present in a context.
  fromContext :: Context -> [a]
  fromContext _ = []

-- | Context for generating correlated Marlowe terms and state.
data Context = Context
  { parties :: [Party]
  -- ^ Universe of parties.
  , tokens :: [Token]
  -- ^ Universe of tokens.
  , amounts :: [Integer]
  -- ^ Universe of token amounts.
  , choiceNames :: [ChoiceName]
  -- ^ Universe of choice names.
  , chosenNums :: [ChosenNum]
  -- ^ Universe of chosen numbers.
  , choiceIds :: [ChoiceId]
  -- ^ Universe of token identifiers.
  , valueIds :: [ValueId]
  -- ^ Universe of value identifiers.
  , values :: [Integer]
  -- ^ Universe of values.
  , times :: [POSIXTime]
  -- ^ Universe of times.
  , caccounts :: Accounts
  -- ^ Accounts for state.
  , cchoices :: AM.Map ChoiceId ChosenNum
  -- ^ Choices for state.
  , cboundValues :: AM.Map ValueId Integer
  -- ^ Bound values for state.
  }

instance Arbitrary Context where
  arbitrary =
    do
      parties <- arbitrary
      tokens <- arbitrary
      amounts <- listOf arbitraryPositiveInteger
      choiceNames <- listOf arbitraryChoiceName
      chosenNums <- listOf arbitraryInteger
      valueIds <- arbitrary
      values <- listOf arbitraryInteger
      times <- listOf arbitrary
      choiceIds <- listOf $ ChoiceId <$> perturb arbitraryChoiceName choiceNames <*> perturb arbitrary parties
      caccounts <-
        AM.unsafeFromList . nubBy ((==) `on` fst)
          <$> listOf
            ((,) <$> ((,) <$> perturb arbitrary parties <*> perturb arbitrary tokens) <*> perturb arbitraryPositiveInteger amounts)
      cchoices <-
        AM.unsafeFromList . nubBy ((==) `on` fst)
          <$> listOf ((,) <$> perturb arbitrary choiceIds <*> perturb arbitraryInteger chosenNums)
      cboundValues <-
        AM.unsafeFromList . nubBy ((==) `on` fst)
          <$> listOf ((,) <$> perturb arbitrary valueIds <*> perturb arbitraryInteger values)
      pure Context{..}
  shrink context@Context{..} =
    [context{parties = parties'} | parties' <- shrink parties]
      ++ [context{tokens = tokens'} | tokens' <- shrink tokens]
      ++ [context{amounts = amounts'} | amounts' <- shrink amounts]
      ++ [context{choiceNames = choiceNames'} | choiceNames' <- shrinkList shrinkChoiceName choiceNames]
      ++ [context{chosenNums = chosenNums'} | chosenNums' <- shrink chosenNums]
      ++ [context{valueIds = valueIds'} | valueIds' <- shrink valueIds]
      ++ [context{values = values'} | values' <- shrink values]
      ++ [context{times = times'} | times' <- shrink times]
      ++ [context{choiceIds = choiceIds'} | choiceIds' <- shrink choiceIds]
      ++ [context{caccounts = caccounts'} | caccounts' <- shrink caccounts]
      ++ [context{cchoices = cchoices'} | cchoices' <- shrink cchoices]
      ++ [context{cboundValues = cboundValues'} | cboundValues' <- shrink cboundValues]

-- | Class of reporting the validity of something.
class IsValid a where
  -- | Report whether a value is valid.
  isValid :: a -> Bool

instance IsValid Accounts where
  isValid am =
    let am' = AM.toList am
        am'' = nubBy ((==) `on` fst) $ filter ((> 0) . snd) am'
     in ((==) `on` length) am' am''

instance IsValid (AM.Map ChoiceId ChosenNum) where
  isValid am =
    let am' = AM.toList am
        am'' = nubBy ((==) `on` fst) am'
     in ((==) `on` length) am' am''

instance IsValid (AM.Map ValueId Integer) where
  isValid am =
    let am' = AM.toList am
        am'' = nubBy ((==) `on` fst) am'
     in ((==) `on` length) am' am''

instance IsValid Context where
  isValid Context{..} = isValid caccounts && isValid cchoices && isValid cboundValues

-- | An arbitrary positive integer, mostly small.
arbitraryPositiveInteger :: Gen Integer
arbitraryPositiveInteger =
  arbitraryNonnegativeInteger <&> \case
    0 -> 1
    n -> n

-- | An arbitrary non-negative integer, mostly small.
arbitraryNonnegativeInteger :: Gen Integer
arbitraryNonnegativeInteger =
  frequency
    [ (1, abs <$> arbitrary)
    , (6, floor . sqrt @Double . abs <$> arbitrary)
    ]

-- | An arbitrary integer, mostly small.
arbitraryInteger :: Gen Integer
arbitraryInteger =
  frequency
    [ (1, negate <$> arbitraryPositiveInteger)
    , (6, arbitraryNonnegativeInteger)
    ]

-- | Some public key hashes.
randomPubKeyHashes :: [PubKeyHash]
randomPubKeyHashes =
  [ "03e718291f87b2b004caac168d55e81da688e4501ce560ae613fa7e7"
  , "1b4a1ddd07976d3eee561fdce46d70f2f4c03985195906c08f547249"
  , "2f7bbc9ac7345b557515f6313cd66730241f2b0300c46b12f083ef46"
  , "40db357de24517df3f94cda9b7cc8078a0031d1e4a42cea9127cc730"
  , "592790c9e6af421fff4ffd7ad91de5aed0a703a03445beb8efb17fbf"
  , "6487e89309a827dc94d0e4a9b509c218fb405749d7d4f3fce3ea03f7"
  , "657e1b6b758226ad1c43d544f1d628daadf4eb4b6b411fbbc547ba7c"
  , "7b6fcf5a5528d3ee37493391fd4536a46f9aa63d41bdae6506ecf58d"
  , "8169906694a3558bd393fdd404a60b0ddb51a5e6c018698054f92f0a"
  , "85f24903d9a4f7a9b7284c0440882b1b9e0946ec51ee8cce40ad423b"
  , "9845463ec1285b4f6923133bf58517e4d90d1b1a71263f882ea6911d"
  , "a2bd7dd7f41c2781d1d11c7f4994fac750525705f9c259f97cb27d0e"
  , "c5b4c543a0d0d181ec387ad8250b18617bb18bcf2eccc0f27fe7aa23"
  , "d877b83ece77d785fee4a52bd7226949fa64e111aa0e20cd4a1c471b"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c47"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c470908"
  ]

instance Arbitrary PubKeyHash where
  arbitrary = arbitraryFibonacci randomPubKeyHashes

-- | Some currency symbols.
randomCurrencySymbols :: [CurrencySymbol]
randomCurrencySymbols =
  unsafeMkCurrencySymbolHex
    <$> [ "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8"
        , "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741"
        , "23d79373f7d9edbd016c99e21a473f498a2e425491244ecbc663e9d0"
        , "2839d40108e194eced45205c89613df56bd482e07e6c81a1df2b0e9b"
        , "2c60fb96c894b099f1a21ca9cf51c8c46a4672eb9a30b85252e9adb7"
        , "35c100db45fdf04b9317a2c520c2638ead47fd792984f32c9652cbc7"
        , "443d7002ac74be8c3c53f901d95c89c5932ee8946b188ca9f59db24e"
        , "63f3875b161780b82c7706fbc36fe906e54742e9f5b4c68d260e5da9"
        , "64da8cbb98eccc616bb0061efed2717393e4b48d8f78147396f4521f"
        , "66879477b60f46e5c5ad1d1bb124ab5c3d46a3acc9e54b7da4259655"
        , "9019bb7fb44ec03537b61a6f4aa3fd7b1effaf0776c3d449e9c6274e"
        , "9f92753881b398a247e53b6cad08eab0e158cf1ef5df84c7f5766041"
        , "c1f46ec0147542f9bc155805993497ed44150687a41d0a63af3be466"
        , "cc2189d7adde0ed26355fd03e134feb508e5924959b07a53557f285e"
        , "cc2189d7adde0ed26355fd03e134feb508e5924959b07a53557f"
        , "cc2189d7adde0ed26355fd03e134feb508e5924959b07a53557f123456"
        , ""
        ]

instance Arbitrary CurrencySymbol where
  arbitrary = arbitraryFibonacci randomCurrencySymbols

-- | Some token names.
randomTokenNames :: [TokenName]
randomTokenNames =
  mkTokenNameByteString
    <$> [ "I"
        , "AD"
        , "PIN"
        , "TALE"
        , "RIVER"
        , "METHOD"
        , "REVENUE"
        , ""
        , "POSSIBILITY"
        , "SATISFACTION"
        , "PAYMENT CONCEPT"
        , "OFFICE DEFINITION"
        , "ARTISAN CONVERSATION"
        , "SOFTWARE FEEDBACK METHOD"
        , "INDEPENDENCE EXPLANATION REVENUE"
        , "INDEPENDENCE EXPLANATION REVENUE INDEPENDENCE EXPLANATION REVENUE"
        , "INDEPENDENCE EXPLANATION REVENUE INDEPENDENCE EXPLANATION REVENUE INDEPENDENCE EXPLANATION REVENUE INDEPENDENCE EXPLANATION REVENUE"
        ]

instance Arbitrary TokenName where
  arbitrary = arbitraryFibonacci randomTokenNames
  shrink = shrinkByteString (\(TokenName x) -> x) randomTokenNames

instance Arbitrary Token where
  arbitrary = Token <$> arbitrary <*> arbitrary
  shrink (Token c n)
    | c == adaSymbol && n == adaToken = []
    | otherwise = Token adaSymbol adaToken : (uncurry Token <$> shrink (c, n))

instance SemiArbitrary Token where
  fromContext = tokens

-- | Some role names.
randomRoleNames :: [TokenName]
randomRoleNames =
  mkTokenNameByteString
    <$> [ "Cy"
        , "Noe"
        , "Sten"
        , "Cara"
        , "Alene"
        , "Hande"
        , ""
        , "I"
        , "Zakkai"
        , "Laurent"
        , "Prosenjit"
        , "Dafne Helge Mose"
        , "Nonso Ernie Blanka"
        , "Umukoro Alexander Columb"
        , "Urbanus Roland Alison Ty Ryoichi"
        ]

instance Arbitrary Party where
  arbitrary =
    frequency
      [ (1, Address testnet <$> arbitrary)
      , (4, Role <$> arbitraryFibonacci randomRoleNames)
      ]
  shrink (Address _ _) = []
  shrink (Role x) = Role <$> shrinkByteString unTokenName randomRoleNames x

instance SemiArbitrary Party where
  fromContext = parties

instance Arbitrary Ledger.Address where
  arbitrary = Ledger.Address <$> arbitrary <*> arbitrary

instance Arbitrary Credential where
  arbitrary = do
    frequency
      [ (39, PubKeyCredential <$> arbitrary)
      , (1, ScriptCredential <$> arbitrary)
      ]

instance Arbitrary StakingCredential where
  arbitrary = StakingHash <$> arbitrary

instance Arbitrary ScriptHash where
  arbitrary = arbitraryFibonacci randomScriptHashes

-- | Some validator hashes.
randomScriptHashes :: [ScriptHash]
randomScriptHashes =
  [ "03e718204caac168d55e891f87b2b01da688e4501ce560ae613fa7e7"
  , "1b4a1ddee561fdce46d70d07976d3ef2f4c03985195906c08f547249"
  , "2f7bbc97515f6313cd667ac7345b5530241f2b0300c46b12f083ef46"
  , "40db3573f94cda9b7cc80de24517df78a0031d1e4a42cea9127cc730"
  , "592790cff4ffd7ad91de59e6af421faed0a703a03445beb8efb17fbf"
  , "6487e8994d0e4a9b509c2309a827dc18fb405749d7d4f3fce3ea03f7"
  , "657e1b61c43d544f1d628b758226addaadf4eb4b6b411fbbc547ba7c"
  , "7b6fcf537493391fd4536a5528d3eea46f9aa63d41bdae6506ecf58d"
  , "8169906d393fdd404a60b694a3558b0ddb51a5e6c018698054f92f0a"
  , "85f2490b7284c0440882b3d9a4f7a91b9e0946ec51ee8cce40ad423b"
  , "98454636923133bf58517ec1285b4fe4d90d1b1a71263f882ea6911d"
  , "a2bd7ddd1d11c7f4994fa7f41c2781c750525705f9c259f97cb27d0e"
  , "c5b4c54ec387ad8250b183a0d0d181617bb18bcf2eccc0f27fe7aa23"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c471b"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c47"
  , "d877b83fee4a52bd72269ece77d78549fa64e111aa0e20cd4a1c470908"
  ]

instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitraryInteger
  shrink (POSIXTime i) = POSIXTime <$> shrink i

instance SemiArbitrary POSIXTime where
  fromContext = times

-- | Some choice names.
randomChoiceNames :: [ChoiceName]
randomChoiceNames =
  [ "be"
  , "dry"
  , "grab"
  , "weigh"
  , "enable"
  , "enhance"
  , "allocate"
  , ""
  , "originate"
  , "characterize"
  , "derive witness"
  , "envisage software"
  , "attend unknown animals"
  , "position increated radiation"
  , "candidate apartment reaction replacement"
  ]

-- | Generate a choice name from a few possibilities.
arbitraryChoiceName :: Gen ChoiceName
arbitraryChoiceName = arbitraryFibonacci randomChoiceNames

-- | Shrink a generated choice name.
shrinkChoiceName :: ChoiceName -> [ChoiceName]
shrinkChoiceName = shrinkByteString id randomChoiceNames

-- | Generate a random choice inside given bounds.
choiceInBoundsIfNonempty :: [Bound] -> Gen ChosenNum
choiceInBoundsIfNonempty [] = arbitrary
choiceInBoundsIfNonempty bounds = oneof $ chooseInBound <$> bounds
  where
    chooseInBound (Bound lower upper) = chooseInteger (lower, upper)

-- | Generate a random choice not in given bounds.
choiceNotInBounds :: [Bound] -> Gen ChosenNum
choiceNotInBounds [] = arbitrary
choiceNotInBounds bounds =
  let inBound chosenNum (Bound lower upper) = chosenNum >= lower && chosenNum <= upper
   in suchThat arbitrary $ \chosenNum -> not $ any (inBound chosenNum) bounds

-- | Generate relevant Input content for a given input action
interestingInput :: Environment -> State -> Bool -> Action -> [InputContent]
interestingInput env state validity (Notify value) =
  let validNotification = evalObservation env state value :: Bool
   in [INotify | validity == validNotification]
interestingInput env state validity (Deposit account party token value) =
  let expectedDepositAmount = evalValue env state value :: Integer
   in if validity
        then [IDeposit account party token expectedDepositAmount]
        else
          [ IDeposit account party token (expectedDepositAmount - 1)
          , IDeposit account party token (expectedDepositAmount + 1)
          ]
            <> [IDeposit account party' token expectedDepositAmount | party' <- interestingParties party validity]
            <> [IDeposit account' party token expectedDepositAmount | account' <- interestingParties account validity]
interestingInput _ _ validity (Choice choiceId bounds) = IChoice <$> interestingChoiceId choiceId validity <*> interestingChoiceNums validity bounds

-- ChoiceId is a choice name and a party making the choice. Then we want to generate a list of choiceIds that are either invalid or valid
interestingChoiceId :: ChoiceId -> Bool -> [ChoiceId]
interestingChoiceId (ChoiceId byteString party) validity = fmap (\party' -> ChoiceId byteString party') $ interestingParties party validity

interestingChoiceNums :: Bool -> [Bound] -> [ChosenNum]
interestingChoiceNums True bounds = concatMap (interestingChoiceNum True) bounds
interestingChoiceNums False bounds =
  let candidates = concatMap (interestingChoiceNum False) bounds
   in [ candidate
      | candidate <- candidates
      , let outside (Bound lower upper) = candidate < lower && candidate > upper
      , all outside bounds
      ]

interestingChoiceNum :: Bool -> Bound -> [ChosenNum]
interestingChoiceNum True (Bound lower upper) = validValues lower upper
interestingChoiceNum False (Bound lower upper) = invalidValues lower upper

-- TODO: `interestingParties` is missing a case for PK which is currently being replaced with Address.
-- We will need to update the PK case with and address case when it's ready.
interestingParties :: Party -> Bool -> [Party]
interestingParties validRole True = [validRole]
interestingParties (Role roleName) False =
  [ Role (removeFirstLetter roleName)
  , Role (TokenName $ unTokenName roleName `appendByteString` "s")
  ]
interestingParties _ _ = []

removeFirstLetter :: TokenName -> TokenName
removeFirstLetter (TokenName tokenName) = TokenName $ sliceByteString 1 (lengthOfByteString tokenName) tokenName

validValues :: Integer -> Integer -> [Integer]
validValues lower upper = [x | x <- availableValues, x >= lower && x <= upper]

invalidValues :: Integer -> Integer -> [Integer]
invalidValues lower upper = [x | x <- availableValues, x < lower || x > upper]

availableValues :: [Integer]
availableValues = drop 1 $ [0 ..] >>= \x -> [x, -x]

-- | Generate a semi-random time interval.
arbitraryTimeInterval :: Gen TimeInterval
arbitraryTimeInterval =
  do
    start <- arbitraryInteger
    duration <- arbitraryNonnegativeInteger
    pure (POSIXTime start, POSIXTime $ start + duration)

-- | Generate a semi-random time interval straddling a given time.
arbitraryTimeIntervalAround :: POSIXTime -> Gen TimeInterval
arbitraryTimeIntervalAround (POSIXTime limit) =
  do
    (dStart, dEnd) <- (,) <$> arbitraryNonnegativeInteger <*> arbitraryNonnegativeInteger
    pure (POSIXTime $ limit - dStart, POSIXTime $ limit + dEnd)

-- | Generate a semi-random time interval before a given time.
arbitraryTimeIntervalBefore :: POSIXTime -> POSIXTime -> Gen TimeInterval
arbitraryTimeIntervalBefore (POSIXTime lower) (POSIXTime upper) =
  do
    (dStart, dEnd) <- (,) <$> arbitraryNonnegativeInteger <*> arbitraryNonnegativeInteger
    pure (POSIXTime $ lower - dStart, POSIXTime $ upper - dEnd)

-- | Generate a semi-random time interval after a given time.
arbitraryTimeIntervalAfter :: POSIXTime -> Gen TimeInterval
arbitraryTimeIntervalAfter (POSIXTime lower) =
  do
    (dStart, duration) <- (,) <$> arbitraryNonnegativeInteger <*> arbitraryNonnegativeInteger
    let start = lower + dStart
    pure (POSIXTime start, POSIXTime $ start + duration)

-- | Shrink a generated time interval.
shrinkTimeInterval :: TimeInterval -> [TimeInterval]
shrinkTimeInterval (start, end) =
  let mid = (start + end) `div` 2
   in filter (/= (start, end)) $
        nub
          [ (start, start)
          , (start, mid)
          , (mid, mid)
          , (mid, end)
          , (end, end)
          ]

instance Arbitrary ChoiceId where
  arbitrary = ChoiceId <$> arbitraryChoiceName <*> arbitrary
  shrink (ChoiceId n p) =
    (ChoiceId <$> shrinkChoiceName n <*> pure p)
      ++ (ChoiceId n <$> shrink p)

instance SemiArbitrary ChoiceId where
  fromContext = choiceIds

-- | Some value identifiers.
randomValueIds :: [ValueId]
randomValueIds =
  [ "x"
  , "id"
  , "lab"
  , "idea"
  , "story"
  , "memory"
  , "fishing"
  , ""
  , "drawing"
  , "reaction"
  , "difference"
  , "replacement"
  , "paper apartment"
  , "leadership information"
  , "entertainment region assumptions"
  , "candidate apartment reaction replacement" -- NB: Too long for ledger.
  ]

instance Arbitrary ValueId where
  arbitrary = arbitraryFibonacci randomValueIds
  shrink = shrinkByteString (\(ValueId x) -> x) randomValueIds

instance SemiArbitrary ValueId where
  fromContext = valueIds

-- | Generate a semi-random integer.
arbitraryNumber
  :: (Context -> [Integer])
  -- ^ How to select the universe of some possibilities.
  -> Context
  -- ^ The Marlowe context.
  -> Gen Integer
  -- ^ Generator for a integer.
arbitraryNumber = (perturb arbitraryInteger .)

-- | Generate a semi-random token amount.
arbitraryAmount :: Context -> Gen Integer
arbitraryAmount = arbitraryNumber amounts

-- | Generate a semi-random chosen number.
arbitraryChosenNum :: Context -> Gen Integer
arbitraryChosenNum = arbitraryNumber chosenNums

-- | Generate a semi-random value.
arbitraryValueNum :: Context -> Gen Integer
arbitraryValueNum = arbitraryNumber values

instance SemiArbitrary Integer where
  semiArbitrary context =
    frequency
      [ (1, arbitraryAmount context)
      , (1, arbitraryChosenNum context)
      , (1, arbitraryValueNum context)
      ]

instance Arbitrary (Value Observation) where
  arbitrary = sized \size ->
    if size <= 0
      then leaves
      else
        oneof
          [ leaves
          , resize (pred size) $ NegValue <$> arbitrary
          , resize (size `quot` 2) $ AddValue <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ SubValue <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ MulValue <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ DivValue <$> arbitrary <*> arbitrary
          , resize (size `quot` 3) $ Cond <$> arbitrary <*> arbitrary <*> arbitrary
          ]
    where
      leaves =
        frequency
          [ (4, AvailableMoney <$> arbitrary <*> arbitrary)
          , (7, Constant <$> arbitrary)
          , (5, ChoiceValue <$> arbitrary)
          , (3, pure TimeIntervalStart)
          , (3, pure TimeIntervalEnd)
          , (4, UseValue <$> arbitrary)
          ]

  shrink (AvailableMoney a t) = [AvailableMoney a' t | a' <- shrink a] ++ [AvailableMoney a t' | t' <- shrink t]
  shrink (NegValue x) = x : (NegValue <$> shrink x)
  shrink (AddValue x y) = x : y : ([AddValue x' y | x' <- shrink x] ++ [AddValue x y' | y' <- shrink y])
  shrink (SubValue x y) = x : y : ([SubValue x' y | x' <- shrink x] ++ [SubValue x y' | y' <- shrink y])
  shrink (MulValue x y) = x : y : ([MulValue x' y | x' <- shrink x] ++ [MulValue x y' | y' <- shrink y])
  shrink (DivValue x y) = x : y : ([DivValue x' y | x' <- shrink x] ++ [DivValue x y' | y' <- shrink y])
  shrink (ChoiceValue c) = ChoiceValue <$> shrink c
  shrink (UseValue v) = UseValue <$> shrink v
  shrink (Cond o x y) = x : y : ([Cond o' x y | o' <- shrink o] ++ [Cond o x' y | x' <- shrink x] ++ [Cond o x y' | y' <- shrink y])
  shrink (Constant c) = Constant <$> shrink c
  shrink TimeIntervalStart = []
  shrink TimeIntervalEnd = []

instance SemiArbitrary (Value Observation) where
  semiArbitrary context = sized \size ->
    if size <= 0
      then leaves
      else
        oneof
          [ leaves
          , resize (pred size) $ NegValue <$> semiArbitrary context
          , resize (size `quot` 2) $ AddValue <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ SubValue <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ MulValue <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ DivValue <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 3) $ Cond <$> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context
          ]
    where
      leaves =
        frequency
          [ (4, AvailableMoney <$> semiArbitrary context <*> semiArbitrary context)
          , (7, Constant <$> semiArbitrary context)
          , (5, ChoiceValue <$> semiArbitrary context)
          , (3, pure TimeIntervalStart)
          , (3, pure TimeIntervalEnd)
          , (4, UseValue <$> semiArbitrary context)
          ]

instance Arbitrary Observation where
  arbitrary = sized \size ->
    if size <= 0
      then leaves
      else
        oneof -- size > 0 produces compound observations.
          [ leaves
          , resize (size `quot` 2) $ AndObs <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ OrObs <$> arbitrary <*> arbitrary
          , resize (pred size) $ NotObs <$> arbitrary
          , resize (size `quot` 2) $ ValueGE <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ ValueGT <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ ValueLT <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ ValueLE <$> arbitrary <*> arbitrary
          , resize (size `quot` 2) $ ValueEQ <$> arbitrary <*> arbitrary
          ]
    where
      leaves =
        frequency
          [ (8, ChoseSomething <$> arbitrary)
          , (5, pure TrueObs)
          , (5, pure FalseObs)
          ]
  shrink (AndObs x y) = x : y : ([AndObs x' y | x' <- shrink x] ++ [AndObs x y' | y' <- shrink y])
  shrink (OrObs x y) = x : y : ([OrObs x' y | x' <- shrink x] ++ [OrObs x y' | y' <- shrink y])
  shrink (NotObs x) = x : (NotObs <$> shrink x)
  shrink (ChoseSomething c) = TrueObs : FalseObs : (ChoseSomething <$> shrink c)
  shrink (ValueGE x y) = TrueObs : FalseObs : [ValueGE x' y | x' <- shrink x] ++ [ValueGE x y' | y' <- shrink y]
  shrink (ValueGT x y) = TrueObs : FalseObs : [ValueGT x' y | x' <- shrink x] ++ [ValueGT x y' | y' <- shrink y]
  shrink (ValueLT x y) = TrueObs : FalseObs : [ValueLT x' y | x' <- shrink x] ++ [ValueLT x y' | y' <- shrink y]
  shrink (ValueLE x y) = TrueObs : FalseObs : [ValueLE x' y | x' <- shrink x] ++ [ValueLE x y' | y' <- shrink y]
  shrink (ValueEQ x y) = TrueObs : FalseObs : [ValueEQ x' y | x' <- shrink x] ++ [ValueEQ x y' | y' <- shrink y]
  shrink TrueObs = []
  shrink FalseObs = []

instance SemiArbitrary Observation where
  semiArbitrary context = sized \size ->
    if size <= 0
      then leaves
      else
        oneof
          [ leaves
          , resize (size `quot` 2) $ AndObs <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ OrObs <$> semiArbitrary context <*> semiArbitrary context
          , resize (pred size) $ NotObs <$> semiArbitrary context
          , resize (size `quot` 2) $ ValueGE <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ ValueGT <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ ValueLT <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ ValueLE <$> semiArbitrary context <*> semiArbitrary context
          , resize (size `quot` 2) $ ValueEQ <$> semiArbitrary context <*> semiArbitrary context
          ]
    where
      leaves =
        frequency
          [ (8, ChoseSomething <$> semiArbitrary context)
          , (5, pure TrueObs)
          , (5, pure FalseObs)
          ]

instance Arbitrary Bound where
  arbitrary =
    do
      lower <- arbitraryInteger
      extent <- arbitraryNonnegativeInteger
      pure $ Bound lower (lower + extent)
  shrink (Bound lower upper) =
    let mid = (lower + upper) `div` 2
     in filter (/= Bound lower upper) $
          nub
            [ Bound lower lower
            , Bound lower mid
            , Bound mid mid
            , Bound mid upper
            , Bound upper upper
            ]

instance SemiArbitrary Bound where
  semiArbitrary context =
    do
      lower <- semiArbitrary context
      extent <- arbitraryNonnegativeInteger
      pure $ Bound lower (lower + extent)

instance (SemiArbitrary a) => SemiArbitrary [a] where
  semiArbitrary = listOf . semiArbitrary

instance Arbitrary Action where
  arbitrary =
    frequency
      [ (3, Deposit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
      , (6, Choice <$> arbitrary <*> (take 4 <$> arbitrary))
      , (1, Notify <$> arbitrary)
      ]
  shrink (Deposit a p t x) =
    [Deposit a' p t x | a' <- shrink a]
      ++ [Deposit a p' t x | p' <- shrink p]
      ++ [Deposit a p t' x | t' <- shrink t]
      ++ [Deposit a p t x' | x' <- shrink x]
  shrink (Choice c b) = [Choice c' b | c' <- shrink c] ++ [Choice c b' | b' <- shrink b]
  shrink (Notify o) = Notify <$> shrink o

instance SemiArbitrary Action where
  semiArbitrary context@Context{..} =
    let arbitraryDeposit =
          do
            (account, token) <- perturb arbitrary $ AM.keys caccounts
            party <- semiArbitrary context
            Deposit account party token <$> semiArbitrary context
        arbitraryChoice = Choice <$> semiArbitrary context <*> semiArbitrary context
     in frequency
          [ (3, arbitraryDeposit)
          , (6, arbitraryChoice)
          , (1, Notify <$> semiArbitrary context)
          ]

instance Arbitrary Payee where
  arbitrary =
    oneof
      [ Party <$> arbitrary
      , Account <$> arbitrary
      ]
  shrink (Party x) = Party <$> shrink x
  shrink (Account x) = Account <$> shrink x

instance SemiArbitrary Payee where
  semiArbitrary context =
    oneof
      [ Party <$> semiArbitrary context
      , Account <$> semiArbitrary context
      ]

instance Arbitrary (Case Contract) where
  arbitrary = semiArbitrary =<< arbitrary
  shrink (Case a c) = [Case a' c | a' <- shrink a] ++ [Case a c' | c' <- shrink c]
  shrink (MerkleizedCase a c) = (`MerkleizedCase` c) <$> shrink a

instance SemiArbitrary (Case Contract) where
  semiArbitrary context = Case <$> semiArbitrary context <*> semiArbitrary context

-- | Generate a random case, weighted towards different contract constructs.
arbitraryCaseWeighted
  :: [(Int, Int, Int, Int, Int, Int)]
  -- ^ The weights for contract terms.
  -> Context
  -- ^ The Marlowe context.
  -> Gen (Case Contract)
  -- ^ Generator for a case.
arbitraryCaseWeighted w context =
  Case <$> semiArbitrary context <*> arbitraryContractWeighted w context

-- | Generate one of the golden contracts and its initial state.
goldenContract :: Gen (Contract, State)
goldenContract = (,) <$> elements goldenContracts <*> pure (State AM.empty AM.empty AM.empty $ POSIXTime 0)

instance Arbitrary Contract where
  arbitrary = frequency [(95, semiArbitrary =<< arbitrary), (5, fst <$> goldenContract)]
  shrink (Pay a p t x c) =
    c
      : [Pay a' p t x c | a' <- shrink a]
      ++ [Pay a p' t x c | p' <- shrink p]
      ++ [Pay a p t' x c | t' <- shrink t]
      ++ [Pay a p t x' c | x' <- shrink x]
      ++ [Pay a p t x c' | c' <- shrink c]
  shrink (If o x y) = x : y : [If o' x y | o' <- shrink o] ++ [If o x' y | x' <- shrink x] ++ [If o x y' | y' <- shrink y]
  shrink (When a t c) =
    c
      : [When a' t c | a' <- shrink a]
      ++ [When a t' c | t' <- shrink t]
      ++ [When a t c' | c' <- shrink c]
      ++ (a >>= \case Case _ contract -> [contract]; _ -> [])
  shrink (Let v x c) = c : [Let v' x c | v' <- shrink v] ++ [Let v x' c | x' <- shrink x] ++ [Let v x c' | c' <- shrink c]
  shrink (Assert o c) = c : [Assert o' c | o' <- shrink o] ++ [Assert o c' | c' <- shrink c]
  shrink Close = []

-- | Generate an arbitrary contract, weighted towards different contract constructs.
arbitraryContractWeighted
  :: [(Int, Int, Int, Int, Int, Int)]
  -- ^ The weights of contract terms, which must eventually include `Close` as a possibility.
  -> Context
  -- ^ The Marlowe context.
  -> Gen Contract
  -- ^ Generator for a contract.
arbitraryContractWeighted ((wClose, wPay, wIf, wWhen, wLet, wAssert) : w) context =
  frequency
    [ (wClose, pure Close)
    ,
      ( wPay
      , Pay
          <$> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> arbitraryContractWeighted w context
      )
    , (wIf, If <$> semiArbitrary context <*> arbitraryContractWeighted w context <*> arbitraryContractWeighted w context)
    ,
      ( wWhen
      , When
          <$> (take (length w) <$> listOf (arbitraryCaseWeighted w context))
          <*> semiArbitrary context
          <*> arbitraryContractWeighted w context
      )
    , (wLet, Let <$> semiArbitrary context <*> semiArbitrary context <*> arbitraryContractWeighted w context)
    , (wAssert, Assert <$> semiArbitrary context <*> arbitraryContractWeighted w context)
    ]
arbitraryContractWeighted [] _ = pure Close

-- | Default weights for contract terms.
defaultContractWeights :: (Int, Int, Int, Int, Int, Int)
defaultContractWeights = (35, 20, 10, 15, 20, 5)

-- | Contract weights selecting only `Close`.
closeContractWeights :: (Int, Int, Int, Int, Int, Int)
closeContractWeights = (1, 0, 0, 0, 0, 0)

-- | Contract weights selecting only `Pay`.
payContractWeights :: (Int, Int, Int, Int, Int, Int)
payContractWeights = (0, 1, 0, 0, 0, 0)

-- | Contract weights selecting only `If`.
ifContractWeights :: (Int, Int, Int, Int, Int, Int)
ifContractWeights = (0, 0, 1, 0, 0, 0)

-- | Contract weights selecting only `When`.
whenContractWeights :: (Int, Int, Int, Int, Int, Int)
whenContractWeights = (0, 0, 0, 1, 0, 0)

-- | Contract weights selecting only `Let`.
letContractWeights :: (Int, Int, Int, Int, Int, Int)
letContractWeights = (0, 0, 0, 0, 1, 0)

-- | Contract weights selecting only `Assert`.
assertContractWeights :: (Int, Int, Int, Int, Int, Int)
assertContractWeights = (0, 0, 0, 0, 0, 1)

instance SemiArbitrary Contract where
  semiArbitrary ctx = sized \size ->
    if size <= 0
      then
        oneof -- Why not simply close? Because there are tests that effectively do this: `arbitrary `suchThat` (/= Close), which
          [ Pay <$> semiArbitrary ctx <*> semiArbitrary ctx <*> semiArbitrary ctx <*> semiArbitrary ctx <*> pure Close
          , If <$> semiArbitrary ctx <*> resize (size `quot` 2) (semiArbitrary ctx) <*> pure Close
          , When [Case (Notify TrueObs) Close] <$> semiArbitrary ctx <*> semiArbitrary ctx
          , Let <$> semiArbitrary ctx <*> semiArbitrary ctx <*> pure Close
          , Assert <$> semiArbitrary ctx <*> pure Close
          , pure Close
          ]
      else
        frequency
          [
            ( 4
            , Pay
                <$> semiArbitrary ctx
                <*> semiArbitrary ctx
                <*> semiArbitrary ctx
                <*> semiArbitrary ctx
                <*> resize (pred size) (semiArbitrary ctx)
            )
          ,
            ( 2
            , If <$> semiArbitrary ctx <*> resize (size `quot` 2) (semiArbitrary ctx) <*> resize (size `quot` 2) (semiArbitrary ctx)
            )
          ,
            ( 3
            , do
                -- Since the size of a `When` is O(c*n) where `c` is the number of cases and `n` is the size of sub
                -- contracts, we need to use `c ~ sqrt size` and `n = size / c` to create a contract that is an appropriate size.
                let maxCases = floor $ sqrt @Double $ fromIntegral size
                numCases <- chooseInt (0, maxCases)
                let numSubContracts = succ numCases
                let subContractSize = size `quot` numSubContracts
                When
                  <$> (vectorOf numCases $ resize subContractSize $ semiArbitrary ctx)
                  <*> semiArbitrary ctx
                  <*> resize subContractSize (semiArbitrary ctx)
            )
          , (4, Let <$> semiArbitrary ctx <*> semiArbitrary ctx <*> resize (pred size) (semiArbitrary ctx))
          , (1, Assert <$> semiArbitrary ctx <*> resize (pred size) (semiArbitrary ctx))
          , (1, pure Close)
          ]

-- | Generate a random association map.
arbitraryAssocMap :: (Eq k) => Gen k -> Gen v -> Gen (AM.Map k v)
arbitraryAssocMap arbitraryKey arbitraryValue =
  AM.unsafeFromList . nubBy ((==) `on` fst)
    <$> listOf ((,) <$> arbitraryKey <*> arbitraryValue)

-- | Shrink a generated association map.
shrinkAssocMap :: (P.Eq k) => AM.Map k v -> [AM.Map k v]
shrinkAssocMap am =
  [ AM.delete k am
  | k <- AM.keys am
  ]

instance Arbitrary Accounts where
  arbitrary = arbitraryAssocMap arbitrary arbitraryPositiveInteger
  shrink = shrinkAssocMap

instance SemiArbitrary Accounts where
  semiArbitrary context = arbitraryAssocMap (semiArbitrary context) (semiArbitrary context `suchThat` (> 0))

instance {-# OVERLAPPING #-} SemiArbitrary TimeInterval where
  semiArbitrary context =
    do
      POSIXTime start <- semiArbitrary context
      duration <- arbitraryNonnegativeInteger
      pure (POSIXTime start, POSIXTime $ start + duration)
  semiShrink = fmap shrinkTimeInterval

instance {-# OVERLAPPING #-} (SemiArbitrary a, SemiArbitrary b) => SemiArbitrary (a, b) where
  semiArbitrary context = (,) <$> semiArbitrary context <*> semiArbitrary context

instance Arbitrary (AM.Map ChoiceId ChosenNum) where
  arbitrary = arbitraryAssocMap arbitrary arbitraryInteger
  shrink = shrinkAssocMap

instance SemiArbitrary (AM.Map ChoiceId ChosenNum) where
  semiArbitrary context = arbitraryAssocMap (semiArbitrary context) (semiArbitrary context)

instance Arbitrary (AM.Map ValueId Integer) where
  arbitrary = arbitraryAssocMap arbitrary arbitraryInteger
  shrink = shrinkAssocMap

instance SemiArbitrary (AM.Map ValueId Integer) where
  semiArbitrary context = arbitraryAssocMap (semiArbitrary context) (semiArbitrary context)

instance Arbitrary State where
  arbitrary = semiArbitrary =<< arbitrary
  shrink s@State{..} =
    [s{accounts = accounts'} | accounts' <- shrinkAssocMap accounts]
      <> [s{choices = choices'} | choices' <- shrinkAssocMap choices]
      <> [s{boundValues = boundValues'} | boundValues' <- shrinkAssocMap boundValues]
      <> [s{minTime = minTime'} | minTime' <- shrink minTime]

instance SemiArbitrary State where
  semiArbitrary context =
    do
      accounts <- semiArbitrary context
      choices <- semiArbitrary context
      boundValues <- semiArbitrary context
      minTime <- semiArbitrary context
      pure State{..}

instance Arbitrary Environment where
  arbitrary = Environment <$> arbitraryTimeInterval
  shrink (Environment x) = Environment <$> shrinkTimeInterval x

instance SemiArbitrary Environment where
  semiArbitrary context = Environment <$> semiArbitrary context

instance Arbitrary InputContent where
  arbitrary = semiArbitrary =<< arbitrary
  shrink (IDeposit a p t x) =
    [IDeposit a' p t x | a' <- shrink a]
      ++ [IDeposit a p' t x | p' <- shrink p]
      ++ [IDeposit a p t' x | t' <- shrink t]
      ++ [IDeposit a p t x' | x' <- shrink x]
  shrink (IChoice c x) = [IChoice c' x | c' <- shrink c] ++ [IChoice c x' | x' <- shrink x]
  shrink _ = []

instance SemiArbitrary InputContent where
  semiArbitrary context =
    do
      deposit <- IDeposit <$> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context <*> arbitrary
      choice <- IChoice <$> semiArbitrary context <*> semiArbitrary context
      elements [deposit, choice, INotify]

instance Arbitrary Input where
  arbitrary = semiArbitrary =<< arbitrary
  shrink (NormalInput i) = NormalInput <$> shrink i
  shrink (MerkleizedInput i b c) =
    [NormalInput i]
      <> [MerkleizedInput i' b c | i' <- shrink i]
      <> [MerkleizedInput i (dataHash c) c' | c' <- shrink c]

instance SemiArbitrary Input where
  semiArbitrary context =
    frequency
      [ (9, NormalInput <$> semiArbitrary context)
      ,
        ( 1
        , do
            input <- semiArbitrary context
            contract <- semiArbitrary context
            pure $
              MerkleizedInput input (dataHash contract) contract
        )
      ]

instance Arbitrary TransactionInput where
  arbitrary = semiArbitrary =<< arbitrary
  shrink TransactionInput{..} =
    [TransactionInput txInterval' txInputs | txInterval' <- shrink txInterval]
      <> [TransactionInput txInterval txInputs' | txInputs' <- shrink txInputs]

instance SemiArbitrary TransactionInput where
  semiArbitrary context = TransactionInput <$> semiArbitrary context <*> semiArbitrary context

-- | Generate a random step for a contract.
arbitraryValidStep
  :: State
  -- ^ The state of the contract.
  -> Contract
  -- ^ The contract.
  -> Gen TransactionInput
  -- ^ Generator for a transaction input for a single step.
arbitraryValidStep _ (When [] timeout _) =
  TransactionInput <$> arbitraryTimeIntervalAfter timeout <*> pure []
arbitraryValidStep state@State{..} contract@(When cases timeout _) =
  do
    let isEmptyChoice (Choice _ []) = True
        isEmptyChoice _ = False
    isTimeout <- frequency [(9, pure False), (1, pure True)]
    if isTimeout || minTime >= timeout || all (isEmptyChoice . getAction) cases
      then TransactionInput <$> arbitraryTimeIntervalAfter timeout <*> pure []
      else do
        times <- arbitraryTimeIntervalBefore minTime timeout
        case' <- elements $ filter (not . isEmptyChoice . getAction) cases
        i <- case getAction case' of
          Deposit a p t v -> pure . IDeposit a p t $ evalValue (Environment times) state v
          Choice n bs -> do
            Bound lower upper <- elements bs
            IChoice n <$> chooseInteger (lower, upper)
          Notify _ -> pure INotify
        is <-
          frequency
            [ (9, pure [NormalInput i])
            ,
              ( 1
              , pure [MerkleizedInput i (dataHash contract) contract]
              )
            ]
        pure $ TransactionInput times is
arbitraryValidStep State{minTime} contract =
  {-
    NOTE: Alternatively, if semantics should allow applying `[]` to a non-quiescent contract
    without ever throwing a timeout-related error, then replace the above with the following:

    TransactionInput <$> arbitraryTimeIntervalAround minTime <*> pure []
  -}
  let nextTimeout Close = minTime
      nextTimeout (Pay _ _ _ _ continuation) = nextTimeout continuation
      nextTimeout (If _ thenContinuation elseContinuation) = on max nextTimeout thenContinuation elseContinuation
      nextTimeout (When _ timeout _) = timeout
      nextTimeout (Let _ _ continuation) = nextTimeout continuation
      nextTimeout (Assert _ continuation) = nextTimeout continuation
   in TransactionInput <$> arbitraryTimeIntervalAfter (max minTime (nextTimeout contract)) <*> pure []

-- | Generate random transaction input.
arbitraryValidInput
  :: State
  -- ^ The state of the contract.
  -> Contract
  -- ^ The contract.
  -> Gen TransactionInput
  -- ^ Generator for a transaction input.
arbitraryValidInput = arbitraryValidInput' Nothing

arbitraryValidInput' :: Maybe TransactionInput -> State -> Contract -> Gen TransactionInput
arbitraryValidInput' Nothing state contract =
  arbitraryValidStep state contract
arbitraryValidInput' (Just input) state contract =
  case computeTransaction input state contract of
    Error{} -> pure input
    TransactionOutput{..} -> do
      nextInput <- arbitraryValidStep state contract
      let combinedInput = input{txInputs = txInputs input ++ txInputs nextInput}
      case computeTransaction combinedInput txOutState txOutContract of
        Error{} -> pure input
        TransactionOutput{} -> pure combinedInput

-- | Generate a random path through a contract.
arbitraryValidInputs
  :: State
  -- ^ The state of the contract.
  -> Contract
  -- ^ The contract.
  -> Gen [TransactionInput]
  -- ^ Generator for a transaction inputs.
arbitraryValidInputs _ Close = pure []
arbitraryValidInputs state contract =
  do
    input <- arbitraryValidInput state contract
    case computeTransaction input state contract of -- FIXME: It is tautological to use `computeTransaction` to filter test cases.
      Error{} -> pure []
      TransactionOutput{..} -> (input :) <$> arbitraryValidInputs txOutState txOutContract

-- | Generate an arbitrary golden transaction.
arbitraryGoldenTransaction :: Bool -> Gen GoldenTransaction
arbitraryGoldenTransaction allowMerkleization =
  do
    let perhapsMerkleize gt@(state, contract, input, _) =
          let (mcContract, mcContinuations) = runWriter $ shallowMerkleize contract
              input' = merkleizeInputs @String MerkleizedContract{..} state input
           in case input' of
                Left _ -> pure gt
                Right input'' -> frequency [(9, pure gt), (1, pure (state, mcContract, input'', computeTransaction input'' state mcContract))]
    equalContractWeights <- frequency [(1, pure True), (5, pure False)]
    (if allowMerkleization then perhapsMerkleize else pure)
      =<< if equalContractWeights
        then elements =<< elements goldenTransactions
        else elements $ concat goldenTransactions

instance Arbitrary Payment where
  arbitrary = Payment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary IntervalError where
  arbitrary =
    oneof
      [ InvalidInterval <$> arbitrary
      , IntervalInPastError <$> arbitrary <*> arbitrary
      ]

instance Arbitrary TransactionError where
  arbitrary =
    oneof
      [ pure TEAmbiguousTimeIntervalError
      , pure TEApplyNoMatchError
      , TEIntervalError <$> arbitrary
      , pure TEUselessTransaction
      , pure TEHashMismatch
      ]

instance Arbitrary TransactionWarning where
  arbitrary =
    oneof
      [ TransactionNonPositiveDeposit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , TransactionNonPositivePay <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , TransactionPartialPay <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , TransactionShadowing <$> arbitrary <*> arbitrary <*> arbitrary
      , pure TransactionAssertionFailed
      ]

instance Arbitrary TransactionOutput where
  arbitrary =
    oneof
      [ TransactionOutput <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , Error <$> arbitrary
      ]

instance Arbitrary SafetyError where
  arbitrary =
    oneof
      [ pure MissingRolesCurrency
      , pure ContractHasNoRoles
      , MissingRoleToken <$> arbitrary
      , ExtraRoleToken <$> arbitrary
      , RoleNameTooLong <$> arbitrary
      , InvalidCurrencySymbol <$> arbitrary
      , TokenNameTooLong <$> arbitrary
      , InvalidToken <$> arbitrary
      , NonPositiveBalance <$> arbitrary <*> arbitrary
      , DuplicateAccount <$> arbitrary <*> arbitrary
      , DuplicateChoice <$> arbitrary
      , DuplicateBoundValue <$> arbitrary
      , MaximumValueMayExceedProtocol . fromInteger <$> arbitraryPositiveInteger
      , TransactionSizeMayExceedProtocol <$> arbitrary <*> fmap fromInteger arbitraryPositiveInteger
      , TransactionCostMayExceedProtocol
          <$> arbitrary
          <*> (ExBudget <$> fmap fromInteger arbitraryPositiveInteger <*> fmap fromInteger arbitraryPositiveInteger)
      , TransactionValidationError <$> arbitrary <*> arbitrary
      , TransactionWarning <$> arbitrary <*> arbitrary
      , MissingContinuation . DatumHash . toBuiltin . pack <$> replicateM 32 arbitrary
      , pure InconsistentNetworks
      , pure WrongNetwork
      , IllegalAddress <$> arbitrary
      , pure SafetyAnalysisTimeout
      ]
  shrink MissingRolesCurrency = mempty
  shrink ContractHasNoRoles = mempty
  shrink (MissingRoleToken tokenName) = MissingRoleToken <$> shrink tokenName
  shrink (ExtraRoleToken tokenName) = ExtraRoleToken <$> shrink tokenName
  shrink (RoleNameTooLong tokenName) = RoleNameTooLong <$> shrink tokenName
  shrink (InvalidCurrencySymbol _) = mempty
  shrink (TokenNameTooLong tokenName) = TokenNameTooLong <$> shrink tokenName
  shrink (InvalidToken token) = InvalidToken <$> shrink token
  shrink (NonPositiveBalance accountId token) =
    (NonPositiveBalance accountId <$> shrink token)
      <> (flip NonPositiveBalance token <$> shrink accountId)
  shrink (DuplicateAccount accountId token) =
    (DuplicateAccount accountId <$> shrink token)
      <> (flip DuplicateAccount token <$> shrink accountId)
  shrink (DuplicateChoice choiceId) = DuplicateChoice <$> shrink choiceId
  shrink (DuplicateBoundValue valueId) = DuplicateBoundValue <$> shrink valueId
  shrink (MaximumValueMayExceedProtocol _) = mempty
  shrink (TransactionSizeMayExceedProtocol transaction natural) =
    flip TransactionSizeMayExceedProtocol natural <$> shrink transaction
  shrink (TransactionCostMayExceedProtocol transaction exBudget) =
    flip TransactionCostMayExceedProtocol exBudget <$> shrink transaction
  shrink (TransactionValidationError transaction string) =
    flip TransactionValidationError string <$> shrink transaction
  shrink (TransactionWarning transaction warning) =
    flip TransactionWarning warning <$> shrink transaction
  shrink (MissingContinuation _) = mempty
  shrink InconsistentNetworks = mempty
  shrink WrongNetwork = mempty
  shrink (IllegalAddress address) = IllegalAddress <$> shrink address
  shrink SafetyAnalysisTimeout = mempty

instance (Arbitrary a) => Arbitrary (Transaction a) where
  arbitrary =
    do
      context <- arbitrary
      txState <- semiArbitrary context
      txContract <- semiArbitrary context
      txInput <- semiArbitrary context
      txOutput <- arbitrary
      txAnnotation <- arbitrary
      pure Transaction{..}
  shrink transaction@Transaction{..} =
    [transaction{txState = state} | state <- shrink txState]
      <> [transaction{txContract = contract} | contract <- shrink txContract]
      <> [transaction{txInput = input} | input <- shrink txInput]
      <> [transaction{txOutput = output} | output <- shrink txOutput]

newtype ValidContractStructure = ValidContractStructure {unValidContractStructure :: Contract}
  deriving (Eq, Show)

instance Arbitrary ValidContractStructure where
  arbitrary = do
    contract <- arbitrary
    case A.fromJSON . A.toJSON $ contract of
      A.Success (_ :: Contract) -> pure $ ValidContractStructure contract
      A.Error _ -> arbitrary
