-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | Shared functions for Marlowe testing.
module Spec.Marlowe.Common (
  -- * Types
  MarloweScenario (..),
  PubKey (..),

  -- * Generating
  actionGen,
  actionGenSized,
  amount,
  boundListGen,
  caseRelGenSized,
  choiceIdGen,
  contractGen,
  contractGenSized,
  contractRelGenSized,
  listLengthGen,
  observationGen,
  observationGenSized,
  partyGen,
  payeeGen,
  positiveAmount,
  rationalGen,
  simpleIntegerGen,
  tokenGen,
  valueGen,
  valueGenSized,
  valueIdGen,

  -- * Shrinking
  shrinkAction,
  shrinkCase,
  shrinkChoiceId,
  shrinkContract,
  shrinkObservation,
  shrinkPOSIXTime,
  shrinkParty,
  shrinkPayee,
  shrinkSimpleInteger,
  shrinkToken,
  shrinkValue,
  shrinkValueId,

  -- * Instances
  alicePk,
  pangramContract,
) where

import Data.Map.Strict (Map)
import Data.Ratio (Ratio)
import Data.String (fromString)
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (..),
  Bound (..),
  Case (..),
  ChoiceId (..),
  Contract (..),
  Observation (..),
  Party (..),
  Payee (..),
  Token (..),
  Value (..),
  ValueId (..),
  mkChoiceIdUtf8,
  mkRoleUtf8,
  mkTokenNameUtf8,
  unsafeMkCurrencySymbolHex,
  unsafeMkTokenNameHex,
  pattern RoleUtf8,
 )
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Util (merkleizedCase)
import Test.QuickCheck (Gen, choose, frequency, oneof, shrinkList, sized, vectorOf)

import qualified Language.Marlowe.Extended.V1 as Extended
import qualified PlutusLedgerApi.V2 as Ledger

-- | A public key represented as a string.
newtype PubKey = PubKey String

-- | A scenario for Marlowe testing.
newtype MarloweScenario = MarloweScenario {mlInitialBalances :: Map PubKey Ledger.Value}

-- | Generate a small integer amount at random.
amount :: Gen Integer
amount = choose (-100, 100)

-- | Generate a small positive integer amount at random.
positiveAmount :: Gen Integer
positiveAmount = choose (1, 100)

-- | Generate one of three parties at random.
partyGen :: Gen Party
partyGen =
  oneof
    [ return $ fromString "alice"
    , return $ fromString "bob"
    , return $ fromString "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
    ]

-- | Shrink a generated party.
shrinkParty :: Party -> [Party]
shrinkParty party = case party of
  Address _ _ -> [mkRoleUtf8 "alice", mkRoleUtf8 "bob"]
  RoleUtf8 "bob" -> [mkRoleUtf8 "alice"]
  RoleUtf8 "alice" -> []
  _ -> []

-- | Generate a payee at random.
payeeGen :: Gen Payee
payeeGen =
  oneof
    [ Account <$> partyGen
    , Party <$> partyGen
    ]

-- | Shrink a generated payee.
shrinkPayee :: Payee -> [Payee]
shrinkPayee (Account accId) = [Account x | x <- shrinkParty accId]
shrinkPayee (Party party) = [Party x | x <- shrinkParty party]

-- | Generate one of two tokens at random.
tokenGen :: Gen Token
tokenGen =
  oneof
    [ return $ ada
    , return $
        Token
          (unsafeMkCurrencySymbolHex "424954")
          (unsafeMkTokenNameHex "434f494e")
    ]

-- | Shrink a generated token.
shrinkToken :: Token -> [Token]
shrinkToken token
  | token == ada = []
  | otherwise = [ada]

-- | Generate at random an integer with one of a few values.
simpleIntegerGen :: Gen Integer
simpleIntegerGen =
  frequency
    [ (1, return (-100))
    , (1, return (-1))
    , (2, return 0)
    , (64, return 1)
    , (32, return 2)
    , (16, return 4)
    , (8, return 8)
    , (4, return 16)
    , (2, return 32)
    ]

-- | Shrink a simply generated integer.
shrinkSimpleInteger :: Integer -> [Integer]
shrinkSimpleInteger 0 = []
shrinkSimpleInteger v = [0, v `quot` 2]

-- | Shrink a POSIX time.
shrinkPOSIXTime :: Ledger.POSIXTime -> [Ledger.POSIXTime]
shrinkPOSIXTime (Ledger.POSIXTime t) = map Ledger.POSIXTime (shrinkSimpleInteger t)

-- | Generate a choice identifier at random.
choiceIdGen :: Gen ChoiceId
choiceIdGen = do
  choName <-
    oneof
      [ return "first"
      , return "second"
      ]
  chooser <- partyGen
  return $ ChoiceId choName chooser

-- | Shrink a generated choice identifieer.
shrinkChoiceId :: ChoiceId -> [ChoiceId]
shrinkChoiceId (ChoiceId "second" chooser) =
  ChoiceId "first" chooser
    : [ChoiceId "second" x | x <- shrinkParty chooser]
shrinkChoiceId (ChoiceId "first" chooser) = [ChoiceId "first" x | x <- shrinkParty chooser]
shrinkChoiceId _ = []

-- | Generate at random one of two values.
valueIdGen :: Gen ValueId
valueIdGen =
  oneof
    [ return "alpha"
    , return "beta"
    ]

-- | Shrink a generated value.
shrinkValueId :: ValueId -> [ValueId]
shrinkValueId "beta" = ["alpha"]
shrinkValueId "alpha" = []
shrinkValueId _ = []

-- | Generate a ratio at random.
rationalGen :: Gen (Ratio Integer)
rationalGen = do
  a <- simpleIntegerGen
  b <- positiveAmount
  return $ a Extended.% b

-- | Generate a value at random.
valueGenSized
  :: Int
  -- ^ The maximum size (complexity) of the value.
  -> Gen (Value Observation)
  -- ^ Generator for a value.
valueGenSized s
  | s > 0 =
      oneof
        [ AvailableMoney <$> partyGen <*> tokenGen
        , Constant <$> simpleIntegerGen
        , NegValue <$> valueGenSized (s - 1)
        , AddValue <$> valueGenSized (s `quot` 2) <*> valueGenSized (s `quot` 2)
        , SubValue <$> valueGenSized (s `quot` 2) <*> valueGenSized (s `quot` 2)
        , MulValue <$> valueGenSized (s `quot` 2) <*> valueGenSized (s `quot` 2)
        , DivValue <$> valueGenSized (s `quot` 2) <*> valueGenSized (s `quot` 2)
        , ChoiceValue <$> choiceIdGen
        , Cond
            <$> observationGenSized (s `quot` 3)
            <*> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , return TimeIntervalStart
        , return TimeIntervalEnd
        , UseValue <$> valueIdGen
        ]
  | otherwise =
      oneof
        [ AvailableMoney <$> partyGen <*> tokenGen
        , Constant <$> simpleIntegerGen
        , return TimeIntervalStart
        , return TimeIntervalEnd
        , UseValue <$> valueIdGen
        ]

-- | Generate a value at random.
valueGen :: Gen (Value Observation)
valueGen = sized valueGenSized

-- | Shrink a generated value.
shrinkValue :: Value Observation -> [Value Observation]
shrinkValue value = case value of
  Constant x -> [Constant y | y <- shrinkSimpleInteger x]
  TimeIntervalStart -> [Constant 0]
  TimeIntervalEnd -> [Constant 0, TimeIntervalStart]
  AvailableMoney accId tok ->
    Constant 0
      : ( [AvailableMoney x tok | x <- shrinkParty accId]
            ++ [AvailableMoney accId y | y <- shrinkToken tok]
        )
  UseValue valId -> Constant 0 : [UseValue x | x <- shrinkValueId valId]
  ChoiceValue choId -> Constant 0 : [ChoiceValue x | x <- shrinkChoiceId choId]
  NegValue val -> Constant 0 : val : [NegValue x | x <- shrinkValue val]
  AddValue val1 val2 ->
    Constant 0
      : val1
      : val2
      : ( [AddValue x val2 | x <- shrinkValue val1]
            ++ [AddValue val1 y | y <- shrinkValue val2]
        )
  SubValue val1 val2 ->
    Constant 0
      : val1
      : val2
      : ( [SubValue x val2 | x <- shrinkValue val1]
            ++ [SubValue val1 y | y <- shrinkValue val2]
        )
  MulValue val1 val2 ->
    Constant 0
      : val1
      : val2
      : ( [MulValue x val2 | x <- shrinkValue val1]
            ++ [MulValue val1 y | y <- shrinkValue val2]
        )
  DivValue val1 val2 ->
    Constant 0
      : val1
      : val2
      : ( [DivValue x val2 | x <- shrinkValue val1]
            ++ [DivValue val1 y | y <- shrinkValue val2]
        )
  Cond b val1 val2 ->
    Constant 0
      : val1
      : val2
      : ( [Cond x val1 val2 | x <- shrinkObservation b]
            ++ [Cond b x val2 | x <- shrinkValue val1]
            ++ [Cond b val1 y | y <- shrinkValue val2]
        )

-- | Generate an observation at random.
observationGenSized
  :: Int
  -- ^ The maximum size (complexity) of the observation.
  -> Gen Observation
  -- ^ Generator for a observation.
observationGenSized s
  | s > 0 =
      oneof
        [ AndObs
            <$> observationGenSized (s `quot` 2)
            <*> observationGenSized (s `quot` 2)
        , OrObs
            <$> observationGenSized (s `quot` 2)
            <*> observationGenSized (s `quot` 2)
        , NotObs <$> observationGenSized (s - 1)
        , ChoseSomething <$> choiceIdGen
        , ValueGE
            <$> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , ValueGT
            <$> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , ValueLT
            <$> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , ValueLE
            <$> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , ValueEQ
            <$> valueGenSized (s `quot` 2)
            <*> valueGenSized (s `quot` 2)
        , return TrueObs
        , return FalseObs
        ]
  | otherwise =
      oneof
        [ ChoseSomething <$> choiceIdGen
        , return TrueObs
        , return FalseObs
        ]

-- | Generate an observation at random.
observationGen :: Gen Observation
observationGen = sized observationGenSized

-- | Shrink a generated observation.
shrinkObservation :: Observation -> [Observation]
shrinkObservation obs = case obs of
  FalseObs -> []
  TrueObs -> [FalseObs]
  ChoseSomething choId -> FalseObs : TrueObs : [ChoseSomething x | x <- shrinkChoiceId choId]
  ValueGE lhs rhs ->
    FalseObs
      : TrueObs
      : ( [ValueGE x rhs | x <- shrinkValue lhs]
            ++ [ValueGE lhs y | y <- shrinkValue rhs]
        )
  ValueGT lhs rhs ->
    FalseObs
      : TrueObs
      : ( [ValueGT x rhs | x <- shrinkValue lhs]
            ++ [ValueGT lhs y | y <- shrinkValue rhs]
        )
  ValueLT lhs rhs ->
    FalseObs
      : TrueObs
      : ( [ValueLT x rhs | x <- shrinkValue lhs]
            ++ [ValueLT lhs y | y <- shrinkValue rhs]
        )
  ValueLE lhs rhs ->
    FalseObs
      : TrueObs
      : ( [ValueLE x rhs | x <- shrinkValue lhs]
            ++ [ValueLE lhs y | y <- shrinkValue rhs]
        )
  ValueEQ lhs rhs ->
    FalseObs
      : TrueObs
      : ( [ValueEQ x rhs | x <- shrinkValue lhs]
            ++ [ValueEQ lhs y | y <- shrinkValue rhs]
        )
  AndObs lhs rhs ->
    FalseObs
      : TrueObs
      : lhs
      : rhs
      : ( [AndObs x rhs | x <- shrinkObservation lhs]
            ++ [AndObs lhs y | y <- shrinkObservation rhs]
        )
  OrObs lhs rhs ->
    FalseObs
      : TrueObs
      : lhs
      : rhs
      : ( [OrObs x rhs | x <- shrinkObservation lhs]
            ++ [OrObs lhs y | y <- shrinkObservation rhs]
        )
  NotObs subObs -> FalseObs : TrueObs : subObs : [NotObs x | x <- shrinkObservation subObs]

-- | Generate a short list of integers at random.
listLengthGen :: Gen Int
listLengthGen =
  frequency
    [ (1, return 0)
    , (8, return 1)
    , (4, return 2)
    , (1, return 3)
    ]

-- | Generate a list of bounds at random.
boundListGenAux
  :: Int
  -- ^ The size of the list.
  -> Integer
  -- ^ The minimum lower bound.
  -> Gen [Bound]
  -- ^ Generator for a bounds.
boundListGenAux s lb
  | s > 0 = do
      inc1 <- simpleIntegerGen
      inc2 <- simpleIntegerGen
      let tlb = lb + inc1
          thb = tlb + inc2
      rest <- boundListGenAux (s - 1) thb
      return (Bound tlb thb : rest)
  | otherwise = return []

-- | Generate a list of bounds at random.
boundListGen :: Gen [Bound]
boundListGen = do
  len <- listLengthGen
  firstBound <- simpleIntegerGen
  boundListGenAux len firstBound

-- | Generate an action at random.
actionGenSized
  :: Int
  -- ^ The maximum size (complexity) of the action.
  -> Gen Action
  -- ^ Generator for a action.
actionGenSized s =
  oneof
    [ Deposit <$> partyGen <*> partyGen <*> tokenGen <*> valueGenSized (s - 1)
    , Choice <$> choiceIdGen <*> boundListGen
    , Notify <$> observationGenSized (s - 1)
    ]

-- | Generate an action at random.
actionGen :: Gen Action
actionGen = sized actionGenSized

-- | Shrink a generated action.
shrinkAction :: Action -> [Action]
shrinkAction action = case action of
  Deposit accId party tok val ->
    Notify FalseObs
      : [Deposit accId party tok v | v <- shrinkValue val]
      ++ [Deposit x party tok val | x <- shrinkParty accId]
      ++ [Deposit accId y tok val | y <- shrinkParty party]
      ++ [Deposit accId party z val | z <- shrinkToken tok]
  Choice choId boundList ->
    Notify FalseObs
      : [Choice choId b | b <- shrinkList (const []) boundList]
      ++ [Choice x boundList | x <- shrinkChoiceId choId]
  Notify obs -> [Notify x | x <- shrinkObservation obs]

-- | Generate a case at random.
caseRelGenSized
  :: Int
  -- ^ The maximum size (complexity) of the case.
  -> Integer
  -- ^ The minimum timeout for continuations.
  -> Gen (Case Contract)
  -- ^ Generator for a contract.
caseRelGenSized s bn =
  frequency
    [ (9, Case <$> actionGenSized s <*> contractRelGenSized s bn)
    , (1, merkleizedCase <$> actionGenSized s <*> contractRelGenSized s bn)
    ]

-- | Shrink a generated case.
shrinkCase :: Case Contract -> [Case Contract]
shrinkCase (Case act cont) =
  [Case act x | x <- shrinkContract cont]
    ++ [Case y cont | y <- shrinkAction act]
shrinkCase (MerkleizedCase act bs) = [MerkleizedCase y bs | y <- shrinkAction act]

-- | Generate a contract at random.
contractRelGenSized
  :: Int
  -- ^ The maximum size (complexity) of the contract.
  -> Integer
  -- ^ The minimum timeout for continuations.
  -> Gen Contract
  -- ^ Generator for a contract.
contractRelGenSized s bn
  | s > 0 =
      oneof
        [ return Close
        , Pay
            <$> partyGen
            <*> payeeGen
            <*> tokenGen
            <*> valueGenSized (s `quot` 4)
            <*> contractRelGenSized (s - 1) bn
        , If
            <$> observationGenSized (s `quot` 4)
            <*> contractRelGenSized (s `quot` 3) bn
            <*> contractRelGenSized (s `quot` 3) bn
        , Let <$> valueIdGen <*> valueGenSized (s `quot` 2) <*> contractRelGenSized (s `quot` 2) bn
        , do
            timeOutDelta <- simpleIntegerGen
            numCases <- listLengthGen
            let newTimeout = bn + timeOutDelta
                ns = if numCases > 0 then s `quot` numCases else s - 1
            When
              <$> vectorOf numCases (caseRelGenSized ns bn)
              <*> (return $ Ledger.POSIXTime newTimeout)
              <*> contractRelGenSized ns newTimeout
        , Assert
            <$> observationGenSized (s `quot` 3)
            <*> contractRelGenSized (s `quot` 2) bn
        ]
  | otherwise = return Close

-- | Generate a contract at random.
contractGenSized
  :: Int
  -- ^ The maximum size (complexity) of the contract.
  -> Gen Contract
  -- ^ Generator for a contract.
contractGenSized s = do
  iniBn <- simpleIntegerGen
  contractRelGenSized s iniBn

-- | Generate a contract at random.
contractGen :: Gen Contract
contractGen = sized contractGenSized

-- | Shrink a generated contract.
shrinkContract :: Contract -> [Contract]
shrinkContract cont = case cont of
  Close -> []
  Let vid val cont ->
    Close
      : cont
      : ( [Let vid v cont | v <- shrinkValue val]
            ++ [Let vid val c | c <- shrinkContract cont]
        )
  Pay accId payee tok val cont ->
    Close
      : cont
      : ( [Pay accId payee tok val c | c <- shrinkContract cont]
            ++ [Pay accId payee tok v cont | v <- shrinkValue val]
            ++ [Pay x payee tok val cont | x <- shrinkParty accId]
            ++ [Pay accId y tok val cont | y <- shrinkPayee payee]
            ++ [Pay accId payee z val cont | z <- shrinkToken tok]
        )
  If obs cont1 cont2 ->
    Close
      : cont1
      : cont2
      : ( [If obs x cont2 | x <- shrinkContract cont1]
            ++ [If obs cont1 y | y <- shrinkContract cont2]
            ++ [If z cont1 cont2 | z <- shrinkObservation obs]
        )
  When [] (Ledger.POSIXTime tim) cont ->
    Close
      : cont
      : ( [When [] (Ledger.POSIXTime tim) x | x <- shrinkContract cont]
            ++ [When [] (Ledger.POSIXTime y) cont | y <- shrinkSimpleInteger tim]
        )
  When l (Ledger.POSIXTime tim) cont ->
    Close
      : cont
      : ( [When nl (Ledger.POSIXTime tim) cont | nl <- shrinkList shrinkCase l]
            ++ [When l (Ledger.POSIXTime tim) x | x <- shrinkContract cont]
            ++ [When l (Ledger.POSIXTime y) cont | y <- shrinkSimpleInteger tim]
        )
  Assert obs cont ->
    Close
      : cont
      : ( [Assert x cont | x <- shrinkObservation obs]
            ++ [Assert obs y | y <- shrinkContract cont]
        )

-- | The primary key hash for Alice.
alicePk :: Party
alicePk = fromString "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"

-- | A contract using all Marlowe DSL terms.
pangramContract :: Contract
pangramContract =
  let aliceAcc = alicePk
      bobRole = mkRoleUtf8 "bob"
      constant = Constant 100
      choiceId = mkChoiceIdUtf8 "choice" alicePk
      token = Token (unsafeMkCurrencySymbolHex "10") (mkTokenNameUtf8 "name")
      valueExpr = AddValue constant (SubValue constant (NegValue constant))
   in Assert TrueObs $
        When
          [ Case
              (Deposit aliceAcc alicePk ada valueExpr)
              ( Let
                  (ValueId "x")
                  valueExpr
                  (Pay aliceAcc (Party bobRole) ada (Cond TrueObs (UseValue (ValueId "x")) (UseValue (ValueId "y"))) Close)
              )
          , Case
              (Choice choiceId [Bound 0 1, Bound 10 20])
              ( If
                  (ChoseSomething choiceId `OrObs` (ChoiceValue choiceId `ValueEQ` constant))
                  (Pay aliceAcc (Account aliceAcc) token (DivValue (AvailableMoney aliceAcc token) constant) Close)
                  Close
              )
          , Case (Notify (AndObs (TimeIntervalStart `ValueLT` TimeIntervalEnd) TrueObs)) Close
          ]
          (Ledger.POSIXTime 100)
          Close
