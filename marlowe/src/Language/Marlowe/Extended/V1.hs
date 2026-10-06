{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | = Extended Marlowe: adds templating functionality to Marlowe language
-- Extended Marlowe is not executable, it is translated to core Marlowe before
-- execution, deployment, or analysis, through the process of instantiation.
-- The purpose of Extended Marlowe is to allow Marlowe contracts to be reusable
-- in different situations without cluttering the code that goes on-chain
-- (core Marlowe).
module Language.Marlowe.Extended.V1 (
  module Language.Marlowe.Extended.V1,
  module Language.Marlowe.Pretty,
  S.AccountId,
  S.Bound (..),
  S.ChoiceId (..),
  S.ChoiceName,
  S.ChosenNum,
  S.Party (..),
  S.TimeInterval,
  S.Token (..),
  S.ValueId (..),
  ToCore (..),
  S.ada,
  adaSymbol,
  adaToken,
  (%),
) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as JSON
import Data.Aeson.Types hiding (Error, Value)
import Data.ByteString.Lazy.Char8 as C (putStr)
import qualified Data.Foldable as F
import Data.Ratio ((%))
import Data.Text (pack)
import GHC.Generics
import qualified Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1.Metadata.Types (MetaData)
import Language.Marlowe.ParserUtil (getInteger, withInteger)
import Language.Marlowe.Pretty (Pretty (..), pretty)
import PlutusLedgerApi.V2 (adaSymbol, adaToken)
import qualified PlutusLedgerApi.V2 as L
import Text.PrettyPrint.Leijen (parens, text)

data Timeout
  = TimeParam String
  | POSIXTime Integer
  deriving stock (Show, Generic, Eq)

instance Pretty Timeout where
  prettyFragment (POSIXTime n) = prettyFragment n
  prettyFragment sp@(TimeParam _) = parens $ text $ show sp

instance Num Timeout where
  (+) (POSIXTime a) (POSIXTime b) = POSIXTime (a + b)
  (+) _ _ = error "Tried to add with templates"
  (-) (POSIXTime a) (POSIXTime b) = POSIXTime (a - b)
  (-) _ _ = error "Tried to subtract with templates"
  (*) (POSIXTime a) (POSIXTime b) = POSIXTime (a * b)
  (*) _ _ = error "Tried to multiplate with templates"
  abs (POSIXTime a) = POSIXTime (abs a)
  abs _ = error "Tried to calculate absolute value of template"
  signum (POSIXTime a) = POSIXTime (signum a)
  signum _ = error "Tried to calculate signum of template"
  negate (POSIXTime x) = POSIXTime (-x)
  negate _ = error "Tried to negate a template"
  fromInteger = POSIXTime

instance Pretty Rational where
  prettyFragment r = text $ "(" ++ show r ++ ")"

data Value
  = AvailableMoney S.AccountId S.Token
  | Constant Integer
  | ConstantParam String
  | NegValue Value
  | AddValue Value Value
  | SubValue Value Value
  | MulValue Value Value
  | DivValue Value Value
  | ChoiceValue S.ChoiceId
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue S.ValueId
  | Cond Observation Value Value
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething S.ChoiceId
  | ValueGE Value Value
  | ValueGT Value Value
  | ValueLT Value Value
  | ValueLE Value Value
  | ValueEQ Value Value
  | TrueObs
  | FalseObs
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

data Action
  = Deposit S.AccountId S.Party S.Token Value
  | Choice S.ChoiceId [S.Bound]
  | Notify Observation
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

data Payee
  = Account S.AccountId
  | Party S.Party
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

data Case = Case Action Contract
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

data Contract
  = Close
  | Pay S.AccountId Payee S.Token Value Contract
  | If Observation Contract Contract
  | When [Case] Timeout Contract
  | Let S.ValueId Value Contract
  | Assert Observation Contract
  deriving stock (Show, Generic, Eq)
  deriving anyclass (Pretty)

-- | A module is a way to package a contract with it's metadata. Eventually
--     this type could include imports and exports (reason behind the name).
--     We don't include the version number in the datatype because it is implicit
--     by the package name (`Language.Marlowe.Extended.V1`) and explicit by the
--     JSON serialization.
data Module = Module
  { metadata :: MetaData
  -- ^ The name and information covered by this field is under discussion
  --   on the following thread:
  --   https://github.com/input-output-hk/MIPs/discussions/7
  --   Version 1 of Marlowe Extended shouldn't be closed until the discussion
  --   is resolved.
  , contract :: Contract
  -- ^ Currently we have a single entrypoint for the full contract, but we could
  --   extend this to be a `OMap Identifier Contract` to enable functions.
  --   In order to guarantee that we can convert to Marlowe Core we would need to
  --   validate that a function can only call functions defined above (if bottom-up)
  --   and that they cannot call themselves
  }
  deriving stock (Show, Eq)

class ToCore a b where
  toCore :: a -> Maybe b

instance ToCore Contract S.Contract where
  toCore Close = Just S.Close
  toCore (Pay accId payee tok val cont) = pure (S.Pay accId) <*> toCore payee <*> pure tok <*> toCore val <*> toCore cont
  toCore (If obs cont1 cont2) = S.If <$> toCore obs <*> toCore cont1 <*> toCore cont2
  toCore (When cases tim cont) = S.When <$> traverse toCore cases <*> toCore tim <*> toCore cont
  toCore (Let varId val cont) = pure (S.Let varId) <*> toCore val <*> toCore cont
  toCore (Assert obs cont) = S.Assert <$> toCore obs <*> toCore cont

instance ToCore Value (S.Value S.Observation) where
  toCore (Constant c) = Just $ S.Constant c
  toCore (ConstantParam _) = Nothing
  toCore (AvailableMoney accId tok) = Just $ S.AvailableMoney accId tok
  toCore (NegValue v) = S.NegValue <$> toCore v
  toCore (AddValue lhs rhs) = S.AddValue <$> toCore lhs <*> toCore rhs
  toCore (SubValue lhs rhs) = S.SubValue <$> toCore lhs <*> toCore rhs
  toCore (MulValue lhs rhs) = S.MulValue <$> toCore lhs <*> toCore rhs
  toCore (DivValue lhs rhs) = S.DivValue <$> toCore lhs <*> toCore rhs
  toCore (ChoiceValue choId) = Just $ S.ChoiceValue choId
  toCore TimeIntervalStart = Just S.TimeIntervalStart
  toCore TimeIntervalEnd = Just S.TimeIntervalEnd
  toCore (UseValue vId) = Just $ S.UseValue vId
  toCore (Cond obs lhs rhs) = S.Cond <$> toCore obs <*> toCore lhs <*> toCore rhs

instance ToCore Observation S.Observation where
  toCore (AndObs lhs rhs) = S.AndObs <$> toCore lhs <*> toCore rhs
  toCore (OrObs lhs rhs) = S.OrObs <$> toCore lhs <*> toCore rhs
  toCore (NotObs v) = S.NotObs <$> toCore v
  toCore (ChoseSomething choId) = Just $ S.ChoseSomething choId
  toCore (ValueGE lhs rhs) = S.ValueGE <$> toCore lhs <*> toCore rhs
  toCore (ValueGT lhs rhs) = S.ValueGT <$> toCore lhs <*> toCore rhs
  toCore (ValueLT lhs rhs) = S.ValueLT <$> toCore lhs <*> toCore rhs
  toCore (ValueLE lhs rhs) = S.ValueLE <$> toCore lhs <*> toCore rhs
  toCore (ValueEQ lhs rhs) = S.ValueEQ <$> toCore lhs <*> toCore rhs
  toCore TrueObs = Just S.TrueObs
  toCore FalseObs = Just S.FalseObs

instance ToCore Action S.Action where
  toCore (Deposit accId party tok val) = pure (S.Deposit accId party tok) <*> toCore val
  toCore (Choice choId bounds) = Just $ S.Choice choId bounds
  toCore (Notify obs) = S.Notify <$> toCore obs

instance ToCore Timeout L.POSIXTime where
  toCore (TimeParam _) = Nothing
  toCore (POSIXTime x) = Just (L.POSIXTime x)

instance ToCore Payee S.Payee where
  toCore (Account accId) = Just $ S.Account accId
  toCore (Party roleName) = Just $ S.Party roleName

instance ToCore Case (S.Case S.Contract) where
  toCore (Case act c) = S.Case <$> toCore act <*> toCore c

instance FromJSON Value where
  parseJSON (Object v) =
    ( AvailableMoney
        <$> (v .: "in_account")
        <*> (v .: "amount_of_token")
    )
      <|> (NegValue <$> (v .: "negate"))
      <|> ( AddValue
              <$> (v .: "add")
              <*> (v .: "and")
          )
      <|> ( SubValue
              <$> (v .: "value")
              <*> (v .: "minus")
          )
      <|> ( MulValue
              <$> (v .: "multiply")
              <*> (v .: "times")
          )
      <|> (DivValue <$> (v .: "divide") <*> (v .: "by"))
      <|> (ChoiceValue <$> (v .: "value_of_choice"))
      <|> (UseValue <$> (v .: "use_value"))
      <|> ( Cond
              <$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else")
          )
      <|> (ConstantParam <$> (v .: "constant_param"))
  parseJSON (String "time_interval_start") = return TimeIntervalStart
  parseJSON (String "time_interval_end") = return TimeIntervalEnd
  parseJSON (Number n) = Constant <$> getInteger "constant value" n
  parseJSON _ = fail "Value must be either an object or an integer"
instance ToJSON Value where
  toJSON (AvailableMoney accountId token) =
    object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
  toJSON (ConstantParam x) =
    object
      ["constant_param" .= x]
  toJSON (NegValue x) =
    object
      ["negate" .= x]
  toJSON (AddValue lhs rhs) =
    object
      [ "add" .= lhs
      , "and" .= rhs
      ]
  toJSON (SubValue lhs rhs) =
    object
      [ "value" .= lhs
      , "minus" .= rhs
      ]
  toJSON (MulValue lhs rhs) =
    object
      [ "multiply" .= lhs
      , "times" .= rhs
      ]
  toJSON (DivValue lhs rhs) =
    object
      [ "divide" .= lhs
      , "by" .= rhs
      ]
  toJSON (ChoiceValue choiceId) =
    object
      ["value_of_choice" .= choiceId]
  toJSON TimeIntervalStart = JSON.String $ pack "time_interval_start"
  toJSON TimeIntervalEnd = JSON.String $ pack "time_interval_end"
  toJSON (UseValue valueId) =
    object
      ["use_value" .= valueId]
  toJSON (Cond obs tv ev) =
    object
      [ "if" .= obs
      , "then" .= tv
      , "else" .= ev
      ]

instance FromJSON Observation where
  parseJSON (Bool True) = return TrueObs
  parseJSON (Bool False) = return FalseObs
  parseJSON (Object v) =
    ( AndObs
        <$> (v .: "both")
        <*> (v .: "and")
    )
      <|> ( OrObs
              <$> (v .: "either")
              <*> (v .: "or")
          )
      <|> (NotObs <$> (v .: "not"))
      <|> (ChoseSomething <$> (v .: "chose_something_for"))
      <|> ( ValueGE
              <$> (v .: "value")
              <*> (v .: "ge_than")
          )
      <|> ( ValueGT
              <$> (v .: "value")
              <*> (v .: "gt")
          )
      <|> ( ValueLT
              <$> (v .: "value")
              <*> (v .: "lt")
          )
      <|> ( ValueLE
              <$> (v .: "value")
              <*> (v .: "le_than")
          )
      <|> ( ValueEQ
              <$> (v .: "value")
              <*> (v .: "equal_to")
          )
  parseJSON _ = fail "Observation must be either an object or a boolean"

instance ToJSON Observation where
  toJSON (AndObs lhs rhs) =
    object
      [ "both" .= lhs
      , "and" .= rhs
      ]
  toJSON (OrObs lhs rhs) =
    object
      [ "either" .= lhs
      , "or" .= rhs
      ]
  toJSON (NotObs v) =
    object
      ["not" .= v]
  toJSON (ChoseSomething choiceId) =
    object
      ["chose_something_for" .= choiceId]
  toJSON (ValueGE lhs rhs) =
    object
      [ "value" .= lhs
      , "ge_than" .= rhs
      ]
  toJSON (ValueGT lhs rhs) =
    object
      [ "value" .= lhs
      , "gt" .= rhs
      ]
  toJSON (ValueLT lhs rhs) =
    object
      [ "value" .= lhs
      , "lt" .= rhs
      ]
  toJSON (ValueLE lhs rhs) =
    object
      [ "value" .= lhs
      , "le_than" .= rhs
      ]
  toJSON (ValueEQ lhs rhs) =
    object
      [ "value" .= lhs
      , "equal_to" .= rhs
      ]
  toJSON TrueObs = toJSON True
  toJSON FalseObs = toJSON False

instance FromJSON Action where
  parseJSON =
    withObject
      "Action"
      ( \v ->
          ( Deposit
              <$> (v .: "into_account")
              <*> (v .: "party")
              <*> (v .: "of_token")
              <*> (v .: "deposits")
          )
            <|> ( Choice
                    <$> (v .: "for_choice")
                    <*> ( (v .: "choose_between")
                            >>= withArray
                              "Bound list"
                              ( \bl ->
                                  mapM parseJSON (F.toList bl)
                              )
                        )
                )
            <|> (Notify <$> (v .: "notify_if"))
      )
instance ToJSON Action where
  toJSON (Deposit accountId party token val) =
    object
      [ "into_account" .= accountId
      , "party" .= party
      , "of_token" .= token
      , "deposits" .= val
      ]
  toJSON (Choice choiceId bounds) =
    object
      [ "for_choice" .= choiceId
      , "choose_between" .= toJSONList (map toJSON bounds)
      ]
  toJSON (Notify obs) =
    object
      ["notify_if" .= obs]

instance FromJSON Case where
  parseJSON =
    withObject
      "Case"
      ( \v ->
          Case
            <$> (v .: "case")
            <*> (v .: "then")
      )
instance ToJSON Case where
  toJSON (Case act cont) =
    object
      [ "case" .= act
      , "then" .= cont
      ]

instance FromJSON Payee where
  parseJSON =
    withObject
      "Payee"
      ( \v ->
          (Account <$> (v .: "account"))
            <|> (Party <$> (v .: "party"))
      )

instance ToJSON Payee where
  toJSON (Account acc) = object ["account" .= acc]
  toJSON (Party party) = object ["party" .= party]

instance FromJSON Contract where
  parseJSON (String "close") = return Close
  parseJSON (Object v) =
    ( Pay
        <$> (v .: "from_account")
        <*> (v .: "to")
        <*> (v .: "token")
        <*> (v .: "pay")
        <*> (v .: "then")
    )
      <|> ( If
              <$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else")
          )
      <|> ( When
              <$> ( (v .: "when")
                      >>= withArray
                        "Case list"
                        ( \cl ->
                            mapM parseJSON (F.toList cl)
                        )
                  )
              <*> (v .: "timeout")
              <*> (v .: "timeout_continuation")
          )
      <|> ( Let
              <$> (v .: "let")
              <*> (v .: "be")
              <*> (v .: "then")
          )
      <|> ( Assert
              <$> (v .: "assert")
              <*> (v .: "then")
          )
  parseJSON _ = fail "Contract must be either an object or a the string \"close\""

instance ToJSON Contract where
  toJSON Close = JSON.String $ pack "close"
  toJSON (Pay accountId payee token value cont) =
    object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= cont
      ]
  toJSON (If obs cont1 cont2) =
    object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When caseList timeout cont) =
    object
      [ "when" .= toJSONList (map toJSON caseList)
      , "timeout" .= toJSONTimeout timeout
      , "timeout_continuation" .= cont
      ]
  toJSON (Let valId value cont) =
    object
      [ "let" .= valId
      , "be" .= value
      , "then" .= cont
      ]
  toJSON (Assert obs cont) =
    object
      [ "assert" .= obs
      , "then" .= cont
      ]

instance FromJSON Module where
  parseJSON =
    withObject
      "Module"
      ( \v -> do
          version <- withInteger "module version" =<< (v .: "me_version")
          if version /= 1
            then fail $ "Module version mismatch, expected version 1, got " ++ show version
            else do
              cont <- v .: "contract"
              meta <- v .: "metadata"
              return $ Module{contract = cont, metadata = meta}
      )

instance FromJSON Timeout where
  parseJSON (Number n) = POSIXTime <$> getInteger "timeout" n
  parseJSON (Object v) = TimeParam <$> (v .: "time_param")
  parseJSON _ = fail "Timeout must be an integer or an object with a time_param key"

toJSONTimeout :: Timeout -> JSON.Value
toJSONTimeout (POSIXTime t) = toJSON t
toJSONTimeout (TimeParam p) = object ["time_param" .= p]

printJSON :: Contract -> IO ()
printJSON = C.putStr . JSON.encode
