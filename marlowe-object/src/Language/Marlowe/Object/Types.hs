{--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
--}
{-# LANGUAGE TemplateHaskell #-}

-- | Contains the core type definitions for Marlowe Object bundles. An object bundle is an ordered collection of objects
-- identified by labels. Objects correspond to terms found in a Core Marlowe contract, and are identical to their core
-- counterparts except for the fact that objects can substitute an object reference for an inline term. Ordering matters
-- because an object may only use a previously defined label as a reference (i.e. cyclic references are explicitly
-- forbidden). This also forces an object bundle to list leaf terms first and build up composite terms progressively,
-- which makes the format well suited for incremental processing (e.g. contract merkleization).
--
-- The following core terms can be objects: actions, contracts, observations, parties, tokens and values.
--
-- This module defines the types, several useful instances for them, optics, and conversion functions for core terms.
module Language.Marlowe.Object.Types where

import Cardano.Api (
  Address,
  AsType (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  deserialiseFromBech32,
  deserialiseFromRawBytes,
  serialiseToBech32,
 )
import Control.Applicative (empty)
import Control.Lens (Lens', Plated (..), Prism', makeLensesFor, makePrisms, prism', traversal)
import Control.Monad (join)
import Data.Aeson hiding (Object, String, Value)
import qualified Data.Aeson as A hiding (Object)
import Data.Aeson.Applicative (parseObject)
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.Binary (Binary (..), getWord8, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString.Base16.Aeson (EncodeBase16(..))
import qualified Data.ByteString.Char8 as BS8

import Data.Foldable (asum)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as LNE
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
import GHC.Base (Any)
import GHC.Generics (Generic)
import GHC.Read (Read (..), lexP)
import GHC.Show (showSpace)
import qualified Marlowe.Plutus.Semantics.Types as Core
import Marlowe.Plutus.Semantics.Types.Address (serialiseAddressBech32)
import qualified Marlowe.Plutus.Semantics.Types.Address as Core
import PlutusLedgerApi.V2 (BuiltinByteString, POSIXTime (..))
import qualified PlutusLedgerApi.V2 as PV2
import Text.Read (Lexeme (..), ReadPrec, parens, prec, step)
import Unsafe.Coerce (unsafeCoerce)
import Data.Variations (Variations(..), varyAp)
import Cardano.Api.Address (ShelleyAddr)

-- | A newtype wrapper for a ByteString which is parsed and rendered as an ascii string.
newtype Verbatim = Verbatim {unVerbatim :: ByteString}
  deriving newtype (Eq, Ord, Show, Variations)

instance ToJSON Verbatim where
  toJSON = A.String . T.pack . BS8.unpack . unVerbatim

instance ToJSONKey Verbatim where
  toJSONKey = toJSONKeyText $ T.pack . BS8.unpack . unVerbatim

instance FromJSON Verbatim where
  parseJSON = withText "Verbatim" $ pure . Verbatim . BS8.pack . T.unpack

instance FromJSONKey Verbatim where
  fromJSONKey = FromJSONKeyText $ Verbatim . BS8.pack . T.unpack

-- | A label that refers to an object in a bundle.
newtype Label = Label {unLabel :: Text}
  deriving (Generic, Eq, Ord)
  deriving newtype (Show, Binary, Variations, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | A timeout in a contract.
newtype Timeout = Timeout {unTimeout :: UTCTime}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (Variations)

instance Num Timeout where
  (+) =
    fmap (Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime)
      . on (+) (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout)
  (*) =
    fmap (Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime)
      . on (*) (nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout)
  abs =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . abs
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout
  signum =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . signum
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout
  fromInteger = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . fromInteger
  negate =
    Timeout
      . posixSecondsToUTCTime
      . secondsToNominalDiffTime
      . negate
      . nominalDiffTimeToSeconds
      . utcTimeToPOSIXSeconds
      . unTimeout

instance Binary Timeout where
  put = put @Integer . floor . (* 1000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout
  get = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger <$> get

instance ToJSON Timeout where
  toJSON = toJSON @Integer . floor . (* 1000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout

instance FromJSON Timeout where
  parseJSON v = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger <$> parseJSON v

-- | Convert a timeout to its core representation.
toCoreTimeout :: Timeout -> Core.Timeout
toCoreTimeout = PV2.POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . unTimeout

-- | Convert a core timeout to its object representation.
fromCoreTimeout :: Core.Timeout -> Timeout
fromCoreTimeout = Timeout . posixSecondsToUTCTime . secondsToNominalDiffTime . (/ 1000) . fromInteger . getPOSIXTime

-- | A case in a When contract, consisting of an action and a continuation contract (either an inline contract or a hash
-- in the case of a merkleized contract).
data Case
  = -- | A case with an inline continuation
    Case Action Contract
  | -- | A case with a hashed continuation
    MerkleizedCase Action ContractHash
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary)

-- | The immediate child cases of a case are all cases in its continuation which don't have another case between them
-- (can descend through arbitrarily many levels of contract constructors).
instance Plated Case where
  plate = traversal \focus -> \case
    Case a c -> Case a <$> visitContractCases focus c
    MerkleizedCase a h -> pure $ MerkleizedCase a h

instance ToJSON Case where
  toJSON (Case act cont) =
    A.object
      [ "case" .= act
      , "then" .= cont
      ]
  toJSON (MerkleizedCase act hash) =
    A.object
      [ "case" .= act
      , "merkleized_then" .= hash
      ]

instance FromJSON Case where
  parseJSON =
    parseObject "Case" $
      asum
        [ Case <$> "case" <*> "then"
        , MerkleizedCase <$> "case" <*> "merkleized_then"
        ]

-- | Convert a core case to its object representation.
fromCoreCase :: Core.Case Core.Contract -> Case
fromCoreCase = \case
  Core.Case action contract ->
    Case (fromCoreAction action) (fromCoreContract contract)
  Core.MerkleizedCase action hash ->
    MerkleizedCase (fromCoreAction action) (fromCoreContractHash hash)

-- | An action that can be matched by applying an input to a contract.
data Action
  = -- | An action matched by a deposit input for the given parameters.
    Deposit AccountId Party Token Value
  | -- | An action matched by a choice input for the given parameters.
    Choice ChoiceId [Bound]
  | -- | An action matched by a notify input if the observation evaluates to true.
    Notify Observation
  | -- | A reference to a previously defined action.
    ActionRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Action where
  toJSON (Deposit accountId party token val) =
    A.object
      [ "into_account" .= accountId
      , "party" .= party
      , "of_token" .= token
      , "deposits" .= val
      ]
  toJSON (Choice choiceId bounds) =
    A.object
      [ "for_choice" .= choiceId
      , "choose_between" .= bounds
      ]
  toJSON (Notify obs) = A.object ["notify_if" .= obs]
  toJSON (ActionRef label) = A.object ["ref" .= label]

instance FromJSON Action where
  parseJSON =
    parseObject "Action" $
      asum
        [ Deposit <$> "into_account" <*> "party" <*> "of_token" <*> "deposits"
        , Choice <$> "for_choice" <*> "choose_between"
        , Notify <$> "notify_if"
        , ActionRef <$> "ref"
        ]

-- | Convert a core action to its object representation.
fromCoreAction :: Core.Action -> Action
fromCoreAction = \case
  Core.Deposit accountId party token val ->
    Deposit (fromCoreParty accountId) (fromCoreParty party) (fromCoreToken token) (fromCoreValue val)
  Core.Choice choiceId bounds ->
    Choice (fromCoreChoiceId choiceId) (fromCoreBound <$> bounds)
  Core.Notify obs ->
    Notify (fromCoreObservation obs)

-- | An integer quantity that depends on the state of the contract and the current time interval.
data Value
  = -- | The amount of a given token currently in an account.
    AvailableMoney AccountId Token
  | -- | A constant value.
    Constant Integer
  | -- | Negate a value.
    NegValue Value
  | -- | Add two values.
    AddValue Value Value
  | -- | Subtract the second value from the first.
    SubValue Value Value
  | -- | Multiply two values.
    MulValue Value Value
  | -- | Divide two values and discard the remainder. Division by zero is arbitrarily defined as zero.
    DivValue Value Value
  | -- | The value that has been chosen for a certain choice ID. Defaults to zero.
    ChoiceValue ChoiceId
  | -- | The start of the current transaction's time interval, in POSIX milliseconds.
    TimeIntervalStart
  | -- | The end of the current transaction's time interval, in POSIX milliseconds
    TimeIntervalEnd
  | -- | Use a value that was previously defined in a Let-contract. Defaults to zero
    UseValue ValueId
  | -- | Choses the first value if the condition evaluates to true, the second one otherwise.
    Cond Observation Value Value
  | -- | A reference to a previously defined value.
    ValueRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary)

-- | The immediate children of a value are any value sub-terms. Note that observation and value are mutually recursive,
-- so observations also must be traversed until a value is reached.
instance Plated Value where
  plate = traversal \focus -> \case
    AvailableMoney a b -> pure $ AvailableMoney a b
    AddValue a b -> on (liftA2 AddValue) focus a b
    SubValue a b -> on (liftA2 SubValue) focus a b
    MulValue a b -> on (liftA2 MulValue) focus a b
    DivValue a b -> on (liftA2 DivValue) focus a b
    NegValue a -> NegValue <$> focus a
    Constant a -> pure $ Constant a
    ChoiceValue a -> pure $ ChoiceValue a
    UseValue a -> pure $ UseValue a
    TimeIntervalStart -> pure TimeIntervalStart
    TimeIntervalEnd -> pure TimeIntervalEnd
    Cond a b c -> Cond <$> visitObservationValues focus a <*> focus b <*> focus c
    ValueRef l -> pure $ ValueRef l

-- | Visit all the observations in a value to a depth of one (i.e. don't descend into sub-terms of observations).
visitValueObservations :: (Applicative f) => (Observation -> f Observation) -> Value -> f Value
visitValueObservations f = \case
  AddValue a b -> on (liftA2 AddValue) (visitValueObservations f) a b
  SubValue a b -> on (liftA2 SubValue) (visitValueObservations f) a b
  MulValue a b -> on (liftA2 MulValue) (visitValueObservations f) a b
  DivValue a b -> on (liftA2 DivValue) (visitValueObservations f) a b
  NegValue a -> NegValue <$> visitValueObservations f a
  Cond a b c -> Cond <$> f a <*> visitValueObservations f b <*> visitValueObservations f c
  v -> pure v

instance ToJSON Value where
  toJSON (AvailableMoney accountId token) =
    A.object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
  toJSON (NegValue x) =
    A.object
      ["negate" .= x]
  toJSON (AddValue lhs rhs) =
    A.object
      [ "add" .= lhs
      , "and" .= rhs
      ]
  toJSON (SubValue lhs rhs) =
    A.object
      [ "value" .= lhs
      , "minus" .= rhs
      ]
  toJSON (MulValue lhs rhs) =
    A.object
      [ "multiply" .= lhs
      , "times" .= rhs
      ]
  toJSON (DivValue lhs rhs) =
    A.object
      [ "divide" .= lhs
      , "by" .= rhs
      ]
  toJSON (ChoiceValue choiceId) =
    A.object
      ["value_of_choice" .= choiceId]
  toJSON TimeIntervalStart = "time_interval_start"
  toJSON TimeIntervalEnd = "time_interval_end"
  toJSON (UseValue valueId) =
    A.object
      ["use_value" .= valueId]
  toJSON (Cond obs tv ev) =
    A.object
      [ "if" .= obs
      , "then" .= tv
      , "else" .= ev
      ]
  toJSON (ValueRef label) = A.object ["ref" .= label]

instance FromJSON Value where
  parseJSON = \case
    A.Number n -> Constant <$> parseJSON (A.Number n)
    A.String s -> case s of
      "time_interval_start" -> pure TimeIntervalStart
      "time_interval_end" -> pure TimeIntervalEnd
      _ ->
        parseFail $
          "Invalid string value. Valid string options are: "
            <> intercalate
              ", "
              [ show @String "time_interval_start"
              , show @String "time_interval_end"
              ]
    v ->
      flip (parseObject "Value") v $
        asum
          [ AvailableMoney <$> "in_account" <*> "amount_of_token"
          , NegValue <$> "negate"
          , AddValue <$> "add" <*> "and"
          , SubValue <$> "value" <*> "minus"
          , MulValue <$> "multiply" <*> "times"
          , DivValue <$> "divide" <*> "by"
          , ChoiceValue <$> "value_of_choice"
          , UseValue <$> "use_value"
          , Cond <$> "if" <*> "then" <*> "else"
          , ValueRef <$> "ref"
          ]

instance Num Value where
  (+) = AddValue
  (*) = MulValue
  abs v = Cond (ValueGE v 0) v (-v)
  signum v = Cond (ValueGT v 0) 1 $ Cond (ValueLT v 0) (-1) 0
  fromInteger = Constant
  negate = NegValue

-- | Convert a core value to its object representation.
fromCoreValue :: Core.Value Core.Observation -> Value
fromCoreValue = \case
  Core.AvailableMoney a b -> AvailableMoney (fromCoreParty a) (fromCoreToken b)
  Core.AddValue a b -> on AddValue fromCoreValue a b
  Core.SubValue a b -> on SubValue fromCoreValue a b
  Core.MulValue a b -> on MulValue fromCoreValue a b
  Core.DivValue a b -> on DivValue fromCoreValue a b
  Core.NegValue a -> NegValue $ fromCoreValue a
  Core.Constant a -> Constant a
  Core.ChoiceValue a -> ChoiceValue $ fromCoreChoiceId a
  Core.UseValue a -> UseValue $ fromCoreValueId a
  Core.TimeIntervalStart -> TimeIntervalStart
  Core.TimeIntervalEnd -> TimeIntervalEnd
  Core.Cond a b c -> Cond (fromCoreObservation a) (fromCoreValue b) (fromCoreValue c)

-- | An observation is a value that is either true or false depending on the state of the contract and the time interval
-- of the current transaction.
data Observation
  = -- | Logical conjunction of two observations.
    AndObs Observation Observation
  | -- | Logical disjunction of two observations.
    OrObs Observation Observation
  | -- | Logical negation of an observation.
    NotObs Observation
  | -- | True if a choice ID has an associated value.
    ChoseSomething ChoiceId
  | -- | True if the first value is greater-then or equal to the second.
    ValueGE Value Value
  | -- | True if the first value is greater-then the second.
    ValueGT Value Value
  | -- | True if the first value is less-then the second.
    ValueLT Value Value
  | -- | True if the first value is less-then or equal to the second.
    ValueLE Value Value
  | -- | True if the first value is equal to the second.
    ValueEQ Value Value
  | -- | A true observation.
    TrueObs
  | -- | A false observation.
    FalseObs
  | -- | A reference to a previously defined observation.
    ObservationRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary)

-- | The immediate children of a observation are any observation sub-terms. Note that observation and value are mutually recursive,
-- so value also must be traversed until a observations is reached.
instance Plated Observation where
  plate = traversal \focus -> \case
    AndObs a b -> on (liftA2 AndObs) focus a b
    OrObs a b -> on (liftA2 OrObs) focus a b
    NotObs a -> NotObs <$> focus a
    ChoseSomething a -> pure $ ChoseSomething a
    ValueGE a b -> ValueGE <$> visitValueObservations focus a <*> visitValueObservations focus b
    ValueGT a b -> ValueGT <$> visitValueObservations focus a <*> visitValueObservations focus b
    ValueLE a b -> ValueLE <$> visitValueObservations focus a <*> visitValueObservations focus b
    ValueLT a b -> ValueLT <$> visitValueObservations focus a <*> visitValueObservations focus b
    ValueEQ a b -> ValueEQ <$> visitValueObservations focus a <*> visitValueObservations focus b
    TrueObs -> pure TrueObs
    FalseObs -> pure FalseObs
    ObservationRef l -> pure $ ObservationRef l

-- | Visit all the value in a observations to a depth of one (i.e. don't descend into sub-terms of value).
visitObservationValues :: (Applicative f) => (Value -> f Value) -> Observation -> f Observation
visitObservationValues f = \case
  AndObs a b -> on (liftA2 AndObs) (visitObservationValues f) a b
  OrObs a b -> on (liftA2 OrObs) (visitObservationValues f) a b
  NotObs a -> NotObs <$> visitObservationValues f a
  ValueGE a b -> ValueGE <$> f a <*> f b
  ValueGT a b -> ValueGT <$> f a <*> f b
  ValueLE a b -> ValueLE <$> f a <*> f b
  ValueLT a b -> ValueLT <$> f a <*> f b
  ValueEQ a b -> ValueEQ <$> f a <*> f b
  obs -> pure obs

instance ToJSON Observation where
  toJSON (AndObs lhs rhs) =
    A.object
      [ "both" .= lhs
      , "and" .= rhs
      ]
  toJSON (OrObs lhs rhs) =
    A.object
      [ "either" .= lhs
      , "or" .= rhs
      ]
  toJSON (NotObs v) =
    A.object
      ["not" .= v]
  toJSON (ChoseSomething choiceId) =
    A.object
      ["chose_something_for" .= choiceId]
  toJSON (ValueGE lhs rhs) =
    A.object
      [ "value" .= lhs
      , "ge_than" .= rhs
      ]
  toJSON (ValueGT lhs rhs) =
    A.object
      [ "value" .= lhs
      , "gt" .= rhs
      ]
  toJSON (ValueLT lhs rhs) =
    A.object
      [ "value" .= lhs
      , "lt" .= rhs
      ]
  toJSON (ValueLE lhs rhs) =
    A.object
      [ "value" .= lhs
      , "le_than" .= rhs
      ]
  toJSON (ValueEQ lhs rhs) =
    A.object
      [ "value" .= lhs
      , "equal_to" .= rhs
      ]
  toJSON TrueObs = toJSON True
  toJSON FalseObs = toJSON False
  toJSON (ObservationRef label) = A.object ["ref" .= label]

instance FromJSON Observation where
  parseJSON = \case
    A.Bool True -> pure TrueObs
    A.Bool False -> pure FalseObs
    v ->
      flip (parseObject "Observation") v $
        asum
          [ AndObs <$> "both" <*> "and"
          , OrObs <$> "either" <*> "or"
          , NotObs <$> "not"
          , ChoseSomething <$> "chose_something_for"
          , ValueGE <$> "value" <*> "ge_than"
          , ValueGT <$> "value" <*> "gt"
          , ValueLE <$> "value" <*> "le_than"
          , ValueLT <$> "value" <*> "lt"
          , ValueEQ <$> "value" <*> "equal_to"
          , ObservationRef <$> "ref"
          ]

-- | Convert a core observation to its object representation.
fromCoreObservation :: Core.Observation -> Observation
fromCoreObservation = \case
  Core.AndObs a b -> on AndObs fromCoreObservation a b
  Core.OrObs a b -> on OrObs fromCoreObservation a b
  Core.NotObs a -> NotObs $ fromCoreObservation a
  Core.ChoseSomething a -> ChoseSomething $ fromCoreChoiceId a
  Core.ValueGE a b -> on ValueGE fromCoreValue a b
  Core.ValueGT a b -> on ValueGT fromCoreValue a b
  Core.ValueLE a b -> on ValueLE fromCoreValue a b
  Core.ValueLT a b -> on ValueLT fromCoreValue a b
  Core.ValueEQ a b -> on ValueEQ fromCoreValue a b
  Core.TrueObs -> TrueObs
  Core.FalseObs -> FalseObs

type AccountId = Party

data Party
  = Address !ShelleyAddress
  | Role !TokenName
  | PartyRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Party where
  toJSON (Address address) = A.object ["address" .= address]
  toJSON (Role token) = A.object ["role_token" .= token]
  toJSON (PartyRef ref) = A.object ["ref" .= ref]

instance FromJSON Party where
  parseJSON =
    parseObject "Party" $
      asum
        [ Address <$> "address"
        , Role <$> "role_token"
        , PartyRef <$> "ref"
        ]

fromCoreParty :: Core.Party -> Party
fromCoreParty = \case
  Core.Address network addr -> Address $ fromCoreAddress network addr
  Core.Role role -> Role $ fromCoreTokenName role

newtype ShelleyAddress = ShelleyAddress {unShelleyAddress :: Address ShelleyAddr}
  deriving (Eq, Ord)

instance Show ShelleyAddress where
  show = show . serialiseToBech32 . unShelleyAddress

-- instance Read ShelleyAddress where
--   readPrec = do
--     String s <- lexP
--     Right addr <- first show $ deserialiseFromBech32 $ T.pack s
--     pure $ ShelleyAddress addr
-- 
-- instance IsString ShelleyAddress where
--   fromString =
--     either (error . showString "fromString(ShelleyAddress): " . show) ShelleyAddress
--       . deserialiseFromBech32 (AsAddress AsShelleyAddr)
--       . T.pack
-- 
instance Binary ShelleyAddress where
  put = put . serialiseToRawBytes . unShelleyAddress
  get =
    either (fail . (<> "failed to deserialize address: ") . show) (pure . ShelleyAddress)
      . deserialiseFromRawBytes (AsAddress AsShelleyAddr)
      =<< get

instance Variations ShelleyAddress where
  variations = do
    let
      mockAddress :: ShelleyAddress
      mockAddress = case deserialiseFromBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz" of
        Left err -> error $ "Failed to parse mock address: " <> show err
        Right addr -> ShelleyAddress addr
    LNE.fromList [mockAddress]

instance ToJSON ShelleyAddress where
  toJSON (ShelleyAddress address) = A.String $ serialiseToBech32 address

instance FromJSON ShelleyAddress where
  parseJSON =
    withText "ShelleyAddress" $
      either (parseFail . show) (pure . ShelleyAddress) . deserialiseFromBech32

fromCoreAddress :: Core.Network -> PV2.Address -> ShelleyAddress
fromCoreAddress network addr =
  ShelleyAddress $
    either (error . show) id $
      deserialiseFromBech32 $
        serialiseAddressBech32 network addr

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving (Show, Generic, Eq, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary, Variations)

fromCoreTokenName :: PV2.TokenName -> TokenName
fromCoreTokenName = TokenName . PV2.fromBuiltin . PV2.unTokenName

toCoreTokenName :: TokenName -> PV2.TokenName
toCoreTokenName = PV2.TokenName . PV2.toBuiltin . unTokenName

data ChoiceId = ChoiceId Text Party
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) =
    A.object
      [ "choice_name" .= name
      , "choice_owner" .= party
      ]

instance FromJSON ChoiceId where
  parseJSON = parseObject "Payee" $ ChoiceId <$> "choice_name" <*> "choice_owner"

fromCoreChoiceId :: Core.ChoiceId -> ChoiceId
fromCoreChoiceId (Core.ChoiceId name owner) = ChoiceId (T.decodeUtf8 $ PV2.fromBuiltin name) (fromCoreParty owner)

newtype ValueId = ValueId {unValueId :: ByteString}
  deriving (Show, Generic, Eq, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Verbatim
  deriving newtype (Binary, Variations)

fromCoreValueId :: Core.ValueId -> ValueId
fromCoreValueId (Core.ValueId bs) = ValueId $ PV2.fromBuiltin bs

toCoreValueId :: ValueId -> Core.ValueId
toCoreValueId (ValueId bs) = Core.ValueId $ PV2.toBuiltin bs

data Payee
  = Party !Party
  | Account !Party
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Payee where
  toJSON (Account acc) = A.object ["account" .= acc]
  toJSON (Party party) = A.object ["party" .= party]

instance FromJSON Payee where
  parseJSON =
    parseObject "Payee" $
      asum
        [ Party <$> "party"
        , Account <$> "account"
        ]

fromCorePayee :: Core.Payee -> Payee
fromCorePayee = \case
  Core.Account acc -> Account $ fromCoreParty acc
  Core.Party p -> Party $ fromCoreParty p

data Token
  = Token CurrencySymbol TokenName
  | TokenRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Token where
  toJSON (Token cs tokName) =
    A.object
      [ "currency_symbol" .= cs
      , "token_name" .= tokName
      ]
  toJSON (TokenRef label) = A.object ["ref" .= label]

instance FromJSON Token where
  parseJSON =
    parseObject "Token" $
      asum
        [ Token <$> "currency_symbol" <*> "token_name"
        , TokenRef <$> "ref"
        ]

fromCoreToken :: Core.Token -> Token
fromCoreToken (Core.Token cs tokName) = Token (fromCoreCurrencySymbol cs) (fromCoreTokenName tokName)

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: ByteString}
  deriving newtype (Eq, Ord, Binary)
  deriving (Show, ToJSON, FromJSON) via EncodeBase16

instance Variations CurrencySymbol where
  -- 28 bytes
  variations = LNE.fromList
    [ CurrencySymbol "0123456789abcdef0123456789abcdef0123456789abcdef012345670123456789ab0123456789ab0123456789ab"
    ]

fromCoreCurrencySymbol :: PV2.CurrencySymbol -> CurrencySymbol
fromCoreCurrencySymbol = CurrencySymbol . PV2.fromBuiltin . PV2.unCurrencySymbol

toCoreCurrencySymbol :: CurrencySymbol -> PV2.CurrencySymbol
toCoreCurrencySymbol = PV2.CurrencySymbol . PV2.toBuiltin . unCurrencySymbol

fromCoreContractHash :: BuiltinByteString -> ContractHash
fromCoreContractHash = ContractHash . PV2.fromBuiltin

newtype ContractHash = ContractHash {unContractHash :: ByteString}
  deriving newtype (Eq, Ord, Binary, Hashable)
  deriving (Show, ToJSON, FromJSON) via EncodeBase16

instance Variations ContractHash where
  -- 28 bytes
  variations = LNE.fromList
    [ContractHash "0123456789abcdef0123456789abcdef0123456789abcdef012345670123456789ab0123456789ab0123456789ab"]
--   deriving newtype (Eq, Ord, Variations)
-- 
-- instance Show Base16 where
--   show = show . encodeBase16 . unBase16

-- instance Read Base16 where
--   readPrec = do
--     String s <- lexP
--     Right parsed <- decodeBase16Untyped $ T.pack s
--     pure $ Base16 parsed
--
--instance IsString Base16 where
--  fromString = either (error . showString "fromString(Base16): " . T.unpack) Base16 . decodeBase16' . T.pack
-- 
-- instance ToJSON Base16 where
--   toJSON = A.String . encodeBase16 . unBase16
-- 
-- instance ToJSONKey Base16 where
--   toJSONKey = toJSONKeyText $ encodeBase16 . unBase16
-- 
-- instance FromJSON Base16 where
--   parseJSON = withText "Base16" $ either (fail . T.unpack) (pure . Base16) . decodeBase16'
-- 
-- instance FromJSONKey Base16 where
--   fromJSONKey = FromJSONKeyTextParser $ either (fail . T.unpack) (pure . Base16) . decodeBase16'
-- 
data Bound = Bound Integer Integer
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary, Variations)

instance ToJSON Bound where
  toJSON (Bound from to) = A.object ["from" .= from, "to" .= to]

instance FromJSON Bound where
  parseJSON = withObject "Bound" \obj -> do
    from <- obj .: "from"
    to <- obj .: "to"
    pure $ Bound from to

fromCoreBound :: Core.Bound -> Bound
fromCoreBound (Core.Bound lo hi) = Bound lo hi

toCoreBound :: Bound -> Core.Bound
toCoreBound (Bound lo hi) = Core.Bound lo hi

data Contract
  = Close
  | Pay AccountId Payee Token Value Contract
  | If Observation Contract Contract
  | When [Case] Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract
  | ContractRef Label
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (Binary)

instance Plated Contract where
  plate = traversal \focus -> \case
    Close -> pure Close
    Pay a p t v c -> Pay a p t v <$> focus c
    If c a b -> If c <$> focus a <*> focus b
    When cases t c ->
      When
        <$> traverse (visitCaseContracts focus) cases
        <*> pure t
        <*> focus c
    Let vi v c -> Let vi v <$> focus c
    Assert obs c -> Assert obs <$> focus c
    ContractRef l -> pure $ ContractRef l

visitCaseContracts :: (Applicative f) => (Contract -> f Contract) -> Case -> f Case
visitCaseContracts f = \case
  Case a c' -> Case a <$> f c'
  c' -> pure c'

visitContractCases :: (Applicative f) => (Case -> f Case) -> Contract -> f Contract
visitContractCases f = \case
  Close -> pure Close
  Pay a p t v c -> Pay a p t v <$> visitContractCases f c
  If c a b -> If c <$> visitContractCases f a <*> visitContractCases f b
  When cases t c ->
    When
      <$> traverse f cases
      <*> pure t
      <*> visitContractCases f c
  Let vi v c -> Let vi v <$> visitContractCases f c
  Assert obs c -> Assert obs <$> visitContractCases f c
  ContractRef l -> pure $ ContractRef l

instance ToJSON Contract where
  toJSON Close = "close"
  toJSON (Pay accountId payee token value contract) =
    A.object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= contract
      ]
  toJSON (If obs cont1 cont2) =
    A.object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When cases timeout cont) =
    A.object
      [ "when" .= cases
      , "timeout" .= timeout
      , "timeout_continuation" .= cont
      ]
  toJSON (Let valId value cont) =
    A.object
      [ "let" .= valId
      , "be" .= value
      , "then" .= cont
      ]
  toJSON (Assert obs cont) =
    A.object
      [ "assert" .= obs
      , "then" .= cont
      ]
  toJSON (ContractRef label) = A.object ["ref" .= label]

instance FromJSON Contract where
  parseJSON = \case
    A.String "close" -> pure Close
    v ->
      flip (parseObject "Contract") v $
        asum
          [ Pay <$> "from_account" <*> "to" <*> "token" <*> "pay" <*> "then"
          , If <$> "if" <*> "then" <*> "else"
          , When <$> "when" <*> "timeout" <*> "timeout_continuation"
          , Let <$> "let" <*> "be" <*> "then"
          , Assert <$> "assert" <*> "then"
          , ContractRef <$> "ref"
          ]

fromCoreContract :: Core.Contract -> Contract
fromCoreContract = \case
  Core.Close -> Close
  Core.Pay account payee token value contract ->
    Pay
      (fromCoreParty account)
      (fromCorePayee payee)
      (fromCoreToken token)
      (fromCoreValue value)
      (fromCoreContract contract)
  Core.If obs a b ->
    If (fromCoreObservation obs) (fromCoreContract a) (fromCoreContract b)
  Core.When cases timeout contract ->
    When (fromCoreCase <$> cases) (fromCoreTimeout timeout) (fromCoreContract contract)
  Core.Let valueId value contract ->
    Let (fromCoreValueId valueId) (fromCoreValue value) (fromCoreContract contract)
  Core.Assert obs contract ->
    Assert (fromCoreObservation obs) (fromCoreContract contract)

-- The following require manual instances to avoid infinite recursion.
instance Variations Contract where
  variations =
    join $
      LNE.fromList
        [ pure Close
        , Pay <$> variations `varyAp` variations `varyAp` variations `varyAp` variations <*> pure Close
        , If <$> variations <*> pure Close <*> pure Close
        , When <$> variations `varyAp` variations <*> pure Close
        , Let <$> variations `varyAp` variations <*> pure Close
        , Assert <$> variations <*> pure Close
        , ContractRef <$> variations
        ]

instance Variations Case where
  variations =
    join $
      LNE.fromList
        [ Case <$> variations <*> pure Close
        , MerkleizedCase <$> variations `varyAp` variations
        ]

instance Variations Observation where
  variations =
    join $
      LNE.fromList
        [ pure $ AndObs FalseObs FalseObs
        , pure $ OrObs FalseObs FalseObs
        , pure $ NotObs FalseObs
        , ChoseSomething <$> variations
        , pure $ ValueGE TimeIntervalStart TimeIntervalStart
        , pure $ ValueLE TimeIntervalStart TimeIntervalStart
        , pure $ ValueGT TimeIntervalStart TimeIntervalStart
        , pure $ ValueLT TimeIntervalStart TimeIntervalStart
        , pure $ ValueEQ TimeIntervalStart TimeIntervalStart
        , pure FalseObs
        , pure TrueObs
        , ObservationRef <$> variations
        ]

instance Variations Value where
  variations =
    join $
      LNE.fromList
        [ AvailableMoney <$> variations `varyAp` variations
        , Constant <$> variations
        , pure $ NegValue (Constant 1)
        , pure $ AddValue (Constant 1) (Constant 1)
        , pure $ SubValue (Constant 1) (Constant 1)
        , pure $ MulValue (Constant 1) (Constant 1)
        , pure $ DivValue (Constant 1) (Constant 1)
        , ChoiceValue <$> variations
        , pure TimeIntervalStart
        , pure TimeIntervalEnd
        , UseValue <$> variations
        , pure $ Cond FalseObs (Constant 1) (Constant 1)
        , ValueRef <$> variations
        ]

-- A singleton witness of the type of an object.
data ObjectType a where
  ValueType :: ObjectType Value
  ObservationType :: ObjectType Observation
  ContractType :: ObjectType Contract
  PartyType :: ObjectType Party
  TokenType :: ObjectType Token
  ActionType :: ObjectType Action

deriving instance Show (ObjectType a)
deriving instance Eq (ObjectType a)
deriving instance Ord (ObjectType a)

instance TestEquality ObjectType where
  testEquality ValueType ValueType = Just Refl
  testEquality ValueType _ = Nothing
  testEquality ObservationType ObservationType = Just Refl
  testEquality ObservationType _ = Nothing
  testEquality ContractType ContractType = Just Refl
  testEquality ContractType _ = Nothing
  testEquality PartyType PartyType = Just Refl
  testEquality PartyType _ = Nothing
  testEquality TokenType TokenType = Just Refl
  testEquality TokenType _ = Nothing
  testEquality ActionType ActionType = Just Refl
  testEquality ActionType _ = Nothing

instance ToJSON (ObjectType a) where
  toJSON = \case
    ValueType -> "value"
    ObservationType -> "observation"
    ContractType -> "contract"
    PartyType -> "party"
    TokenType -> "token"
    ActionType -> "action"

-- | An existential encoding of an ObjectType.
newtype SomeObjectType = UnsafeSomeObjectType (ObjectType Any)
  deriving newtype (ToJSON)

instance Eq SomeObjectType where
  SomeObjectType x == SomeObjectType y = isJust $ testEquality x y

instance Ord SomeObjectType where
  compare (SomeObjectType ValueType) (SomeObjectType ValueType) = EQ
  compare (SomeObjectType ValueType) _ = LT
  compare _ (SomeObjectType ValueType) = GT
  compare (SomeObjectType ObservationType) (SomeObjectType ObservationType) = EQ
  compare (SomeObjectType ObservationType) _ = LT
  compare _ (SomeObjectType ObservationType) = GT
  compare (SomeObjectType ContractType) (SomeObjectType ContractType) = EQ
  compare (SomeObjectType ContractType) _ = LT
  compare _ (SomeObjectType ContractType) = GT
  compare (SomeObjectType PartyType) (SomeObjectType PartyType) = EQ
  compare (SomeObjectType PartyType) _ = LT
  compare _ (SomeObjectType PartyType) = GT
  compare (SomeObjectType TokenType) (SomeObjectType TokenType) = EQ
  compare (SomeObjectType TokenType) _ = LT
  compare _ (SomeObjectType TokenType) = GT
  compare (SomeObjectType ActionType) (SomeObjectType ActionType) = EQ

{-# COMPLETE SomeObjectType #-}
pattern SomeObjectType :: ObjectType a -> SomeObjectType
pattern SomeObjectType x <- UnsafeSomeObjectType x
  where
    SomeObjectType x = UnsafeSomeObjectType (unsafeCoerce x)

instance Show SomeObjectType where
  showsPrec p (SomeObjectType t) =
    showParen
      (p >= 11)
      ( showString "SomeObjectType"
          . showSpace
          . showsPrec 11 t
      )

readObjectType :: ReadPrec SomeObjectType
readObjectType = do
  Ident ctor <- lexP
  case ctor of
    "ValueType" -> pure $ SomeObjectType ValueType
    "ObservationType" -> pure $ SomeObjectType ObservationType
    "ContractType" -> pure $ SomeObjectType ContractType
    "PartyType" -> pure $ SomeObjectType PartyType
    "TokenType" -> pure $ SomeObjectType TokenType
    "ActionType" -> pure $ SomeObjectType ActionType
    _ -> empty

instance Read SomeObjectType where
  readPrec = parens $ prec 10 do
    Ident "SomeObjectType" <- lexP
    step readObjectType

instance FromJSON SomeObjectType where
  parseJSON = withText "ObjectType" \case
    "value" -> pure $ SomeObjectType ValueType
    "observation" -> pure $ SomeObjectType ObservationType
    "contract" -> pure $ SomeObjectType ContractType
    "party" -> pure $ SomeObjectType PartyType
    "token" -> pure $ SomeObjectType TokenType
    "action" -> pure $ SomeObjectType ActionType
    _ ->
      parseFail $
        "expected one of: "
          <> intercalate
            ", "
            [ "value"
            , "observation"
            , "contract"
            , "party"
            , "token"
            , "action"
            ]

instance Binary SomeObjectType where
  put (SomeObjectType t) = putWord8 case t of
    ValueType -> 0
    ObservationType -> 1
    ContractType -> 2
    PartyType -> 3
    TokenType -> 4
    ActionType -> 5
  get = do
    tag <- getWord8
    case tag of
      0 -> pure $ SomeObjectType ValueType
      1 -> pure $ SomeObjectType ObservationType
      2 -> pure $ SomeObjectType ContractType
      3 -> pure $ SomeObjectType PartyType
      4 -> pure $ SomeObjectType TokenType
      5 -> pure $ SomeObjectType ActionType
      _ -> fail $ "Invalid tag byte " <> show tag

instance Variations SomeObjectType where
  variations =
    LNE.fromList
      [ SomeObjectType ValueType
      , SomeObjectType ObservationType
      , SomeObjectType ContractType
      , SomeObjectType PartyType
      , SomeObjectType TokenType
      , SomeObjectType ActionType
      ]

-- | An object type paired with a value whose type is determined by the value of the object type.
data Object where
  Object :: ObjectType a -> a -> Object

-- | A prism that focuses on an object if it is a value.
_ValueObject :: Prism' Object Value
_ValueObject = prism' (Object ValueType) \case
  Object ValueType a -> Just a
  _ -> Nothing

-- | A prism that focuses on an object if it is an observation.
_ObservationObject :: Prism' Object Observation
_ObservationObject = prism' (Object ObservationType) \case
  Object ObservationType a -> Just a
  _ -> Nothing

-- | A prism that focuses on an object if it is a contract.
_ContractObject :: Prism' Object Contract
_ContractObject = prism' (Object ContractType) \case
  Object ContractType a -> Just a
  _ -> Nothing

-- | A prism that focuses on an object if it is a party.
_PartyObject :: Prism' Object Party
_PartyObject = prism' (Object PartyType) \case
  Object PartyType a -> Just a
  _ -> Nothing

-- | A prism that focuses on an object if it is a token.
_TokenObject :: Prism' Object Token
_TokenObject = prism' (Object TokenType) \case
  Object TokenType a -> Just a
  _ -> Nothing

-- | A prism that focuses on an object if it is an action.
_ActionObject :: Prism' Object Action
_ActionObject = prism' (Object ActionType) \case
  Object ActionType a -> Just a
  _ -> Nothing

instance Show Object where
  showsPrec p (Object t a) =
    showParen
      (p >= 11)
      ( showString "Object"
          . showSpace
          . showsPrec 11 t
          . showSpace
          . case t of
            ValueType -> showsPrec 11 a
            ObservationType -> showsPrec 11 a
            ContractType -> showsPrec 11 a
            PartyType -> showsPrec 11 a
            TokenType -> showsPrec 11 a
            ActionType -> showsPrec 11 a
      )

-- instance Read Object where
--   readPrec = parens $ prec 10 $ do
--     Ident "Object" <- lexP
--     SomeObjectType t <- step readObjectType
--     case t of
--       ValueType -> Object t <$> step readPrec
--       ObservationType -> Object t <$> step readPrec
--       ContractType -> Object t <$> step readPrec
--       PartyType -> Object t <$> step readPrec
--       TokenType -> Object t <$> step readPrec
--       ActionType -> Object t <$> step readPrec
-- 
instance Eq Object where
  Object t v == Object t' v' = case (t, t') of
    (ValueType, ValueType) -> v == v'
    (ValueType, _) -> False
    (ObservationType, ObservationType) -> v == v'
    (ObservationType, _) -> False
    (ContractType, ContractType) -> v == v'
    (ContractType, _) -> False
    (PartyType, PartyType) -> v == v'
    (PartyType, _) -> False
    (TokenType, TokenType) -> v == v'
    (TokenType, _) -> False
    (ActionType, ActionType) -> v == v'
    (ActionType, _) -> False

instance Ord Object where
  compare (Object t v) (Object t' v') = case (t, t') of
    (ValueType, ValueType) -> compare v v'
    (ValueType, _) -> LT
    (_, ValueType) -> GT
    (ObservationType, ObservationType) -> compare v v'
    (ObservationType, _) -> LT
    (_, ObservationType) -> GT
    (ContractType, ContractType) -> compare v v'
    (ContractType, _) -> LT
    (_, ContractType) -> GT
    (PartyType, PartyType) -> compare v v'
    (PartyType, _) -> LT
    (_, PartyType) -> GT
    (TokenType, TokenType) -> compare v v'
    (TokenType, _) -> LT
    (_, TokenType) -> GT
    (ActionType, ActionType) -> compare v v'

-- | An object with a label.
data LabelledObject = forall a.
  LabelledObject
  { _label :: Label
  -- ^ The label of the object
  , _type :: ObjectType a
  -- ^ The type of the object
  , _value :: a
  -- ^ The value of the object
  }

-- | A lens that focuses on the type and value of a labelled object.
object :: Lens' LabelledObject Object
object f LabelledObject{..} = (\(Object t a) -> LabelledObject _label t a) <$> f (Object _type _value)

instance Show LabelledObject where
  showsPrec _ LabelledObject{..} =
    showString "LabelledObject { label = "
      . shows _label
      . showString ", _type = "
      . shows _type
      . showString ", value = "
      . ( case _type of
            ValueType -> shows _value
            ObservationType -> shows _value
            ContractType -> shows _value
            PartyType -> shows _value
            TokenType -> shows _value
            ActionType -> shows _value
        )
      . showString " }"

-- instance Read LabelledObject where
--   readPrec =
--     asum
--       [ readNormal
--       , readRecord
--       ]
--     where
--       readNormal :: ReadPrec LabelledObject
--       readNormal = parens $ prec 10 $ do
--         Ident "LabelledObject" <- lexP
--         lbl :: Label <- step readPrec
--         SomeObjectType _type <- step readObjectType
--         case _type of
--           ValueType -> LabelledObject lbl _type <$> step readPrec
--           ObservationType -> LabelledObject lbl _type <$> step readPrec
--           ContractType -> LabelledObject lbl _type <$> step readPrec
--           PartyType -> LabelledObject lbl _type <$> step readPrec
--           TokenType -> LabelledObject lbl _type <$> step readPrec
--           ActionType -> LabelledObject lbl _type <$> step readPrec
-- 
--       readRecord :: ReadPrec LabelledObject
--       readRecord = do
--         Ident "LabelledObject" <- lexP
--         Punc "{" <- lexP
--         Ident "label" <- lexP
--         Punc "=" <- lexP
--         lbl :: Label <- reset readPrec
--         Punc "," <- lexP
--         Ident "_type" <- lexP
--         Punc "=" <- lexP
--         SomeObjectType _type <- reset readObjectType
--         Punc "," <- lexP
--         Ident "value" <- lexP
--         Punc "=" <- lexP
--         x <- case _type of
--           ValueType -> LabelledObject lbl _type <$> reset readPrec
--           ObservationType -> LabelledObject lbl _type <$> reset readPrec
--           ContractType -> LabelledObject lbl _type <$> reset readPrec
--           PartyType -> LabelledObject lbl _type <$> reset readPrec
--           TokenType -> LabelledObject lbl _type <$> reset readPrec
--           ActionType -> LabelledObject lbl _type <$> reset readPrec
--         Punc "}" <- lexP
--         pure x

instance Eq LabelledObject where
  LabelledObject l t v == LabelledObject l' t' v' = l == l' && Object t v == Object t' v'

instance Ord LabelledObject where
  compare (LabelledObject l t v) (LabelledObject l' t' v') =
    compare l l' <> compare (Object t v) (Object t' v')

instance ToJSON LabelledObject where
  toJSON LabelledObject{..} =
    A.object
      [ "label" .= _label
      , "type" .= _type
      , case _type of
          ValueType -> "value" .= _value
          ObservationType -> "value" .= _value
          ContractType -> "value" .= _value
          PartyType -> "value" .= _value
          TokenType -> "value" .= _value
          ActionType -> "value" .= _value
      ]

instance FromJSON LabelledObject where
  parseJSON = withObject "LabelledObject" \obj -> do
    lbl :: Label <- obj .: "label"
    SomeObjectType _type <- obj .: "type"
    case _type of
      ValueType -> LabelledObject lbl _type <$> obj .: "value"
      ObservationType -> LabelledObject lbl _type <$> obj .: "value"
      ContractType -> LabelledObject lbl _type <$> obj .: "value"
      PartyType -> LabelledObject lbl _type <$> obj .: "value"
      TokenType -> LabelledObject lbl _type <$> obj .: "value"
      ActionType -> LabelledObject lbl _type <$> obj .: "value"

instance Binary LabelledObject where
  put LabelledObject{..} = do
    put _label
    put $ SomeObjectType _type
    case _type of
      ValueType -> put _value
      ObservationType -> put _value
      ContractType -> put _value
      PartyType -> put _value
      TokenType -> put _value
      ActionType -> put _value

  get = do
    lbl :: Label <- get
    SomeObjectType _type <- get
    case _type of
      ValueType -> LabelledObject lbl _type <$> get
      ObservationType -> LabelledObject lbl _type <$> get
      ContractType -> LabelledObject lbl _type <$> get
      PartyType -> LabelledObject lbl _type <$> get
      TokenType -> LabelledObject lbl _type <$> get
      ActionType -> LabelledObject lbl _type <$> get

instance Variations LabelledObject where
  variations = do
    (lbl :: Label, _type) <- variations
    case _type of
      SomeObjectType ValueType -> LabelledObject lbl ValueType <$> variations
      SomeObjectType ObservationType -> LabelledObject lbl ObservationType <$> variations
      SomeObjectType ContractType -> LabelledObject lbl ContractType <$> variations
      SomeObjectType PartyType -> LabelledObject lbl PartyType <$> variations
      SomeObjectType TokenType -> LabelledObject lbl TokenType <$> variations
      SomeObjectType ActionType -> LabelledObject lbl ActionType <$> variations

labelledObjectType :: LabelledObject -> SomeObjectType
labelledObjectType LabelledObject{..} = SomeObjectType _type

newtype ObjectBundle = ObjectBundle {getObjects :: [LabelledObject]}
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Variations, Binary)

makePrisms ''Contract
makePrisms ''ObjectBundle
makePrisms ''Label
makePrisms ''Timeout
makePrisms ''Bound
makePrisms ''ContractHash
makePrisms ''Token
makePrisms ''Payee
makePrisms ''ValueId
makePrisms ''ChoiceId
makePrisms ''TokenName
makePrisms ''ShelleyAddress
makePrisms ''Party
makePrisms ''Observation
makePrisms ''Value
makePrisms ''Action
makePrisms ''Case

makeLensesFor [("_label", "label")] ''LabelledObject
