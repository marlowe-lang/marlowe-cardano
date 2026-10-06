-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- A big hammer, but it helps.
{-# OPTIONS_GHC -fno-specialise #-}

-- | Types for Marlowe semantics
module Language.Marlowe.Core.V1.Semantics.Types (
  -- * Type Aliases
  AccountId,
  Accounts,
  ChoiceName,
  ChosenNum,
  Money,
  TimeInterval,
  Timeout,

  -- * Contract Types
  Action (..),
  Bound (..),
  Case (..),
  ChoiceId (..),
  Contract (..),
  CurrencySymbol (..),
  Environment (..),
  Input (..),
  InputContent (..),
  IntervalResult (..),
  Observation (..),
  Party (..),
  Payee (..),
  State (..),
  Token (..),
  TokenName (..),
  Value (..),
  ValueId (..),

  -- * Error Types
  IntervalError (..),

  -- * Utility Functions/Constants
  ada,
  emptyState,
  getAction,
  getInputContent,
  inBounds,
  mkChoiceIdUtf8,
  mkCurrencySymbolHex,
  mkPartyAddressBech32,
  mkRoleByteString,
  mkRoleHex,
  mkRoleUtf8,
  mkTokenNameByteString,
  mkTokenNameHex,
  mkTokenNameUtf8,
  mkValueIdUtf8,
  mkValueIdHex,
  unsafeMkCurrencySymbolHex,
  unsafeMkPartyAddressBech32,
  unsafeMkRoleHex,
  unsafeMkTokenNameHex,

  -- * Pattern synonyms
  pattern LenientCurrencySymbolHex,
  pattern LenientRoleHex,
  pattern LenientTokeNameHex,
  pattern PartyAddressBech32,
  pattern RoleByteString,
  pattern RoleUtf8,
  pattern TokenNameByteString,
  pattern TokenNameUtf8,
  pattern UnsafeCurrencySymbolHex,
  pattern UnsafePartyAddressBech32,
  pattern UnsafeRoleHex,
  pattern UnsafeTokenNameHex,

  -- * Serialisation
  fromJSONAssocMap,
  posixTimeFromJSON,
  posixTimeToJSON,
  toJSONAssocMap,
) where

import Control.Applicative ((<*>), (<|>))
import Control.DeepSeq (NFData)
import Control.Newtype.Generics (Newtype)
import qualified Data.Aeson as A
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types hiding (Error, Value)
import qualified Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Base16.Aeson (EncodeBase16 (EncodeBase16))
import qualified Data.ByteString.Base16.Aeson as Base16.Aeson
import qualified Data.Char as Char
import qualified Data.Foldable as F
import Data.Scientific (floatingOrInteger, scientific)
import Data.String (IsString (..), String)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding as Text (decodeUtf8, decodeUtf8', encodeUtf8)
import Deriving.Aeson
import Language.Marlowe.Core.V1.Semantics.Types.Address
import Language.Marlowe.ParserUtil (getInteger, withInteger)
import Language.Marlowe.Pretty (Pretty (..))
import qualified PlutusLedgerApi.V1.Value as Val
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol, unCurrencySymbol), POSIXTime (..), TokenName (unTokenName))
import qualified PlutusLedgerApi.V2 as Ledger (Address (..))
import PlutusTx (makeIsDataIndexed)
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)
import PlutusTx.Lift (makeLift)
import qualified PlutusTx.List as List
import PlutusTx.Prelude hiding (encodeUtf8, (<*>), (<>))
import Text.PrettyPrint.Leijen (parens, text)
import qualified Prelude as H

-- Provides equality for PlutusTx types.
import Adapter.PlutusTx.AssocMap ()

-- Functions that used in Plutus Core must be inlinable,
-- so their code is available for PlutusTx compiler.
{-# INLINEABLE getAction #-}
{-# INLINEABLE getInputContent #-}
{-# INLINEABLE inBounds #-}
{-# INLINEABLE emptyState #-}

-- | A Party to a contractt.
data Party
  = -- | Party identified by a network address.
    Address Network Ledger.Address
  | -- | Party identified by a role token name.
    Role TokenName
  deriving stock (Generic, H.Eq, H.Ord)

mkPartyAddressBech32 :: String -> Maybe Party
mkPartyAddressBech32 s = do
  (network, address) <- deserialiseAddressBech32 $ Text.pack s
  return $ Address network address

unsafeMkPartyAddressBech32 :: String -> Party
unsafeMkPartyAddressBech32 s =
  case mkPartyAddressBech32 s of
    Just party -> party
    Nothing -> H.error $ "Invalid address: " H.++ s

serialiseAddressBech32' :: Party -> Maybe Text
serialiseAddressBech32' (Address net addr) = Just $ serialiseAddressBech32 net addr
serialiseAddressBech32' _ = Nothing

-- Only pattern matching is safe.
{-# COMPLETE PartyAddressBech32, RoleByteString #-}
pattern PartyAddressBech32 :: String -> Party
pattern PartyAddressBech32 str <- (fmap Text.unpack . serialiseAddressBech32' -> Just str)

-- Actually the constructor here is unsafe.
{-# COMPLETE UnsafePartyAddressBech32, RoleByteString #-}
pattern UnsafePartyAddressBech32 :: String -> Party
pattern UnsafePartyAddressBech32 str <- (fmap Text.unpack . serialiseAddressBech32' -> Just str)
  where
    UnsafePartyAddressBech32 = unsafeMkPartyAddressBech32

fromBuiltinByteStringToHex :: BuiltinByteString -> String
fromBuiltinByteStringToHex = Text.unpack . Text.decodeUtf8 . Base16.encode . fromBuiltin

fromBuiltinByteStringToUtf8 :: BuiltinByteString -> Maybe String
fromBuiltinByteStringToUtf8 (fromBuiltin -> bs) =
  case Text.decodeUtf8' bs of
    Left _ -> Nothing
    Right txt -> Just $ Text.unpack txt

-- Useful helpers which decode a literal using
-- `Text` or `ByteString` instances for `IsString`.
-- Example: "café" is encoded as 5 bytes (0x 63 61 66 C3 A9)
mkRoleUtf8 :: String -> Party
mkRoleUtf8 = Role . mkTokenNameUtf8

-- Example: "café" is encoded as 4 bytes (0x 63 61 66 E9)
-- because the over 255 value C3 is replaced by single E9 byte.
-- On the other hand you can control the exact bytes using escape
-- codes.
mkRoleByteString :: ByteString -> Party
mkRoleByteString = Role . mkTokenNameByteString

-- You can be exact and use this constructor instead:
-- `roleHex "636166E9"`  or `roleHex "636166C3A9"`
mkRoleHex :: String -> Maybe Party
mkRoleHex str = Role <$> mkTokenNameHex str

unsafeMkRoleHex :: String -> Party
unsafeMkRoleHex = Role . unsafeMkTokenNameHex

pattern RoleUtf8 :: String -> Party
pattern RoleUtf8 name <- Role (TokenNameUtf8 name)
  where
    RoleUtf8 name = Role (mkTokenNameUtf8 name)

pattern RoleByteString :: ByteString -> Party
pattern RoleByteString bs <- Role (TokenNameByteString bs)
  where
    RoleByteString bs = Role (mkTokenNameByteString bs)

-- Actually the constructor is unsafe here
{-# COMPLETE UnsafeRoleHex #-}
pattern UnsafeRoleHex :: String -> Party
pattern UnsafeRoleHex hexStr <- Role (UnsafeTokenNameHex hexStr)
  where
    UnsafeRoleHex hexStr = Role (unsafeMkTokenNameHex hexStr)

{-# COMPLETE LenientRoleHex #-}
pattern LenientRoleHex :: String -> Party
pattern LenientRoleHex hexStr <- Role (LenientTokeNameHex hexStr)
  where
    LenientRoleHex hexStr = Role (LenientTokeNameHex hexStr)

mkTokenNameUtf8 :: String -> Val.TokenName
mkTokenNameUtf8 = Val.TokenName . toBuiltin . Text.encodeUtf8 . Text.pack

mkTokenNameByteString :: ByteString -> Val.TokenName
mkTokenNameByteString = Val.TokenName . toBuiltin

mkTokenNameHex :: String -> Maybe Val.TokenName
mkTokenNameHex str
  | List.all Char.isHexDigit str = Just . Val.TokenName . stringToBuiltinByteStringHex $ str
  | otherwise = Nothing

unsafeMkTokenNameHex :: String -> Val.TokenName
unsafeMkTokenNameHex str =
  case mkTokenNameHex str of
    Just tn -> tn
    Nothing -> H.error $ "Invalid hex string for TokenName: " H.++ str

-- This partial is tracked by the compiler
pattern TokenNameUtf8 :: String -> Val.TokenName
pattern TokenNameUtf8 txt <- Val.TokenName (fromBuiltinByteStringToUtf8 -> Just txt)
  where
    TokenNameUtf8 = Val.TokenName . toBuiltin . Text.encodeUtf8 . Text.pack

{-# COMPLETE TokenNameByteString #-}
pattern TokenNameByteString :: ByteString -> Val.TokenName
pattern TokenNameByteString bs <- Val.TokenName (fromBuiltin -> bs)
  where
    TokenNameByteString bs = Val.TokenName (toBuiltin bs)

-- Actually the constructor here is unsafe.
{-# COMPLETE UnsafeTokenNameHex #-}
pattern UnsafeTokenNameHex :: String -> Val.TokenName
pattern UnsafeTokenNameHex hexStr <- Val.TokenName (fromBuiltinByteStringToHex -> hexStr)
  where
    UnsafeTokenNameHex hexStr = unsafeMkTokenNameHex hexStr

{-# COMPLETE LenientTokeNameHex #-}
pattern LenientTokeNameHex :: String -> Val.TokenName
pattern LenientTokeNameHex hexStr <- UnsafeTokenNameHex hexStr
  where
    LenientTokeNameHex = Val.TokenName . stringToBuiltinByteStringHex

mkCurrencySymbolHex :: String -> Maybe Val.CurrencySymbol
mkCurrencySymbolHex str
  | List.all Char.isHexDigit str = Just . Val.CurrencySymbol . stringToBuiltinByteStringHex $ str
  | otherwise = Nothing

unsafeMkCurrencySymbolHex :: String -> Val.CurrencySymbol
unsafeMkCurrencySymbolHex str =
  case mkCurrencySymbolHex str of
    Just cs -> cs
    Nothing -> H.error $ "Invalid hex string for CurrencySymbol: " H.++ str

-- Actually the constructor here is unsafe.
{-# COMPLETE UnsafeCurrencySymbolHex #-}
pattern UnsafeCurrencySymbolHex :: String -> Val.CurrencySymbol
pattern UnsafeCurrencySymbolHex hexStr <- CurrencySymbol (fromBuiltinByteStringToHex -> hexStr)
  where
    UnsafeCurrencySymbolHex = unsafeMkCurrencySymbolHex

{-# COMPLETE LenientCurrencySymbolHex #-}
pattern LenientCurrencySymbolHex :: String -> Val.CurrencySymbol
pattern LenientCurrencySymbolHex hexStr <- Val.CurrencySymbol (fromBuiltinByteStringToHex -> hexStr)
  where
    LenientCurrencySymbolHex = Val.CurrencySymbol . stringToBuiltinByteStringHex

instance NFData Party

instance Pretty Party where
  prettyFragment (Address network address) = parens $ text "Address " H.<> prettyFragment (serialiseAddressBech32 network address)
  prettyFragment (Role role) = parens $ text "Role " H.<> prettyFragment role

instance H.Show Party where
  showsPrec _ (Address network address) = H.showsPrec 11 $ H.show (serialiseAddressBech32 network address)
  showsPrec _ (Role role) = H.showsPrec 11 $ unTokenName role

-- | A party's internal account in a contract.
type AccountId = Party

-- | A timeout in a contract.
type Timeout = POSIXTime

-- | A multi-asset value.
type Money = Val.Value

-- | The name of a choice in a contract.
type ChoiceName = BuiltinByteString

-- | A numeric choice in a contract.
type ChosenNum = Integer

-- | The time validity range for a Marlowe transaction, inclusive of both endpoints.
type TimeInterval = (POSIXTime, POSIXTime)

-- | The accounts in a contract.
type Accounts = Map (AccountId, Token) Integer

-- | Choices – of integers – are identified by ChoiceId which combines a name for
-- the choice with the Party who had made the choice.
data ChoiceId = ChoiceId BuiltinByteString Party
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

mkChoiceIdUtf8 :: String -> Party -> ChoiceId
mkChoiceIdUtf8 name party = ChoiceId (toBuiltin $ Text.encodeUtf8 $ Text.pack name) party

instance NFData ChoiceId

-- | Token - represents a currency or token, it groups
--   a pair of a currency symbol and token name.
data Token = Token CurrencySymbol TokenName
  deriving stock (Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

ada :: Token
ada = Token Val.adaSymbol Val.adaToken

instance NFData Token

instance H.Show Token where
  showsPrec p (Token cs tn) =
    H.showParen
      (p H.>= 11)
      (H.showString $ "Token \"" H.++ H.show cs H.++ "\" " H.++ H.show tn)

-- | Values, as defined using Let ar e identified by name,
--   and can be used by 'UseValue' construct.
newtype ValueId = ValueId BuiltinByteString
  deriving newtype (IsString, H.Show)
  deriving stock (H.Eq, H.Ord, Generic)
  deriving anyclass (Newtype)

instance NFData ValueId

mkValueIdHex :: String -> ValueId
mkValueIdHex = ValueId . stringToBuiltinByteStringHex

mkValueIdUtf8 :: String -> ValueId
mkValueIdUtf8 = ValueId . toBuiltin . Text.encodeUtf8 . Text.pack

-- | Values include some quantities that change with time,
--   including “the time interval”, “the current balance of an account”,
--   and any choices that have already been made.
--
--   Values can also be scaled, and combined using addition, subtraction, and negation.
data Value a
  = AvailableMoney AccountId Token
  | Constant Integer
  | NegValue (Value a)
  | AddValue (Value a) (Value a)
  | SubValue (Value a) (Value a)
  | MulValue (Value a) (Value a)
  | DivValue (Value a) (Value a)
  | ChoiceValue ChoiceId
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue ValueId
  | Cond a (Value a) (Value a)
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance (NFData a) => NFData (Value a)

-- | Observations are Boolean values derived by comparing values,
--   and can be combined using the standard Boolean operators.
--
--   It is also possible to observe whether any choice has been made
--   (for a particular identified choice).
data Observation
  = AndObs Observation Observation
  | OrObs Observation Observation
  | NotObs Observation
  | ChoseSomething ChoiceId
  | ValueGE (Value Observation) (Value Observation)
  | ValueGT (Value Observation) (Value Observation)
  | ValueLT (Value Observation) (Value Observation)
  | ValueLE (Value Observation) (Value Observation)
  | ValueEQ (Value Observation) (Value Observation)
  | TrueObs
  | FalseObs
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance NFData Observation

-- | The (inclusive) bound on a choice number.
data Bound = Bound Integer Integer
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance NFData Bound

-- | Actions happen at particular points during execution.
--   Three kinds of action are possible:
--
--   * A @Deposit n p v@ makes a deposit of value @v@ into account @n@ belonging to party @p@.
--
--   * A choice is made for a particular id with a list of bounds on the values that are acceptable.
--     For example, @[(0, 0), (3, 5]@ offers the choice of one of 0, 3, 4 and 5.
--
--   * The contract is notified that a particular observation be made.
--     Typically this would be done by one of the parties,
--     or one of their wallets acting automatically.
data Action
  = Deposit AccountId Party Token (Value Observation)
  | Choice ChoiceId [Bound]
  | Notify Observation
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance NFData Action

-- | A payment can be made to one of the parties to the contract,
--   or to one of the accounts of the contract,
--   and this is reflected in the definition.
data Payee
  = Account AccountId
  | Party Party
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance NFData Payee

-- | A case is a branch of a when clause, guarded by an action.
--   The continuation of the contract may be merkleized or not.
--
--   Plutus doesn't support mutually recursive data types yet.
--   datatype Case is mutually recursive with @Contract@
data Case a
  = Case Action a
  | MerkleizedCase Action BuiltinByteString
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance (NFData a) => NFData (Case a)

-- | Extract the @Action@ from a @Case@.
getAction :: Case a -> Action
getAction (Case action _) = action
getAction (MerkleizedCase action _) = action

-- | Marlowe has six ways of building contracts.
--   Five of these – 'Pay', 'Let', 'If', 'When' and 'Assert' –
--   build a complex contract from simpler contracts, and the sixth, 'Close',
--   is a simple contract.
--   At each step of execution, as well as returning a new state and continuation contract,
--   it is possible that effects – payments – and warnings can be generated too.
data Contract
  = Close
  | Pay AccountId Payee Token (Value Observation) Contract
  | If Observation Contract Contract
  | When [Case Contract] Timeout Contract
  | Let ValueId (Value Observation) Contract
  | Assert Observation Contract
  deriving stock (H.Show, Generic, H.Eq, H.Ord)
  deriving anyclass (Pretty)

instance NFData Contract

-- | Marlowe contract internal state. Stored in a /Datum/ of a transaction output.
data State = State
  { accounts :: Accounts
  , choices :: Map ChoiceId ChosenNum
  , boundValues :: Map ValueId Integer
  , minTime :: POSIXTime
  }
  deriving stock (H.Show, H.Eq, Generic)

instance NFData State

-- | Execution environment. Contains a time interval of a transaction.
newtype Environment = Environment {timeInterval :: TimeInterval}
  deriving stock (H.Show, H.Eq, H.Ord)

instance FromJSON Environment where
  parseJSON =
    withObject
      "Environment"
      (\v -> Environment H.<$> (posixIntervalFromJSON =<< v .: "timeInterval"))

instance ToJSON Environment where
  toJSON Environment{..} =
    object
      ["timeInterval" .= posixIntervalToJSON timeInterval]

-- | Input for a Marlowe contract. Correspond to expected 'Action's.
data InputContent
  = IDeposit AccountId Party Token Integer
  | IChoice ChoiceId ChosenNum
  | INotify
  deriving stock (H.Show, H.Eq, Generic)
  deriving anyclass (Pretty)

instance NFData InputContent

instance FromJSON InputContent where
  parseJSON (String "input_notify") = return INotify
  parseJSON (Object v) =
    IChoice
      H.<$> v .: "for_choice_id"
      <*> v .: "input_that_chooses_num"
      <|> IDeposit
        H.<$> v .: "into_account"
        <*> v .: "input_from_party"
        <*> v .: "of_token"
        <*> v .: "that_deposits"
  parseJSON _ = H.fail "Input must be either an object or the string \"input_notify\""

instance ToJSON InputContent where
  toJSON (IDeposit accId party tok amount) =
    object
      [ "input_from_party" .= party
      , "that_deposits" .= amount
      , "of_token" .= tok
      , "into_account" .= accId
      ]
  toJSON (IChoice choiceId chosenNum) =
    object
      [ "input_that_chooses_num" .= chosenNum
      , "for_choice_id" .= choiceId
      ]
  toJSON INotify = JSON.String $ Text.pack "input_notify"

-- | Input to a contract, which may include the merkleized continuation
--   of the contract and its hash.
data Input
  = NormalInput InputContent
  | MerkleizedInput InputContent BuiltinByteString Contract
  deriving stock (H.Show, H.Eq, Generic)
  deriving anyclass (Pretty)

instance NFData Input

instance FromJSON Input where
  parseJSON (String s) = NormalInput H.<$> parseJSON (String s)
  parseJSON (Object v) = do
    let parseContinuationHash = do
          h <- v .: "continuation_hash"
          EncodeBase16 bs <- parseJSON h
          return $ toBuiltin bs
    MerkleizedInput H.<$> parseJSON (Object v) <*> parseContinuationHash <*> v .: "merkleized_continuation"
      <|> MerkleizedInput INotify H.<$> parseContinuationHash <*> v .: "merkleized_continuation"
      <|> NormalInput H.<$> parseJSON (Object v)
  parseJSON _ = H.fail "Input must be either an object or the string \"input_notify\""

instance ToJSON Input where
  toJSON (NormalInput content) = toJSON content
  toJSON (MerkleizedInput content hash continuation) =
    let obj = case toJSON content of
          Object o -> o
          _ -> KeyMap.empty
     in Object
          $ obj
          `KeyMap.union` KeyMap.fromList
            [ ("merkleized_continuation", toJSON continuation)
            , ("continuation_hash", toJSON $ EncodeBase16 $ fromBuiltin hash)
            ]

-- | Extract the content of input.
getInputContent :: Input -> InputContent
getInputContent (NormalInput inputContent) = inputContent
getInputContent (MerkleizedInput inputContent _ _) = inputContent

-- | Time interval errors.
--   'InvalidInterval' means @slotStart > slotEnd@, and
--   'IntervalInPastError' means time interval is in the past, relative to the contract.
--
--   These errors should never occur, but we are always prepared.
data IntervalError
  = InvalidInterval TimeInterval
  | IntervalInPastError POSIXTime TimeInterval
  deriving stock (H.Show, Generic, H.Eq)

instance NFData IntervalError

instance ToJSON IntervalError where
  toJSON (InvalidInterval (s, e)) =
    A.object
      [("invalidInterval", A.object [("from", posixTimeToJSON s), ("to", posixTimeToJSON e)])]
  toJSON (IntervalInPastError t (s, e)) =
    A.object
      [
        ( "intervalInPastError"
        , A.object [("minTime", posixTimeToJSON t), ("from", posixTimeToJSON s), ("to", posixTimeToJSON e)]
        )
      ]

instance FromJSON IntervalError where
  parseJSON (JSON.Object v) =
    let parseInvalidInterval = do
          o <- v .: "invalidInterval"
          InvalidInterval H.<$> posixIntervalFromJSON o
        parseIntervalInPastError = do
          o <- v .: "intervalInPastError"
          IntervalInPastError
            H.<$> (posixTimeFromJSON =<< o .: "minTime")
            <*> posixIntervalFromJSON o
     in parseIntervalInPastError <|> parseInvalidInterval
  parseJSON invalid =
    JSON.prependFailure "parsing IntervalError failed, " (JSON.typeMismatch "Object" invalid)

-- | Parse a JSON value as time.
posixTimeFromJSON :: JSON.Value -> Parser POSIXTime
posixTimeFromJSON = \case
  v@(JSON.Number n) ->
    either
      (\_ -> JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Integer" v))
      (return . POSIXTime)
      (floatingOrInteger n :: Either H.Double Integer)
  invalid ->
    JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Number" invalid)

posixIntervalFromJSON :: A.Object -> Parser TimeInterval
posixIntervalFromJSON o = (,) H.<$> (posixTimeFromJSON =<< o .: "from") <*> (posixTimeFromJSON =<< o .: "to")

-- | Serialise time as a JSON value.
posixTimeToJSON :: POSIXTime -> JSON.Value
posixTimeToJSON (POSIXTime n) = JSON.Number $ scientific n 0

posixIntervalToJSON :: TimeInterval -> JSON.Value
posixIntervalToJSON (from, to) =
  object
    [ "from" .= posixTimeToJSON from
    , "to" .= posixTimeToJSON to
    ]

-- | Result of 'fixInterval'
data IntervalResult
  = IntervalTrimmed Environment State
  | IntervalError IntervalError
  deriving stock (H.Show)

-- | Empty State for a given minimal 'POSIXTime'
emptyState :: POSIXTime -> State
emptyState sn =
  State
    { accounts = Map.empty
    , choices = Map.empty
    , boundValues = Map.empty
    , minTime = sn
    }

-- | Check if a 'num' is within a list of inclusive bounds.
inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = List.any (\(Bound l u) -> num >= l && num <= u)

instance FromJSON State where
  parseJSON =
    withObject
      "State"
      ( \v ->
          State
            H.<$> (v .: "accounts" >>= fromJSONAssocMap)
            <*> (v .: "choices" >>= fromJSONAssocMap)
            <*> (v .: "boundValues" >>= fromJSONAssocMap)
            <*> (POSIXTime H.<$> (withInteger "minTime" =<< (v .: "minTime")))
      )
instance ToJSON State where
  toJSON
    State
      { accounts = a
      , choices = c
      , boundValues = bv
      , minTime = POSIXTime ms
      } =
      object
        [ "accounts" .= toJSONAssocMap a
        , "choices" .= toJSONAssocMap c
        , "boundValues" .= toJSONAssocMap bv
        , "minTime" .= ms
        ]

-- | Serialise an association list to JSON.
toJSONAssocMap :: (ToJSON k) => (ToJSON v) => Map k v -> JSON.Value
toJSONAssocMap = toJSON . Map.toList

-- | Parse an association list from JSON.
fromJSONAssocMap :: (FromJSON k) => (FromJSON v) => JSON.Value -> JSON.Parser (Map k v)
fromJSONAssocMap v = Map.unsafeFromList H.<$> parseJSON v

instance FromJSON Party where
  parseJSON = withObject "Party" $ \v ->
    (maybe (parseFail "Address") (return . uncurry Address) . deserialiseAddressBech32 =<< (v .: "address"))
      <|> (Role . Val.tokenName . Text.encodeUtf8 H.<$> (v .: "role_token"))

instance ToJSON Party where
  toJSON (Address network address) =
    object
      ["address" .= serialiseAddressBech32 network address]
  toJSON (Role (Val.TokenName name)) =
    object
      ["role_token" .= (JSON.String $ Text.decodeUtf8 $ fromBuiltin name)]

instance ToJSONKey ChoiceId where
  toJSONKey = JSON.ToJSONKeyValue toJSON JSON.toEncoding

instance FromJSONKey ChoiceId where
  fromJSONKey = JSON.FromJSONKeyValue parseJSON

instance FromJSON ChoiceId where
  parseJSON =
    withObject
      "ChoiceId"
      ( \v ->
          ChoiceId
            H.<$> (toBuiltin . Text.encodeUtf8 H.<$> (v .: "choice_name"))
            <*> (v .: "choice_owner")
      )

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) =
    object
      [ "choice_name" .= (JSON.String $ Text.decodeUtf8 $ fromBuiltin name)
      , "choice_owner" .= party
      ]

instance FromJSON Token where
  parseJSON =
    withObject
      "Token"
      ( \v ->
          Token
            H.<$> do
              cs <- v .: "currency_symbol"
              EncodeBase16 bs <- parseJSON cs
              return $ Val.currencySymbol bs
            <*> (Val.tokenName . Text.encodeUtf8 H.<$> (v .: "token_name"))
      )

instance ToJSON Token where
  toJSON (Token currSym tokName) =
    object
      [ "currency_symbol" .= (toJSON $ EncodeBase16 $ fromBuiltin $ unCurrencySymbol currSym)
      , "token_name" .= (JSON.String $ Text.decodeUtf8 $ fromBuiltin $ unTokenName tokName)
      ]

instance FromJSON ValueId where
  parseJSON = withText "ValueId" $ return . ValueId . toBuiltin . Text.encodeUtf8

instance ToJSON ValueId where
  toJSON (ValueId x) = JSON.String (Text.decodeUtf8 $ fromBuiltin x)

instance FromJSON (Value Observation) where
  parseJSON (Object v) =
    ( AvailableMoney
        H.<$> (v .: "in_account")
        <*> (v .: "amount_of_token")
    )
      <|> (NegValue H.<$> (v .: "negate"))
      <|> ( AddValue
              H.<$> (v .: "add")
              <*> (v .: "and")
          )
      <|> ( SubValue
              H.<$> (v .: "value")
              <*> (v .: "minus")
          )
      <|> ( MulValue
              H.<$> (v .: "multiply")
              <*> (v .: "times")
          )
      <|> (DivValue H.<$> (v .: "divide") <*> (v .: "by"))
      <|> (ChoiceValue H.<$> (v .: "value_of_choice"))
      <|> (UseValue H.<$> (v .: "use_value"))
      <|> ( Cond
              H.<$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else")
          )
  parseJSON (String "time_interval_start") = return TimeIntervalStart
  parseJSON (String "time_interval_end") = return TimeIntervalEnd
  parseJSON (Number n) = Constant H.<$> getInteger "constant value" n
  parseJSON _ = H.fail "Value must be either an object or an integer"

instance ToJSON (Value Observation) where
  toJSON (AvailableMoney accountId token) =
    object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
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
  toJSON TimeIntervalStart = JSON.String $ Text.pack "time_interval_start"
  toJSON TimeIntervalEnd = JSON.String $ Text.pack "time_interval_end"
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
        H.<$> (v .: "both")
        <*> (v .: "and")
    )
      <|> ( OrObs
              H.<$> (v .: "either")
              <*> (v .: "or")
          )
      <|> (NotObs H.<$> (v .: "not"))
      <|> (ChoseSomething H.<$> (v .: "chose_something_for"))
      <|> ( ValueGE
              H.<$> (v .: "value")
              <*> (v .: "ge_than")
          )
      <|> ( ValueGT
              H.<$> (v .: "value")
              <*> (v .: "gt")
          )
      <|> ( ValueLT
              H.<$> (v .: "value")
              <*> (v .: "lt")
          )
      <|> ( ValueLE
              H.<$> (v .: "value")
              <*> (v .: "le_than")
          )
      <|> ( ValueEQ
              H.<$> (v .: "value")
              <*> (v .: "equal_to")
          )
  parseJSON _ = H.fail "Observation must be either an object or a boolean"

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

instance FromJSON Bound where
  parseJSON =
    withObject
      "Bound"
      ( \v ->
          Bound
            H.<$> (getInteger "lower bound" =<< (v .: "from"))
            <*> (getInteger "higher bound" =<< (v .: "to"))
      )
instance ToJSON Bound where
  toJSON (Bound from to) =
    object
      [ "from" .= from
      , "to" .= to
      ]

instance FromJSON Action where
  parseJSON =
    withObject
      "Action"
      ( \v ->
          ( Deposit
              H.<$> (v .: "into_account")
              <*> (v .: "party")
              <*> (v .: "of_token")
              <*> (v .: "deposits")
          )
            <|> ( Choice
                    H.<$> (v .: "for_choice")
                    <*> ( (v .: "choose_between")
                            >>= withArray
                              "Bound list"
                              ( \bl ->
                                  H.mapM parseJSON (F.toList bl)
                              )
                        )
                )
            <|> (Notify H.<$> (v .: "notify_if"))
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
      , "choose_between" .= toJSONList (H.map toJSON bounds)
      ]
  toJSON (Notify obs) =
    object
      ["notify_if" .= obs]

instance FromJSON Payee where
  parseJSON =
    withObject
      "Payee"
      ( \v ->
          (Account H.<$> (v .: "account"))
            <|> (Party H.<$> (v .: "party"))
      )

instance ToJSON Payee where
  toJSON (Account acc) = object ["account" .= acc]
  toJSON (Party party) = object ["party" .= party]

instance (FromJSON a) => FromJSON (Case a) where
  parseJSON = withObject "Case" \v ->
    Case H.<$> (v .: "case") <*> (v .: "then")
      <|> MerkleizedCase
        H.<$> v .: "case"
        <*> do
          mt <- v .: "merkleized_then"
          EncodeBase16 bs <- parseJSON mt
          return $ toBuiltin bs

instance (ToJSON a) => ToJSON (Case a) where
  toJSON (Case act cont) =
    object
      [ "case" .= act
      , "then" .= cont
      ]
  toJSON (MerkleizedCase act bs) =
    object
      [ "case" .= act
      , "merkleized_then" .= (Base16.Aeson.byteStringToJSON $ fromBuiltin bs)
      ]

instance FromJSON Contract where
  parseJSON (String "close") = return Close
  parseJSON (Object v) =
    ( Pay
        H.<$> (v .: "from_account")
        <*> (v .: "to")
        <*> (v .: "token")
        <*> (v .: "pay")
        <*> (v .: "then")
    )
      <|> ( If
              H.<$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else")
          )
      <|> ( When
              H.<$> ( (v .: "when")
                        >>= withArray
                          "Case list"
                          ( \cl ->
                              H.mapM parseJSON (F.toList cl)
                          )
                    )
              <*> (POSIXTime H.<$> (withInteger "when timeout" =<< (v .: "timeout")))
              <*> (v .: "timeout_continuation")
          )
      <|> ( Let
              H.<$> (v .: "let")
              <*> (v .: "be")
              <*> (v .: "then")
          )
      <|> ( Assert
              H.<$> (v .: "assert")
              <*> (v .: "then")
          )
  parseJSON _ = H.fail "Contract must be either an object or a the string \"close\""

instance ToJSON Contract where
  toJSON Close = JSON.String $ Text.pack "close"
  toJSON (Pay accountId payee token value contract) =
    object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= contract
      ]
  toJSON (If obs cont1 cont2) =
    object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When caseList timeout cont) =
    object
      [ "when" .= toJSONList (H.map toJSON caseList)
      , "timeout" .= getPOSIXTime timeout
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

instance Eq Party where
  {-# INLINEABLE (==) #-}
  Address n1 a1 == Address n2 a2 = n1 == n2 && a1 == a2
  Address _ _ == _ = False
  Role r1 == Role r2 = r1 == r2
  Role _ == _ = False

instance Eq ChoiceId where
  {-# INLINEABLE (==) #-}
  ChoiceId n1 p1 == ChoiceId n2 p2 = n1 == n2 && p1 == p2

instance Eq Token where
  {-# INLINEABLE (==) #-}
  Token n1 p1 == Token n2 p2 = n1 == n2 && p1 == p2

instance Eq ValueId where
  {-# INLINEABLE (==) #-}
  ValueId n1 == ValueId n2 = n1 == n2

instance Pretty ValueId where
  prettyFragment (ValueId n) = prettyFragment n

instance Eq Payee where
  {-# INLINEABLE (==) #-}
  Account acc1 == Account acc2 = acc1 == acc2
  Account{} == _ = False
  Party p1 == Party p2 = p1 == p2
  Party{} == _ = False

instance (Eq a) => Eq (Value a) where
  {-# INLINEABLE (==) #-}
  AvailableMoney acc1 tok1 == AvailableMoney acc2 tok2 = acc1 == acc2 && tok1 == tok2
  AvailableMoney _ _ == _ = False
  Constant i1 == Constant i2 = i1 == i2
  Constant{} == _ = False
  NegValue val1 == NegValue val2 = val1 == val2
  NegValue{} == _ = False
  AddValue val1 val2 == AddValue val3 val4 = val1 == val3 && val2 == val4
  AddValue{} == _ = False
  SubValue val1 val2 == SubValue val3 val4 = val1 == val3 && val2 == val4
  SubValue{} == _ = False
  MulValue val1 val2 == MulValue val3 val4 = val1 == val3 && val2 == val4
  MulValue{} == _ = False
  DivValue val1 val2 == DivValue val3 val4 = val1 == val3 && val2 == val4
  DivValue{} == _ = False
  ChoiceValue cid1 == ChoiceValue cid2 = cid1 == cid2
  ChoiceValue{} == _ = False
  TimeIntervalStart == TimeIntervalStart = True
  TimeIntervalStart == _ = False
  TimeIntervalEnd == TimeIntervalEnd = True
  TimeIntervalEnd == _ = False
  UseValue val1 == UseValue val2 = val1 == val2
  UseValue{} == _ = False
  Cond obs1 thn1 els1 == Cond obs2 thn2 els2 = obs1 == obs2 && thn1 == thn2 && els1 == els2
  Cond{} == _ = False

instance Eq Observation where
  {-# INLINEABLE (==) #-}
  AndObs o1l o2l == AndObs o1r o2r = o1l == o1r && o2l == o2r
  AndObs{} == _ = False
  OrObs o1l o2l == OrObs o1r o2r = o1l == o1r && o2l == o2r
  OrObs{} == _ = False
  NotObs a == NotObs b = a == b
  NotObs{} == _ = False
  ChoseSomething cid1 == ChoseSomething cid2 = cid1 == cid2
  ChoseSomething _ == _ = False
  ValueGE v1l v2l == ValueGE v1r v2r = v1l == v1r && v2l == v2r
  ValueGE{} == _ = False
  ValueGT v1l v2l == ValueGT v1r v2r = v1l == v1r && v2l == v2r
  ValueGT{} == _ = False
  ValueLT v1l v2l == ValueLT v1r v2r = v1l == v1r && v2l == v2r
  ValueLT{} == _ = False
  ValueLE v1l v2l == ValueLE v1r v2r = v1l == v1r && v2l == v2r
  ValueLE{} == _ = False
  ValueEQ v1l v2l == ValueEQ v1r v2r = v1l == v1r && v2l == v2r
  ValueEQ{} == _ = False
  TrueObs == TrueObs = True
  TrueObs == _ = False
  FalseObs == FalseObs = True
  FalseObs == _ = False

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Deposit acc1 party1 tok1 val1 == Deposit acc2 party2 tok2 val2 =
    acc1 == acc2 && party1 == party2 && tok1 == tok2 && val1 == val2
  Deposit{} == _ = False
  Choice cid1 bounds1 == Choice cid2 bounds2 =
    cid1
      == cid2
      && List.length bounds1
      == List.length bounds2
      && let bounds = List.zip bounds1 bounds2
             checkBound (Bound low1 high1, Bound low2 high2) = low1 == low2 && high1 == high2
          in List.all checkBound bounds
  Choice{} == _ = False
  Notify obs1 == Notify obs2 = obs1 == obs2
  Notify{} == _ = False

instance (Eq a) => Eq (Case a) where
  {-# INLINEABLE (==) #-}
  Case acl cl == Case acr cr = acl == acr && cl == cr
  Case{} == _ = False
  MerkleizedCase acl bsl == MerkleizedCase acr bsr = acl == acr && bsl == bsr
  MerkleizedCase{} == _ = False

instance Eq Contract where
  {-# INLINEABLE (==) #-}
  Close == Close = True
  Close == _ = False
  Pay acc1 payee1 tok1 value1 cont1 == Pay acc2 payee2 tok2 value2 cont2 =
    acc1 == acc2 && payee1 == payee2 && tok1 == tok2 && value1 == value2 && cont1 == cont2
  Pay{} == _ = False
  If obs1 cont1 cont2 == If obs2 cont3 cont4 =
    obs1 == obs2 && cont1 == cont3 && cont2 == cont4
  If{} == _ = False
  When cases1 timeout1 cont1 == When cases2 timeout2 cont2 =
    -- The sequences of tests are ordered for efficiency.
    timeout1
      == timeout2
      && cont1
      == cont2
      && List.length cases1
      == List.length cases2
      && List.and (List.zipWith (==) cases1 cases2)
  When{} == _ = False
  Let valId1 val1 cont1 == Let valId2 val2 cont2 =
    valId1 == valId2 && val1 == val2 && cont1 == cont2
  Let{} == _ = False
  Assert obs1 cont1 == Assert obs2 cont2 = obs1 == obs2 && cont1 == cont2
  Assert{} == _ = False

instance Eq State where
  {-# INLINEABLE (==) #-}
  l == r =
    minTime l
      == minTime r
      && accounts l
      == accounts r
      && choices l
      == choices r
      && boundValues l
      == boundValues r

-- Lifting data types to Plutus Core
makeLift ''Party
makeIsDataIndexed ''Party [('Address, 0), ('Role, 1)]
makeLift ''ChoiceId
makeIsDataIndexed ''ChoiceId [('ChoiceId, 0)]
makeLift ''Token
makeIsDataIndexed ''Token [('Token, 0)]
makeLift ''ValueId
makeIsDataIndexed ''ValueId [('ValueId, 0)]
makeLift ''Value
makeIsDataIndexed
  ''Value
  [ ('AvailableMoney, 0)
  , ('Constant, 1)
  , ('NegValue, 2)
  , ('AddValue, 3)
  , ('SubValue, 4)
  , ('MulValue, 5)
  , ('DivValue, 6)
  , ('ChoiceValue, 7)
  , ('TimeIntervalStart, 8)
  , ('TimeIntervalEnd, 9)
  , ('UseValue, 10)
  , ('Cond, 11)
  ]
makeLift ''Observation
makeIsDataIndexed
  ''Observation
  [ ('AndObs, 0)
  , ('OrObs, 1)
  , ('NotObs, 2)
  , ('ChoseSomething, 3)
  , ('ValueGE, 4)
  , ('ValueGT, 5)
  , ('ValueLT, 6)
  , ('ValueLE, 7)
  , ('ValueEQ, 8)
  , ('TrueObs, 9)
  , ('FalseObs, 10)
  ]
makeLift ''Bound
makeIsDataIndexed ''Bound [('Bound, 0)]
makeLift ''Action
makeIsDataIndexed ''Action [('Deposit, 0), ('Choice, 1), ('Notify, 2)]
makeLift ''Case
makeIsDataIndexed ''Case [('Case, 0), ('MerkleizedCase, 1)]
makeLift ''Payee
makeIsDataIndexed ''Payee [('Account, 0), ('Party, 1)]
makeLift ''Contract
makeIsDataIndexed
  ''Contract
  [ ('Close, 0)
  , ('Pay, 1)
  , ('If, 2)
  , ('When, 3)
  , ('Let, 4)
  , ('Assert, 5)
  ]
makeLift ''State
makeIsDataIndexed ''State [('State, 0)]
makeLift ''Environment
makeLift ''InputContent
makeIsDataIndexed ''InputContent [('IDeposit, 0), ('IChoice, 1), ('INotify, 2)]
makeLift ''Input
makeIsDataIndexed ''Input [('NormalInput, 0), ('MerkleizedInput, 1)]
