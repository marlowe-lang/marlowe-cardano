{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | CIP-0057 blueprint generation for the Marlowe validators.
--
-- Generates a machine-readable Plutus contract blueprint (CIP-0057) from the
-- Marlowe datum and redeemer types. The blueprint is the authoritative encoding
-- specification referenced by the Marlowe Oracle Protocol CIP appendix.
--
-- The on-chain encoding is unchanged: every type uses the same constructor
-- indices as the existing @makeIsDataIndexed@ calls in @marlowe-plutus@, so
-- deriving the schema here alongside the data instances does not alter the
-- Data representation. We use 'makeHasSchemaInstance' (lower-level helper from
-- 'PlutusTx.Blueprint.TH') instead of 'makeIsDataSchemaIndexed' precisely
-- because the latter would re-derive 'ToData'/'FromData' and clash with the
-- instances already produced in @marlowe-plutus@.
--
-- Build (from the @marlowe-plutus@ dev shell, with the @asdata-case@ flag ON
-- so the generated schema matches production):
--
--     cabal run marlowe-binaries -- blueprint plutus.json
--
-- The output file can be validated against the CIP-0057 meta-schema at
-- https://cips.cardano.org/cip/CIP-0057/schemas/plutus-blueprint.json
-- and inspected with any CIP-0057-aware tool (e.g. Aiken's blueprint import).
module Marlowe.Plutus.Binaries.Blueprint (
  contractBlueprint,
  writeBlueprintToFile,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import PlutusLedgerApi.V3 (CurrencySymbol, ScriptHash, TokenName)
import PlutusTx.AssocMap (Map)
import PlutusTx.Blueprint
import PlutusTx.Blueprint.TH (makeHasSchemaInstance)
import PlutusTx.Builtins (BuiltinByteString)

import Paths_marlowe_plutus (version)

import Marlowe.Plutus.Binaries.Production
  ( marloweValidatorBytes,
    rolePayoutValidatorBytes,
  )
import Marlowe.Plutus.RoleTokens.Types ()
import Marlowe.Plutus.Scripts.Types (MarloweTxInput (..))
import Marlowe.Plutus.Semantics (MarloweData (..), MarloweParams (..))
import Marlowe.Plutus.Semantics.Types
  ( Action (..),
    Bound (..),
    Case (..),
    ChoiceId (..),
    Contract (..),
    InputContent (..),
    Observation (..),
    Party (..),
    Payee (..),
    State (..),
    Token (..),
    Value (..),
    ValueId (..),
  )

-- | Top-level types referenced by 'contractBlueprint'. Both
-- 'contractValidators' and 'contractDefinitions' use this list so that the
-- resulting 'ContractBlueprint' carries a 'referencedTypes' phantom that
-- covers every nested field the validator schema generates a
-- 'definitionRef' for. 'UnrollAll' walks the full transitive type list at
-- the type level.
--
-- @Value Contract@ must be listed explicitly: the 'Case' schema references
-- @(Value a)@ which, instantiated with @a = Contract@, materialises as
-- @Value Contract@. Without listing it, 'deriveDefinitions' only sees
-- @Value Observation@ (the other 'Value' instantiation) and the
-- @Value<Contract>@ @\$ref@ in the output has no matching definition.
-- @[Case Contract]@ is similarly listed so the 'Contract' schema (whose
-- @When@ field is @[Case Contract]@) can find a matching definition.
type BlueprintTopLevel =
  '[ MarloweData
   , MarloweParams
   , State
   , Contract
   , ChoiceId
   , InputContent
   , MarloweTxInput
   , [MarloweTxInput]
   , Party
   , Token
   , Value Observation
   , Value Contract
   , Observation
   , Bound
   , Action
   , Payee
   , ValueId
   , [Case Contract]
   , (CurrencySymbol, TokenName)
   , ScriptHash
   , ()
   ]

-- | The 'referencedTypes' phantom list of 'ContractBlueprint'. Same as
-- 'UnrollAll' 'BlueprintTopLevel' but pre-applied so the validator entries
-- can share the type with 'contractDefinitions'.
type BlueprintTypes = UnrollAll BlueprintTopLevel

-- | Empty 'HasBlueprintDefinition' instances for types in @marlowe-plutus@
-- that do not derive the class themselves. The default 'Unroll' walks the
-- 'Generic' representation and the default 'definitionId' uses 'Typeable';
-- every type below already derives 'Generic', so the default works.
--
-- The default 'Semigroup DefinitionId' joiner in Plutus emits the legacy
-- CIP-0057 underscore form (e.g. @Value_Contract@). Modern form
-- (@Value\<Contract\>@) is restored via JSON post-processing in
-- 'writeBlueprintToFile', which keeps this module's logic purely on the
-- Plutus-provided defaults.
instance HasBlueprintDefinition MarloweParams
instance HasBlueprintDefinition MarloweData
instance HasBlueprintDefinition State
-- 'Contract' is recursive ('When [Case Contract]', etc.). The default 'Unroll'
-- walks the 'Generic' encoding and would not terminate, so we list only the
-- type itself.
instance HasBlueprintDefinition Contract where
  type Unroll Contract = '[Contract]
instance HasBlueprintDefinition ChoiceId
instance HasBlueprintDefinition InputContent
instance HasBlueprintDefinition MarloweTxInput
instance HasBlueprintDefinition Party
instance HasBlueprintDefinition Token
-- 'Value' is polymorphic and recursive. List it as a closed type so its
-- 'Unroll' does not recurse through the field types. We override
-- 'definitionId' so the polymorphic instance compiles (the default uses
-- 'Typeable (Value a)' which is unavailable here). The legacy
-- underscore form @Value_a@ is rewritten to the modern CIP-0057
-- form @Value\<a\>@ by 'rewriteBlueprintToModernForm' after
-- 'writeBlueprint'.
instance HasBlueprintDefinition a => HasBlueprintDefinition (Value a) where
  type Unroll (Value a) = '[Value a]
  definitionId = definitionIdFromTypeK @(Type -> Type) @Value <> definitionId @a
-- 'Observation' is recursive ('AndObs Observation Observation', etc.).
instance HasBlueprintDefinition Observation where
  type Unroll Observation = '[Observation]
instance HasBlueprintDefinition Bound
instance HasBlueprintDefinition Action
instance HasBlueprintDefinition Payee
-- 'Case' is polymorphic and recursive; see note on 'Value' above.
instance HasBlueprintDefinition a => HasBlueprintDefinition (Case a) where
  type Unroll (Case a) = '[Case a]
  definitionId = definitionIdFromTypeK @(Type -> Type) @Case <> definitionId @a
instance HasBlueprintDefinition ValueId

-- * Schema derivation
--
-- Each splice emits only the 'HasBlueprintSchema' instance for the given type
-- using the same constructor indices as 'makeIsDataIndexed' in
-- @marlowe-plutus@. The existing 'ToData'/'FromData'/'UnsafeFromData'
-- instances stay where they are; we add the blueprint schema next to them
-- without touching the data instances.

$(makeHasSchemaInstance ''MarloweParams [('MarloweParams, 0)])
$(makeHasSchemaInstance ''MarloweData [('MarloweData, 0)])
$(makeHasSchemaInstance ''State [('State, 0)])
$(makeHasSchemaInstance ''Contract
    [ ('Close, 0), ('Pay, 1), ('If, 2), ('When, 3), ('Let, 4), ('Assert, 5)
    ])
$(makeHasSchemaInstance ''ChoiceId [('ChoiceId, 0)])
$(makeHasSchemaInstance ''InputContent [('IDeposit, 0), ('IChoice, 1), ('INotify, 2)])
$(makeHasSchemaInstance ''MarloweTxInput [('Input, 0), ('MerkleizedTxInput, 1)])
$(makeHasSchemaInstance ''Party [('Address, 0), ('Role, 1)])
$(makeHasSchemaInstance ''Token [('Token, 0)])
$(makeHasSchemaInstance ''Value
    [ ('AvailableMoney, 0), ('Constant, 1), ('NegValue, 2), ('AddValue, 3)
    , ('SubValue, 4), ('MulValue, 5), ('DivValue, 6), ('ChoiceValue, 7)
    , ('TimeIntervalStart, 8), ('TimeIntervalEnd, 9), ('UseValue, 10), ('Cond, 11)
    ])
$(makeHasSchemaInstance ''Observation
    [ ('AndObs, 0), ('OrObs, 1), ('NotObs, 2), ('ChoseSomething, 3)
    , ('ValueGE, 4), ('ValueGT, 5), ('ValueLT, 6), ('ValueLE, 7)
    , ('ValueEQ, 8), ('TrueObs, 9), ('FalseObs, 10)
    ])
$(makeHasSchemaInstance ''Bound [('Bound, 0)])

-- Hand-written override for `[Bound]`. Plutus's default `[a]` instance in
-- `PlutusTx.Blueprint.Class` inlines `schema @a` for list items, which
-- produces an anonymous constructor schema in the JSON output. We want
-- the consumer (e.g. Aiken) to see the named type "Bound" via `$ref` instead.
instance {-# OVERLAPPING #-} HasBlueprintSchema [Bound] referencedTypes where
  schema =
    SchemaList emptySchemaInfo
      MkListSchema
        { minItems = Nothing
        , maxItems = Nothing
        , uniqueItems = Nothing
        , itemSchema = definitionRef @Bound
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema [MarloweTxInput] referencedTypes where
  schema =
    SchemaList emptySchemaInfo
      MkListSchema
        { minItems = Nothing
        , maxItems = Nothing
        , uniqueItems = Nothing
        , itemSchema = definitionRef @MarloweTxInput
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema [Case Contract] referencedTypes where
  schema =
    SchemaList emptySchemaInfo
      MkListSchema
        { minItems = Nothing
        , maxItems = Nothing
        , uniqueItems = Nothing
        , itemSchema = definitionRef @(Case Contract)
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema (CurrencySymbol, TokenName) referencedTypes where
  schema =
    SchemaBuiltInPair emptySchemaInfo
      MkPairSchema
        { left = definitionRef @CurrencySymbol
        , right = definitionRef @TokenName
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema (Party, Token) referencedTypes where
  schema =
    SchemaBuiltInPair emptySchemaInfo
      MkPairSchema
        { left = definitionRef @Party
        , right = definitionRef @Token
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema (Map ChoiceId Integer) referencedTypes where
  schema =
    SchemaMap emptySchemaInfo
      MkMapSchema
        { minItems = Nothing
        , maxItems = Nothing
        , keySchema = definitionRef @ChoiceId
        , valueSchema = definitionRef @Integer
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema (Map ValueId Integer) referencedTypes where
  schema =
    SchemaMap emptySchemaInfo
      MkMapSchema
        { minItems = Nothing
        , maxItems = Nothing
        , keySchema = definitionRef @ValueId
        , valueSchema = definitionRef @Integer
        }

-- See "Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v" below.
instance {-# OVERLAPPING #-} HasBlueprintSchema (Map (Party, Token) Integer) referencedTypes where
  schema =
    SchemaMap emptySchemaInfo
      MkMapSchema
        { minItems = Nothing
        , maxItems = Nothing
        , keySchema = definitionRef @(Party, Token)
        , valueSchema = definitionRef @Integer
        }

-- Hand-written OVERLAPPING instances for [a]/(a,b)/Map k v
-- ---------------------------------------------------------------------------
-- Plutus's default instances for these parametric types in
-- @PlutusTx.Blueprint.Class@ (for @[a]@, @(a,b)@, @BuiltinList@, @BuiltinPair@)
-- and in @PlutusTx.AssocMap@ (for @Map k v@) use
-- @itemSchema = schema @a@ / @keySchema = schema @k@ / @valueSchema = schema @v@
-- — that is, they inline the element schema rather than producing a
-- @$ref: "#/definitions/<a>"@. As a result, consumers like Aiken cannot see
-- the named type for list elements, and we get unreferenced defs like
-- @Bound@, @Input@, @MarloweTxInput@, @Case<Contract>@, @Tuple2<Party,Token>@.
--
-- We override each container we actually use with a hand-written instance
-- that emits a @$ref@. The @{-# OVERLAPPING #-}@ pragma makes ours win
-- against Plutus's `[a]`/`(a,b)`/`Map k v` head — these heads are more
-- specific than the generic `[a]`, so GHC permits it.
--
-- Falling back to a JSON post-process would also work, but it's nicer
-- to fix this at the Haskell source — typed consumers can introspect
-- the schema and immediately know "this is a list of Bound", rather than
-- "this is a list of an anonymous constructor schema".

$(makeHasSchemaInstance ''Action [('Deposit, 0), ('Choice, 1), ('Notify, 2)])
$(makeHasSchemaInstance ''Payee [('Account, 0), ('Party, 1)])
-- The 'Case' type is defined via 'PlutusTx.AsData.asData' when the
-- @asdata-case@ flag is on, which turns the data constructors into pattern
-- synonyms. 'makeHasSchemaInstance' introspects the real constructor names
-- via Template Haskell, so it can't find pattern synonyms and would fail. The
-- on-chain encoding is unchanged: case index 0 carries an action and the
-- continuation, case index 1 carries an action and a merkle hash. We build
-- the schema by hand instead.
instance
  HasBlueprintDefinition a =>
  HasBlueprintSchema (Case a) referencedTypes
  where
  schema =
    SchemaOneOf $
      NE.fromList
        [ SchemaConstructor emptySchemaInfo $
            MkConstructorSchema 0
              [ definitionRef @Action
              , definitionRef @(Value a)
              ]
        , SchemaConstructor emptySchemaInfo $
            MkConstructorSchema 1
              [ definitionRef @Action
              , definitionRef @BuiltinByteString
              ]
        ]
$(makeHasSchemaInstance ''ValueId [('ValueId, 0)])
-- 'RoleTokens' and 'MintAction' belong to the role-token minting policy, not
-- the semantics or role-payout validators. Their constructors are not in
-- scope outside of the policy module, so we don't try to derive schemas for
-- them here.

-- * Blueprint document
--
-- @MarloweInput@ is a type synonym for @[MarloweTxInput]@ and needs no
-- schema splice — the list instance covers it. Listing @[MarloweTxInput]@
-- in 'deriveDefinitions' rolls out the redeemer underneath.

contractBlueprint :: ContractBlueprint
contractBlueprint =
  MkContractBlueprint
    { contractId = Just "marlowe-v1"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Marlowe V1"
          , preambleDescription =
              Just
                "Plutus V3 validators for the Marlowe financial contract DSL. \
                \Datum and redeemer schemas are derived from the Plinth types \
                \in marlowe-plutus and are guaranteed to match the on-chain \
                \Data encoding."
          , preambleVersion = T.pack (showVersion version)
          , preamblePlutusVersion = PlutusV3
          , preambleLicense = Just "Apache-2.0"
          }
    , contractDefinitions =
        deriveDefinitions @BlueprintTopLevel
    , contractValidators =
        Set.fromList [marloweSemanticsValidator, rolePayoutValidator]
    }

marloweSemanticsValidator :: ValidatorBlueprint BlueprintTypes
marloweSemanticsValidator =
  MkValidatorBlueprint
    { validatorTitle = "marlowe.semantics.spend"
    , validatorDescription =
        Just
          "Marlowe semantics validator. Datum is the current MarloweData \
          \(parameters, state, continuation contract); redeemer is the list \
          \of transaction inputs (deposits, choices, notifications)."
    , validatorDatum = Just semanticsDatum
    , validatorRedeemer = semanticsRedeemer
    , validatorParameters = [scriptHashParameter]
    , validatorCompiled = Just (compiledValidator PlutusV3 (SBS.fromShort marloweValidatorBytes))
    }

rolePayoutValidator :: ValidatorBlueprint BlueprintTypes
rolePayoutValidator =
  MkValidatorBlueprint
    { validatorTitle = "marlowe.role-payout.spend"
    , validatorDescription =
        Just
          "Role payout validator. Datum is the (currency symbol, role token \
          \name) pair authorising withdrawal; unit redeemer."
    , validatorDatum = Just rolePayoutDatum
    , validatorRedeemer = rolePayoutRedeemer
    , validatorParameters = []
    , validatorCompiled = Just (compiledValidator PlutusV3 (SBS.fromShort rolePayoutValidatorBytes))
    }

semanticsDatum :: ArgumentBlueprint BlueprintTypes
semanticsDatum =
  MkArgumentBlueprint
    { argumentTitle = Just "MarloweData"
    , argumentDescription = Just "Current Marlowe contract state: parameters, accounts, choices, bindings, continuation."
    , argumentPurpose = Set.singleton Spend
    , argumentSchema = definitionRef @MarloweData
    }

semanticsRedeemer :: ArgumentBlueprint BlueprintTypes
semanticsRedeemer =
  MkArgumentBlueprint
    { argumentTitle = Just "MarloweInput"
    , argumentDescription = Just "List of Marlowe transaction inputs applied in this transaction."
    , argumentPurpose = Set.singleton Spend
    , argumentSchema = definitionRef @[MarloweTxInput]
    }

scriptHashParameter :: ParameterBlueprint BlueprintTypes
scriptHashParameter =
  MkParameterBlueprint
    { parameterTitle = Just "RolePayoutValidatorHash"
    , parameterDescription = Just "Script hash of the role payout validator."
    , parameterPurpose = Set.singleton Spend
    , parameterSchema = definitionRef @ScriptHash
    }

rolePayoutDatum :: ArgumentBlueprint BlueprintTypes
rolePayoutDatum =
  MkArgumentBlueprint
    { argumentTitle = Just "RolePayoutToken"
    , argumentDescription = Just "Currency symbol of the role-token policy and the role token name authorising withdrawal."
    , argumentPurpose = Set.singleton Spend
    , argumentSchema = definitionRef @(CurrencySymbol, TokenName)
    }

rolePayoutRedeemer :: ArgumentBlueprint BlueprintTypes
rolePayoutRedeemer =
  MkArgumentBlueprint
    { argumentTitle = Just "Unit"
    , argumentDescription = Just "Unit redeemer."
    , argumentPurpose = Set.singleton Spend
    , argumentSchema = definitionRef @()
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile fp = do
  writeBlueprint fp contractBlueprint
  rewriteBlueprintToModernForm fp

-- | Rewrite a CIP-0057 blueprint on disk to use modern parametric type
-- names (e.g. @Value\<Contract\>@, @List\<MarloweTxInput\>@,
-- @Tuple2\<CurrencySymbol,TokenName\>@) instead of the legacy underscore
-- form (@Value_Contract@, @List_MarloweTxInput@,
-- @Tuple2_CurrencySymbol_TokenName@) that the default 'Semigroup'
-- joiner in @PlutusTx.Blueprint.Definition.Id@ produces. Walks every
-- @\$ref@ string and every key of the @definitions@ object.
--
-- Why post-process rather than override 'definitionId'? Plutus already
-- exports instances for @[a]@, @(a, b)@, @(a, b, c)@ that build
-- 'DefinitionId' via the underscore 'Semigroup'. Orphan instances would
-- collide. JSON post-processing is uniform across all keys, requires no
-- 'unsafeCoerce', and works regardless of upstream Plutus changes.
rewriteBlueprintToModernForm :: FilePath -> IO ()
rewriteBlueprintToModernForm fp = do
  -- Read with the strict 'Data.ByteString' API. The lazy variant leaves
  -- the file handle open during decoding, which on some filesystems
  -- (and on Linux with at least @/tmp@) surfaces as @EBUSY@ when the
  -- subsequent @writeFile@ tries to open the same path. Strict reading
  -- fully closes the file before we attempt the write.
  contents <- BS.readFile fp
  let parsed = case Aeson.decode (LBS.fromStrict contents) of
        Just v -> v
        Nothing ->
          error "rewriteBlueprintToModernForm: generated blueprint is not valid JSON."
      rewritten = rewriteValue parsed
  BS.writeFile fp (LBS.toStrict (Aeson.encode rewritten))

-- | Apply the modern-form rewrite to a JSON value, recursing into nested
-- objects and arrays. String values that look like a definition
-- reference (@#/definitions/X@) have their @X@ part rewritten.
rewriteValue :: Aeson.Value -> Aeson.Value
rewriteValue = \case
  Aeson.Object obj ->
    Aeson.Object $
      Aeson.KeyMap.fromList
        [ (rewriteKeyText k, rewriteValue v)
          | (k, v) <- Aeson.KeyMap.toList obj
        ]
  Aeson.Array vs -> Aeson.Array (fmap rewriteValue vs)
  Aeson.String t
    | "#/definitions/" `T.isPrefixOf` t ->
        let suffix = T.drop (T.length "#/definitions/") t
         in Aeson.String
                  ("#/definitions/" <> rewriteDefinitionKeyText suffix)
    | otherwise -> Aeson.String t
  other -> other

-- | Apply the modern-form rewrite to a JSON object key.
rewriteKeyText :: Aeson.Key.Key -> Aeson.Key.Key
rewriteKeyText k =
  Aeson.Key.fromText (rewriteDefinitionKeyText (Aeson.Key.toText k))

-- | Rewrite a single legacy definition key into modern CIP-0057 form.
--
-- The legacy form is the underscore-joined result of the 'Semigroup'
-- instance on 'DefinitionId'. We split on @_@ and reconstruct using the
-- parametric type constructor's arity, recursing into nested parametric
-- arguments:
--
-- > Value_Contract              -> Value<Contract>
-- > List_MarloweTxInput         -> List<MarloweTxInput>
-- > Tuple2_CurrencySymbol_TokenName -> Tuple2<CurrencySymbol,TokenName>
-- > Tuple2_Party_Token          -> Tuple2<Party,Token>
-- > Map_ChoiceId_Integer        -> Map<ChoiceId,Integer>
-- > Map_Tuple2_Party_Token_Integer
--   -> Map<Tuple2<Party,Token>,Integer>
-- > Case_Contract               -> Case<Contract>
-- > List_Case_Contract          -> List<Case<Contract>>
--
-- Non-parametric keys pass through unchanged (e.g. @MarloweData@,
-- @Contract@, @Bool@).
rewriteDefinitionKeyText :: Text -> Text
rewriteDefinitionKeyText txt =
  if not (T.isInfixOf "_" txt)
    then txt
    else
      case T.splitOn "_" txt of
        [] -> txt
        [single] -> single
        prefix : rest ->
          let arity = arityOf prefix
           in if arity == 0
                then txt
                else
                  case consumeArgs arity rest of
                    Nothing -> txt
                    Just (args, []) ->
                      prefix <> "<" <> T.intercalate "," args <> ">"
                    Just (_, _ : _) -> txt -- leftover => bail out

-- | Arity of the parametric type constructor named by a segment, or @0@
-- for non-parametric types / unknown prefixes.
arityOf :: Text -> Int
arityOf = \case
  "Value" -> 1
  "Case" -> 1
  "List" -> 1
  "Tuple2" -> 2
  "Tuple3" -> 3
  "Map" -> 2
  _ -> 0

-- | Consume @n@ argument segments off the head of @parts@, where each
-- argument is itself either a non-parametric type (one segment) or a
-- parametric type (one segment for the constructor plus its arity-many
-- segments for its own arguments). On failure, return 'Nothing'.
consumeArgs :: Int -> [Text] -> Maybe ([Text], [Text])
consumeArgs 0 rest = Just ([], rest)
consumeArgs _ [] = Nothing
consumeArgs n (first : rest)
  | subArity == 0 =
      -- non-parametric: take just this segment
      case consumeArgs (n - 1) rest of
        Nothing -> Nothing
        Just (moreArgs, leftover) -> Just (first : moreArgs, leftover)
  | countList rest >= subArity =
      -- parametric: take @subArity + 1@ segments and rewrite them
      -- into a modern-form argument
      let (subArgs, subRest) = splitAtList subArity rest
       in case consumeArgs (n - 1) subRest of
            Nothing -> Nothing
            Just (moreArgs, leftover) ->
              let arg = first <> "<" <> T.intercalate "," subArgs <> ">"
               in Just (arg : moreArgs, leftover)
  | otherwise = Nothing
  where
    subArity = arityOf first

countList :: [a] -> Int
countList [] = 0
countList (_ : xs) = 1 + countList xs

splitAtList :: Int -> [a] -> ([a], [a])
splitAtList 0 xs = ([], xs)
splitAtList n (x : xs)
  | n > 0 =
      let (a, b) = splitAtList (n - 1) xs
       in (x : a, b)
  | otherwise = ([], x : xs)
splitAtList _ [] = ([], [])