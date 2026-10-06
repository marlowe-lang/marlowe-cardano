{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the request and response types in the Marlowe Runtime
-- | Web API.
module Language.Marlowe.Runtime.Web.Role.TokenFilter (
  RoleTokenFilter (..),
) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value (Array, Bool, Object),
  object,
  (<?>),
 )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPathElement (..), Parser, prependFailure, typeMismatch)
import Data.OpenApi (
  Definitions,
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  Schema,
  ToSchema,
  declareSchemaRef,
  enum_,
  oneOf,
  properties,
  required,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Schema (ToSchema (..))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified Language.Marlowe.Runtime.Transaction.Api as Chain
import Language.Marlowe.Runtime.Web.Core.Asset (
  AssetId,
  PolicyId,
 )
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef)
import Servant (
  Proxy (..),
 )
import Language.Marlowe.Runtime.Web.Server.DTO

data RoleTokenFilter
  = RoleTokensAnd RoleTokenFilter RoleTokenFilter
  | RoleTokensOr RoleTokenFilter RoleTokenFilter
  | RoleTokenNot RoleTokenFilter
  | RoleTokenFilterNone
  | RoleTokenFilterByContracts (Set TxOutRef)
  | RoleTokenFilterByPolicyIds (Set PolicyId)
  | RoleTokenFilterByTokens (Set AssetId)
  | RoleTokenFilterAny
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON RoleTokenFilter where
  toJSON = \case
    RoleTokensAnd a b -> object ["and" .= (a, b)]
    RoleTokensOr a b -> object ["or" .= (a, b)]
    RoleTokenNot a -> object ["not" .= a]
    RoleTokenFilterNone -> toJSON False
    RoleTokenFilterByContracts contracts -> object ["contract_id" .= contracts]
    RoleTokenFilterByPolicyIds policies -> object ["roles_currency" .= policies]
    RoleTokenFilterByTokens tokens -> object ["role_tokens" .= tokens]
    RoleTokenFilterAny -> toJSON True

instance FromJSON RoleTokenFilter where
  parseJSON =
    prependFailure "Parsing RoleTokenFilter failed" . \case
      Object o -> case KeyMap.toList o of
        [(k, v)] -> case k of
          "and" -> uncurry RoleTokensAnd <$> parseJSON v <?> Key "and"
          "or" -> uncurry RoleTokensOr <$> parseJSON v <?> Key "or"
          "not" -> RoleTokenNot <$> parseJSON v <?> Key "not"
          "contract_id" -> RoleTokenFilterByContracts <$> parseSetOrSingle v <?> Key "contract_id"
          "roles_currency" -> RoleTokenFilterByPolicyIds <$> parseSetOrSingle v <?> Key "roles_currency"
          "role_tokens" -> RoleTokenFilterByTokens <$> parseSetOrSingle v <?> Key "role_tokens"
          _ -> fail $ "Unexpected key: " <> show k
        _ -> fail "Unexpected number of keys, expected exactly 1."
      Bool True -> pure RoleTokenFilterAny
      Bool False -> pure RoleTokenFilterNone
      v -> typeMismatch "object|boolean" v

parseSetOrSingle :: (FromJSON a, Ord a) => Value -> Parser (Set a)
parseSetOrSingle = \case
  Array arr -> parseJSON $ Array arr
  v -> Set.singleton <$> parseJSON v

-- | Type only defined to provide a schema for the pair of sub-filters
data RoleTokenFilterPair

instance ToSchema RoleTokenFilterPair where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "RoleTokenFilterPair") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("fst", Ref (OpenApi.Reference "RoleTokenFilter"))
          , ("snd", Ref (OpenApi.Reference "RoleTokenFilter"))
          ]
      & required .~ ["fst", "snd"]

instance ToSchema RoleTokenFilter where
  declareNamedSchema _ = do
    roleTokenFilterPairSchema <- declareSchemaRef (Proxy @RoleTokenFilterPair)
    let
      roleTokenFilterSchema = Ref (OpenApi.Reference "RoleTokenFilter")
      setOrSingleSchema
            :: forall a
             . (ToSchema a)
            => Proxy a
            -> Declare (Definitions Schema) (Referenced Schema)
      setOrSingleSchema p = do
        singleSchema <- declareSchemaRef p
        setSchema <- declareSchemaRef $ Proxy @(Set a)
        pure $ Inline $ mempty & oneOf ?~ [singleSchema, setSchema]
    txOutRefSchema <- setOrSingleSchema $ Proxy @TxOutRef
    policyIdSchema <- setOrSingleSchema $ Proxy @PolicyId
    assetIdSchema <- setOrSingleSchema $ Proxy @AssetId
    let
      andSchema =
          mempty
            & type_ ?~ OpenApiObject
            & OpenApi.description ?~ "Matches any role tokens matched by both sub-filters."
            & required .~ ["and"]
            & properties .~ [("and", roleTokenFilterPairSchema)]
      orSchema =
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Matches any role tokens matched by either sub-filter."
          & required .~ ["or"]
          & properties .~ [("or", roleTokenFilterPairSchema)]
      notSchema =
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Matches any role tokens not matched by the sub-filter."
          & required .~ ["not"]
          & properties .~ [("not", roleTokenFilterSchema)]
      anySchema =
        mempty
          & type_ ?~ OpenApiBoolean
          & OpenApi.description ?~ "Matches any role token."
          & enum_ ?~ [Bool True]
      noneSchema =
        mempty
          & type_ ?~ OpenApiBoolean
          & OpenApi.description ?~ "Matches no role token."
          & enum_ ?~ [Bool False]
      contractsSchema =
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Matches any role tokens used by the given contract(s)."
          & required .~ ["contract_id"]
          & properties .~ [("contract_id", txOutRefSchema)]
      policiesSchema =
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Matches any role tokens with the given currency symbol(s)."
          & required .~ ["roles_currency"]
          & properties .~ [("roles_currency", policyIdSchema)]
      tokensSchema =
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "Matches only the given role token(s)."
          & required .~ ["role_tokens"]
          & properties .~ [("role_tokens", assetIdSchema)]
    pure $
      NamedSchema (Just "RoleTokenFilter") $
        mempty
          & OpenApi.description ?~ "A filter that selects role tokens for burning."
          & oneOf
            ?~ fmap
              Inline
                [ noneSchema
                , anySchema
                , contractsSchema
                , notSchema
                , policiesSchema
                , tokensSchema
                , andSchema
                , orSchema
                ]

instance HasDTO Chain.RoleTokenFilter where
  type DTO Chain.RoleTokenFilter = RoleTokenFilter

instance ToDTO Chain.RoleTokenFilter where
  toDTO = \case
    Chain.RoleTokensAnd a b -> RoleTokensAnd (toDTO a) (toDTO b)
    Chain.RoleTokensOr a b -> RoleTokensOr (toDTO a) (toDTO b)
    Chain.RoleTokensNot a -> RoleTokenNot (toDTO a)
    Chain.RoleTokenFilterNone -> RoleTokenFilterNone
    Chain.RoleTokenFilterByContracts contracts -> RoleTokenFilterByContracts (toDTO contracts)
    Chain.RoleTokenFilterByPolicyIds policies -> RoleTokenFilterByPolicyIds (toDTO policies)
    Chain.RoleTokenFilterByTokens tokens -> RoleTokenFilterByTokens (toDTO tokens)
    Chain.RoleTokenFilterAny -> RoleTokenFilterAny

instance FromDTO Chain.RoleTokenFilter where
  fromDTO = \case
    RoleTokensAnd a b -> Chain.RoleTokensAnd <$> fromDTO a <*> fromDTO b
    RoleTokensOr a b -> Chain.RoleTokensOr <$> fromDTO a <*> fromDTO b
    RoleTokenNot a -> Chain.RoleTokensNot <$> fromDTO a
    RoleTokenFilterNone -> pure Chain.RoleTokenFilterNone
    RoleTokenFilterByContracts contracts -> Chain.RoleTokenFilterByContracts <$> fromDTO contracts
    RoleTokenFilterByPolicyIds policies -> Chain.RoleTokenFilterByPolicyIds <$> fromDTO policies
    RoleTokenFilterByTokens tokens -> Chain.RoleTokenFilterByTokens <$> fromDTO tokens
    RoleTokenFilterAny -> pure Chain.RoleTokenFilterAny
