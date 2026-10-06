{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.Source.API (
  ContractSourcesAPI,
  ContractSourceAPI,
  ContractSourceId (..),
  PostContractSourceResponse (..),
  PreserveActions (..),
  ContractOrSourceId (..),
  mkContractSourceId,
  contractSourceIdFromText,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
 )
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Marlowe.Plutus.Semantics.Types (Action, Contract)
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Object.Types (Label (Label), ObjectBundle)
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject, OperationId, RenameResponseSchema)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Pipes (Producer)
import Servant (
  Description,
  FromHttpApiData,
  Header',
  JSON,
  NewlineFraming,
  Optional,
  Proxy (..),
  QueryFlag,
  QueryParam',
  Required,
  StreamBody,
  Strict,
  Summary,
  ToHttpApiData,
  type (:<|>),
  type (:>)
 )
import Servant.API (FromHttpApiData (..), ToHttpApiData (toHeader, toEncodedUrlPiece, toQueryParam, toUrlPiece), UVerb, StdMethod (POST, GET), HasStatus, StatusOf, Capture)

import Control.Lens ((&), (?~))
import Control.Monad ((<=<))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding qualified as T
import Data.OpenApi (
  HasOneOf (oneOf),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiString),
  ToParamSchema (..),
  ToSchema (..),
  declareSchemaRef,
 )
import qualified Data.OpenApi as OpenApi
import Language.Marlowe.Runtime.Web.Adapter.ByteString (hasLength)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))
import qualified Data.ByteString as BS
import qualified Language.Marlowe.Runtime.Web.Core.Base16 as Base16


-- | /contracts/sources sub-API
type ContractSourcesAPI =
  PostContractSourcesAPI
    :<|> Capture "contractSourceId" ContractSourceId
          :> ContractSourceAPI

-- | /contracts/sources/:contractSourceId sub-API
type ContractSourceAPI =
  GetContractSourceAPI
    :<|> "adjacency"
      :> Summary "Get adjacent contract source IDs by ID"
      :> Description
          "Get the contract source IDs which are adjacent to a contract source (they appear directly in the contract source)."
      :> OperationId "getContractSourceAdjacency"
      :> GetContractSourceIdsAPI
    :<|> "closure"
      :> Summary "Get contract source closure by ID"
      :> Description
          "Get the contract source IDs which appear in the full hierarchy of a contract source (including the ID of the contract source its self)."
      :> OperationId "getContractSourceClosure"
      :> GetContractSourceIdsAPI

type PostContractSourcesAPI =
  Summary "Upload contract sources"
    :> Description
        "Upload a bundle of marlowe objects as contract sources. This API supports request body streaming, with newline \
        \framing between request bundles."
    :> OperationId "initContractSources"
    :> QueryParam' '[Required, Description "The label of the top-level contract object in the bundle(s)."] "main" Label
    :> Header'
        '[ Optional
         , Strict
         , Description
             "JSON-encoded array of Action values. Any Case whose Action is \
            \in the array is preserved as a plain Case (its action stays \
            \readable on-chain) instead of being merkleized. The header \
            \value is a JSON array of Action objects, e.g. \
            \[{\"choose_between\":[...],\"for_choice\":{...}}]."
         ]
        "X-Preserve-Actions"
        PreserveActions
    :> StreamBody NewlineFraming JSON (Producer ObjectBundle IO ())
    :> UVerb
        'POST
        '[JSON]
        '[PostContractSourceResponse]

-- | A header value holding a set of `Action` values to preserve during
-- merkleization. The wire format is a JSON-encoded array of `Action` values
-- (using the standard `Action` JSON shape, with each constructor's keys
-- distinct from the others). During merkleization, any `Case` whose
-- `Action` is in this set is kept as a plain `Case` (its action stays
-- readable on-chain) instead of being rewritten as a `MerkleizedCase`.
newtype PreserveActions = PreserveActions {actions :: Set Action}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

-- | Parse a JSON-encoded array of `Action` values. An empty (or
-- all-whitespace) value is treated as "no preserved actions". An invalid
-- JSON value causes the whole request to fail.
instance FromHttpApiData PreserveActions where
  parseUrlPiece raw =
    let trimmed = T.strip raw
     in case T.uncons trimmed of
          Nothing -> Right $ PreserveActions Set.empty
          Just _ ->
            case Aeson.eitherDecodeStrict (T.encodeUtf8 trimmed) of
              Right actions -> Right $ PreserveActions $ Set.fromList actions
              Left err ->
                Left $
                  "Could not decode X-Preserve-Actions JSON value `"
                    <> trimmed
                    <> "`: "
                    <> T.pack err

-- | PreserveActions can only be carried as a header; we deliberately don't
-- define a `ToUrlPiece` instance so it can't be misused as a query param.
-- The header is rendered as a comma-separated list of JSON-encoded actions
-- so that round-tripping via `FromHttpApiData` recovers the original set.
instance ToHttpApiData PreserveActions where
  toUrlPiece _ = ""
  toQueryParam _ = ""
  toEncodedUrlPiece _ = mempty
  toHeader (PreserveActions actions) =
    LBS.toStrict $ Aeson.encode actions

instance HasStatus Contract where
  type StatusOf Contract = 200

type GetContractSourceAPI =
  Summary "Get contract source by ID"
    :> OperationId "getContractSourceById"
    :> QueryFlag "expand"
    :> UVerb
        'GET
        '[JSON]
        '[Contract]

type GetContractSourceIdsAPI =
  RenameResponseSchema "ContractSourceIds"
    :> UVerb
        'GET
        '[JSON]
        '[ListObject ContractSourceId]

data PostContractSourceResponse = PostContractSourceResponse
  { contractSourceId :: ContractSourceId
  , intermediateIds :: Map Label ContractSourceId
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, NFData)

instance HasStatus PostContractSourceResponse where
  type StatusOf PostContractSourceResponse = 200

-- TODO:
-- * turn this type into `newtype ContractSourceId = ContractSourceId ContractHash`
newtype ContractSourceId = ContractSourceId {unContractSourceId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON, NFData) via Base16

mkContractSourceId :: ByteString -> Maybe ContractSourceId
mkContractSourceId bs = if BS.length bs == 32 then Just (ContractSourceId bs) else Nothing

contractSourceIdFromText :: T.Text -> Maybe ContractSourceId
contractSourceIdFromText text = do
  Base16 bytes <- Base16.fromText text
  mkContractSourceId bytes

instance FromJSON ContractSourceId where
  parseJSON json = do
    Base16 bs <- parseJSON json
    case mkContractSourceId bs of
      Just cid -> pure cid
      Nothing -> fail $ "Invalid ContractSourceId JSON: " ++ show json

deriving newtype instance NFData Label

instance ToParamSchema PreserveActions where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "Comma-separated JSON-encoded Action values preserved during merkleization"

instance FromHttpApiData ContractSourceId where
  parseUrlPiece = fmap ContractSourceId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance ToSchema ContractSourceId where
  declareNamedSchema = pure . NamedSchema (Just "ContractSourceId") . toParamSchema

instance HasStatus (ListObject ContractSourceId) where
  type StatusOf (ListObject ContractSourceId) = 200

instance ToParamSchema ContractSourceId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Marlowe contract source"
      & OpenApi.pattern ?~ "^[a-fA-F0-9]{64}$"

newtype ContractOrSourceId = ContractOrSourceId (Either Semantics.Contract ContractSourceId)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContractOrSourceId where
  parseJSON =
    fmap ContractOrSourceId . \case
      String "close" -> pure $ Left Semantics.Close
      String s -> Right <$> parseJSON (String s)
      j -> Left <$> parseJSON j

instance ToJSON ContractOrSourceId where
  toJSON = \case
    ContractOrSourceId (Left contract) -> toJSON contract
    ContractOrSourceId (Right hash) -> toJSON hash

instance ToSchema ContractOrSourceId where
  declareNamedSchema _ = do
    contractSchema <- declareSchemaRef $ Proxy @Semantics.Contract
    contractSourceIdSchema <- declareSchemaRef $ Proxy @ContractSourceId
    pure $
      NamedSchema Nothing $
        mempty
          & oneOf ?~ [contractSchema, contractSourceIdSchema]

