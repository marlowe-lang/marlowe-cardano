{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the API and server for serving the Open API
-- specification.
module Language.Marlowe.Runtime.Web.OpenAPIServer where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (.~), (?~))
import qualified Control.Lens as Optics
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import Data.Aeson (ToJSON (toJSON), Value (Array))
import Data.Aeson.Lens (atKey, key, members)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.OpenApi (
  Definitions,
  HasComponents (components),
  HasContent (content),
  HasDefault (default_),
  HasDelete (delete),
  HasDescription (description),
  HasEnum (enum_),
  HasGet (get),
  HasHead (head_),
  HasHeaders (headers),
  HasInfo (info),
  HasItems (items),
  HasLicense (license),
  HasName (name),
  HasOneOf (oneOf),
  HasOpenapi (openapi),
  HasOptions (options),
  HasParameters (parameters),
  HasPatch (patch),
  HasPaths (paths),
  HasPost (post),
  HasProperties (properties),
  HasPut (put),
  HasRequestBodies (requestBodies),
  HasRequestBody (requestBody),
  HasRequired (required),
  HasResponses (responses),
  HasSchema (schema),
  HasSchemas (schemas),
  HasServers (servers),
  HasTitle (title),
  HasTrace (trace),
  HasType (type_),
  HasVersion (version),
  Header,
  License (License, _licenseName, _licenseUrl),
  MediaTypeObject,
  OpenApi,
  OpenApiItems (OpenApiItemsObject),
  OpenApiType (OpenApiArray, OpenApiString),
  Operation,
  Param,
  Reference (Reference),
  Referenced (..),
  RequestBody,
  Response,
  Schema,
  ToParamSchema (..),
  URL (URL),
 )
import Data.OpenApi.Internal (OpenApiSpecVersion (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Version (makeVersion, showVersion)
import GHC.Exts (toList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Language.Marlowe.Runtime.Web.API as Web
import Network.HTTP.Media ((//), (/:))
import qualified Paths_marlowe_runtime_web
import Servant (
  Accept,
  Get,
  HasServer (ServerT),
  JSON,
  MimeRender (mimeRender),
  Proxy (..),
  (:<|>) ((:<|>)),
  (:>),
  type (:>),
 )
import Servant.API (Accept (contentType))
import Servant.OpenApi (toOpenApi)
import Servant.Pagination (AcceptRanges, ContentRange, Ranges)

instance ToParamSchema (Ranges fields resource) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString

instance ToParamSchema (ContentRange fields resource) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString

instance (KnownSymbolList fields) => ToParamSchema (AcceptRanges fields) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiArray
      & items ?~ OpenApiItemsObject (Inline fieldsSchema)
    where
      fieldsSchema =
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ (fromString <$> symbolListVal (Proxy @fields))

class KnownSymbolList (ss :: [Symbol]) where
  symbolListVal :: Proxy ss -> [String]

instance KnownSymbolList '[] where
  symbolListVal _ = []

instance (KnownSymbolList ss, KnownSymbol s) => KnownSymbolList (s ': ss) where
  symbolListVal _ = symbolVal (Proxy @s) : symbolListVal (Proxy @ss)

newtype OpenApiWithEmptySecurity = OpenApiWithEmptySecurity OpenApi

instance ToJSON OpenApiWithEmptySecurity where
  toJSON (OpenApiWithEmptySecurity oa) =
    oa
      & toJSON
      & key "paths" . members . members . atKey "security" %~ (<|> Just (Array mempty))

data HTML = HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

newtype Html = Html {getHtml :: LBS.ByteString}

instance MimeRender HTML Html where
  mimeRender _ = getHtml

type API =
  "openapi.json" :> Get '[JSON] OpenApiWithEmptySecurity
    :<|> "openapi.html" :> Get '[HTML] Html

data OpenApiLintIssue = OpenApiLintIssue
  { trace :: Text
  , message :: Text
  }
  deriving (Show, Eq)

showStackTrace :: [Text] -> Text
showStackTrace = T.concat . List.intersperse "/" . List.reverse

newtype OpenApiLintEnvironment = OpenApiLintEnvironment
  { schemaDefinitions :: Definitions Schema
  }

lookupSchema :: (Monad m) => Referenced Schema -> ReaderT OpenApiLintEnvironment m (Maybe Schema)
lookupSchema = \case
  Inline ss -> pure $ Just ss
  Ref (Reference ref) -> do
    defs :: Definitions Schema <- Reader.asks schemaDefinitions
    pure $ IOHM.lookup ref defs

lookupType :: forall m. (Monad m) => Schema -> ReaderT OpenApiLintEnvironment m [OpenApiType]
lookupType s =
  let lookupSchemaType = Maybe.maybeToList $ Optics.view type_ s
      lookupOneOfType = case Optics.view oneOf s of
        Just refs -> do
          lookups :: [Maybe Schema] <- traverse lookupSchema refs
          if any Maybe.isNothing lookups
            then pure []
            else List.nub . concat <$> traverse lookupType (Maybe.catMaybes lookups)
        Nothing -> pure []
   in if null lookupSchemaType
        then lookupOneOfType
        else pure lookupSchemaType

lookupFieldType :: forall m. (Monad m) => Text -> Schema -> ReaderT OpenApiLintEnvironment m (Maybe [OpenApiType])
lookupFieldType fieldName =
  let loop :: Schema -> ReaderT OpenApiLintEnvironment m (Maybe [OpenApiType])
      loop s =
        case IOHM.lookup fieldName (Optics.view properties s) of
          Just ref -> lookupSchema ref >>= maybe (pure Nothing) (fmap (\x -> if null x then Nothing else Just x) . lookupType)
          Nothing -> case Optics.view oneOf s of
            Just s' -> do
              s'' :: [Maybe Schema] <- traverse lookupSchema s'
              if all Maybe.isJust s''
                then do
                  tjosan <- traverse loop (Maybe.catMaybes s'')
                  pure $ List.nub . concat <$> sequence tjosan
                else pure Nothing
            Nothing -> pure Nothing
   in loop

schemaRule1Check :: [Text] -> Schema -> ReaderT OpenApiLintEnvironment [] OpenApiLintIssue
schemaRule1Check stacktrace s = do
  schemaRequiredField :: Text <- Trans.lift $ Optics.view required s
  mFieldType <- lookupFieldType schemaRequiredField s
  case mFieldType of
    Just _ -> Trans.lift []
    Nothing ->
      Trans.lift
        [ OpenApiLintIssue
            { trace = showStackTrace stacktrace
            , message = "Missing type for required field '" <> schemaRequiredField <> "'!"
            }
        ]

lintOpenApi :: OpenApi -> [OpenApiLintIssue]
lintOpenApi oa = schemaDefinitionLints <> pathParametersLints <> pathOperationLints
  where
    schemaDefinitions :: Definitions Schema
    schemaDefinitions = Optics.view (components . schemas) oa

    paramDefinitions :: Definitions Param
    paramDefinitions = Optics.view (components . parameters) oa

    requestBodyDefinitions :: Definitions RequestBody
    requestBodyDefinitions = Optics.view (components . requestBodies) oa

    responseDefinitions :: Definitions Response
    responseDefinitions = Optics.view (components . responses) oa

    headerDefinitions :: Definitions Data.OpenApi.Header
    headerDefinitions = Optics.view (components . headers) oa

    schemaRef :: Referenced Schema -> Maybe Schema
    schemaRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` schemaDefinitions) -> s)) -> s

    paramRef :: Referenced Param -> Maybe Param
    paramRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` paramDefinitions) -> s)) -> s

    reqeuestBodyRef :: Referenced RequestBody -> Maybe RequestBody
    reqeuestBodyRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` requestBodyDefinitions) -> s)) -> s

    responseRef :: Referenced Response -> Maybe Response
    responseRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` responseDefinitions) -> s)) -> s

    headerRef :: Referenced Data.OpenApi.Header -> Maybe Data.OpenApi.Header
    headerRef = \case
      Inline s -> Just s
      Ref (Reference ((`IOHM.lookup` headerDefinitions) -> s)) -> s

    lintSchema :: [Text] -> Schema -> [OpenApiLintIssue]
    lintSchema stacktrace s = runReaderT (schemaRule1Check stacktrace s) (OpenApiLintEnvironment schemaDefinitions)

    lintParam :: [Text] -> Param -> [OpenApiLintIssue]
    lintParam stacktrace param = do
      s :: Schema <- Maybe.maybeToList (schemaRef =<< Optics.view schema param)
      lintSchema (Optics.view name param : stacktrace) s

    lintMediaTypeObject :: [Text] -> MediaTypeObject -> [OpenApiLintIssue]
    lintMediaTypeObject stacktrace mediaTypeObject =
      lintSchema stacktrace =<< (Maybe.maybeToList $ schemaRef =<< Optics.view schema mediaTypeObject)

    lintRequestBody :: [Text] -> RequestBody -> [OpenApiLintIssue]
    lintRequestBody stacktrace request = do
      (show -> T.pack -> mediaType, bodyContent :: MediaTypeObject) <- toList $ Optics.view content request
      lintMediaTypeObject (mediaType : stacktrace) bodyContent

    lintHeader :: [Text] -> Data.OpenApi.Header -> [OpenApiLintIssue]
    lintHeader stacktrace header =
      lintSchema stacktrace =<< (Maybe.maybeToList $ schemaRef =<< Optics.view schema header)

    lintResponse :: [Text] -> Response -> [OpenApiLintIssue]
    lintResponse stacktrace res = do
      let responseContentLints = do
            (show -> T.pack -> mediaType, responseContent :: MediaTypeObject) <- toList $ Optics.view content res
            lintMediaTypeObject (mediaType : "content" : stacktrace) responseContent
          responseHeadersLints = do
            (headerName, headerRef -> maybeHeader) <- toList $ Optics.view headers res
            lintHeader (headerName : stacktrace) =<< Maybe.maybeToList maybeHeader
      responseContentLints <> responseHeadersLints

    schemaDefinitionLints :: [OpenApiLintIssue]
    schemaDefinitionLints = do
      (definitionName, definitionSchema) <- toList schemaDefinitions
      lintSchema [definitionName, "schemas", "components"] definitionSchema

    pathParametersLints :: [OpenApiLintIssue]
    pathParametersLints = do
      (T.pack -> path, endpoint) <- toList $ Optics.view paths oa
      param :: Param <- Maybe.mapMaybe paramRef $ Optics.view parameters endpoint
      lintParam ["parameters", path, "paths"] param

    pathOperationLints :: [OpenApiLintIssue]
    pathOperationLints = do
      (T.pack -> path, endpoint) <- toList $ Optics.view paths oa
      (operationName :: Text, operation :: Operation) <-
        zip
          ["get", "put", "post", "delete", "options", "head", "patch", "trace"]
          $ Maybe.catMaybes
            [ Optics.view get endpoint
            , Optics.view put endpoint
            , Optics.view post endpoint
            , Optics.view delete endpoint
            , Optics.view options endpoint
            , Optics.view head_ endpoint
            , Optics.view patch endpoint
            , Optics.view Data.OpenApi.trace endpoint
            ]

      concat
        [ do
            param :: Param <- Maybe.mapMaybe paramRef $ Optics.view parameters operation
            lintParam ["parameters", operationName, path, "paths"] param
        , do
            request :: RequestBody <- Maybe.maybeToList $ reqeuestBodyRef =<< Optics.view requestBody operation
            lintRequestBody ["requestBody", operationName, path, "paths"] request
        , do
            defaultResponse :: Response <-
              Maybe.maybeToList $ responseRef =<< Optics.view (responses . default_) operation
            lintResponse ["default", "responses", operationName, path, "paths"] defaultResponse
        , do
            (show -> T.pack -> httpCode, responseRef -> maybeRes) <- toList $ Optics.view (responses . responses) operation
            lintResponse [httpCode, "responses", operationName, path, "paths"] =<< Maybe.maybeToList maybeRes
        ]

openApi :: OpenApiWithEmptySecurity
openApi =
  OpenApiWithEmptySecurity $
    toOpenApi Web.runtimeApi
      & info
        %~ (title .~ "Marlowe Runtime REST API")
          . (version .~ T.pack (showVersion Paths_marlowe_runtime_web.version))
          . (description ?~ "REST API for Marlowe Runtime")
          . ( license
                ?~ License
                  { _licenseName = "Apache 2.0"
                  , _licenseUrl = Just $ URL "https://www.apache.org/licenses/LICENSE-2.0.html"
                  }
            )
      & servers
        .~ [ "https://marlowe-runtime-preprod-web.scdev.aws.iohkdev.io"
           , "https://marlowe-runtime-preview-web.scdev.aws.iohkdev.io"
           , "https://marlowe-runtime-mainnet-web.scdev.aws.iohkdev.io"
           , "http://localhost:3780"
           ]
      & openapi .~ OpenApiSpecVersion (makeVersion [3, 1, 0])

rapiDoc :: Html
rapiDoc =
  Html $
    LBS.fromStrict $
      T.encodeUtf8 $
        T.unlines
          [ "<!DOCTYPE html>"
          , "<html>"
          , "<head>"
          , "<title>Marlowe Runtime REST API</title>"
          , -- We use external CDN but the file is preserved in the repo as well in `marlowe-runtime-web/static/rapidoc-9.3.6.min.js`
            "<script src=\"https://cdn.jsdelivr.net/npm/rapidoc@9.3.6/dist/rapidoc-min.min.js\"></script>"
          , "</head>"
          , "<body>"
          , "<rapi-doc id=\"thedoc\" spec-url=\"/openapi.json\" regular-font=\"Open Sans\" mono-font=\"Roboto Mono\" show-header=\"false\"></rapi-doc>"
          , "</body>"
          , "</html>"
          ]

server :: (Applicative m) => ServerT API m
server = pure openApi :<|> pure rapiDoc
