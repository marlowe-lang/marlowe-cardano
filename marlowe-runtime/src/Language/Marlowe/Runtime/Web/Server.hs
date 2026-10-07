-- | This module defines a server for the root REST API.
module Language.Marlowe.Runtime.Web.Server
  ( RuntimeAPIWithOpenAPI
  , ServerDependencies(..)
  , openApiServer
  , runServer
  , server
  , serverWithOpenApi
  ) where

import Control.Lens ((%~), (&), (.~), (?~))
import Data.Version (makeVersion, showVersion)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Paths_marlowe_runtime
import Data.OpenApi.Internal as OpenApi
import Language.Marlowe.Runtime.Web.API (RuntimeAPI)
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, runServer, ServerDependencies(..))
import Language.Marlowe.Runtime.Web.Adapter.Servant ()
import Language.Marlowe.Runtime.Web.Adapter.Servant.Html (Html(..), HTML)

import qualified Language.Marlowe.Runtime.Web.Contract.Server as Contract
import qualified Language.Marlowe.Runtime.Web.Payout.Server as Payout
import qualified Language.Marlowe.Runtime.Web.Role.Server as Role
import qualified Language.Marlowe.Runtime.Web.Withdrawal.Server as Withdrawals
import Servant (
  HasServer (ServerT),
  NoContent (..),
  type (:<|>) ((:<|>)), Get, JSON, (:>),
 )
import qualified Data.OpenApi as OpenApi
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenApi
import qualified Servant.OpenApi as OpenApi
import Data.Proxy (Proxy(Proxy))

-- type RuntimeAPI =
--   WithRuntimeStatus
--     ( "contracts" :> ContractsAPI
--       :<|> "withdrawals" :> WithdrawalsAPI
--       :<|> "payouts" :> PayoutsAPI
--       :<|> "role" :> RoleAPI
--       :<|> "healthcheck"
--         :> ( Summary "Test server status"
--               :> Description "Check if the server is running and ready to respond to requests."
--               :> OperationId "healthcheck"
--               :> Get '[JSON] NoContent
--            )
--     )

server :: ServerT RuntimeAPI ServerM
server =
  Contract.server
    :<|> Withdrawals.server
    :<|> Payout.server
    :<|> Role.server
    :<|> healthcheckServer

healthcheckServer :: ServerM NoContent
healthcheckServer = pure NoContent

type OpenAPI =
  "openapi.json" :> Get '[JSON] OpenApi.OpenApiWithEmptySecurity
  :<|> "openapi.html" :> Get '[HTML] Html

openApiServer :: (Applicative m) => ServerT OpenAPI m
openApiServer = pure openApi :<|> pure rapiDoc
  where
    -- Render the API docs using `rapidoc`
    rapiDoc :: Html
    rapiDoc =
      Html $
        LBS.fromStrict $
          T.encodeUtf8 $
            T.unlines
              [ "<!DOCTYPE html>"
              , "<html>"
              , "<head>"
              , "<title>MGDoC REST API</title>"
              , "<script src=\"https://cdn.jsdelivr.net/npm/rapidoc@9.3.6/dist/rapidoc-min.min.js\"></script>"
              , "</head>"
              , "<body>"
              , "<rapi-doc id=\"thedoc\" spec-url=\"/openapi.json\" regular-font=\"Open Sans\" mono-font=\"Roboto Mono\" show-header=\"false\"></rapi-doc>"
              , "</body>"
              , "</html>"
              ]

    openApi :: OpenApi.OpenApiWithEmptySecurity
    openApi =
      OpenApi.OpenApiWithEmptySecurity $
        OpenApi.toOpenApi (Proxy :: Proxy RuntimeAPI)
          & OpenApi.info
            %~ (OpenApi.title .~ "Marlowe Runtime REST API")
              . (OpenApi.version .~ T.pack (showVersion Paths_marlowe_runtime.version))
              . (OpenApi.description ?~ "REST API for Marlowe Runtime")
              . (OpenApi.license
                    ?~ OpenApi.License
                      { _licenseName = "Apache 2.0"
                      , _licenseUrl = Just $ URL "https://www.apache.org/licenses/LICENSE-2.0.html"
                      }
                )
          & OpenApi.servers
            .~ [ "https://marlowe-lang.org"
               , "http://localhost:3780"
               ]
          & OpenApi.openapi .~ OpenApi.OpenApiSpecVersion (makeVersion [3, 1, 0])


type RuntimeAPIWithOpenAPI = RuntimeAPI :<|> OpenAPI

serverWithOpenApi :: ServerT RuntimeAPIWithOpenAPI ServerM
serverWithOpenApi = server :<|> openApiServer

