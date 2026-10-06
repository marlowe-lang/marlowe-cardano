{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Language.Marlowe.Runtime.Web.Contract.Source.Server
  ( post
  , server
  , fromSourceId
  , toSourceId
  ) where

import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Language.Marlowe.Object.Link (LinkError (DuplicateLabel, TypeMismatch, UnknownSymbol))
import Language.Marlowe.Object.Types (ContractHash (ContractHash), Label, ObjectBundle)
import Language.Marlowe.Runtime.Contract.Store (MainLabel (MainLabel))
import Language.Marlowe.Runtime.Contract.Store qualified as Store
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag (V1))
import Language.Marlowe.Runtime.Core.Api qualified as Api
import Language.Marlowe.Runtime.Web.Adapter.Servant qualified as Adapter
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (UVerbT, runUVerbT)
import Language.Marlowe.Runtime.Web.Contract.API qualified as Web
import Language.Marlowe.Runtime.Web.Server.ApiError (badRequest', badRequest'', notFound')
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, getContractSourceL, withBundleImporterL)
import Marlowe.Plutus.Merkle (Continuations, deepDemerkleize, demerkleizeContract)
import Marlowe.Plutus.Semantics.Types qualified as Core
import Pipes (Producer, hoist, (>->))
import Pipes.Prelude qualified as Pipes
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant (HasServer (ServerT), type (:<|>) ((:<|>)), Union)

-- | The Servant server for `ContractSourcesAPI`.
server :: ServerT Web.ContractSourcesAPI ServerM
server = post :<|> contractSourceServer

contractSourceServer
  :: Web.ContractSourceId
  -> ServerT Web.ContractSourceAPI ServerM
contractSourceServer sourceId = do
  getOne sourceId
    :<|> getAdjacency sourceId
    :<|> getClosure sourceId

loadContract
  :: forall resp
   . Web.ContractSourceId
  -> UVerbT
       resp
       ServerM
       (Api.ContractWithAdjacency 'V1)
loadContract sourceId = do
  liftIO $ putStrLn $ "Loading contract handler: " <> show sourceId
  loadContract' <- lift $ view getContractSourceL
  lift (loadContract' sourceId) >>= \case
    Nothing -> lift . throwM . notFound' $ "Contract source not found: " <> show sourceId
    Just contractWithAdjacency -> pure contractWithAdjacency

loadContinuations
  :: forall resp
   . Set Web.ContractSourceId
  -> UVerbT resp ServerM Continuations
loadContinuations hs = do
  pairs <- for (Set.toList hs) \h -> do
    Api.ContractWithAdjacency{contract} <- loadContract h
    pure (toPlutusDatumHash (fromSourceId h), contract)
  pure $ Map.fromList pairs

-- | GET /contracts/sources/{id}?expand=true. Returns the demerkleized
-- contract (when `expand=true`) or the raw merkleized form.
getOne
  :: Web.ContractSourceId
  -> Bool
  -> ServerM (Union '[Core.Contract])
getOne sourceId expand = runUVerbT do
  Api.ContractWithAdjacency{contract, closure} <- loadContract sourceId
  if expand
    then do
      continuations <- loadContinuations (Set.fromList $ toSourceId <$> Set.toList closure)
      case demerkleizeContract continuations (deepDemerkleize False contract) of
        Left err -> lift . throwM $ badRequest''
          "Failed to demerkleize contract."
          "BadRequest"
          (err :: Text)
        Right expanded -> pure expanded
    else pure contract

-- | GET /contracts/sources/{id}/adjacency. Returns the immediate
-- adjacent hashes.
getAdjacency
  :: Web.ContractSourceId
  -> ServerM (Union '[Adapter.ListObject Web.ContractSourceId])
getAdjacency sourceId = runUVerbT do
  Api.ContractWithAdjacency{adjacency} <- loadContract sourceId
  pure $ Adapter.ListObject
    $ fmap toSourceId
    $ Set.toList adjacency

-- | GET /contracts/sources/{id}/closure. Returns the transitive
-- closure of all referenced hashes (including the contract itself).
getClosure
  :: Web.ContractSourceId
  -> ServerM (Union '[Adapter.ListObject Web.ContractSourceId])
getClosure sourceId = runUVerbT do
  Api.ContractWithAdjacency{closure} <- loadContract sourceId
  pure $ Adapter.ListObject
    $ fmap toSourceId
    $ Set.toList closure

fromSourceId :: Web.ContractSourceId -> ContractHash
fromSourceId (Web.ContractSourceId bs) = ContractHash bs

toSourceId :: ContractHash -> Web.ContractSourceId
toSourceId (ContractHash bs) = Web.ContractSourceId bs

-- | Convert a chain `ContractHash` to the corresponding Plutus `ContractHash`.
toPlutusDatumHash :: ContractHash -> PV2.DatumHash
toPlutusDatumHash (ContractHash bs) = PV2.DatumHash (BuiltinByteString bs)

post
  :: Label
  -> Maybe Web.PreserveActions
  -> Producer ObjectBundle IO ()
  -> ServerM (Union '[Web.PostContractSourceResponse])
post main mPreserveActions bundles = do
  let preserveActions =
        maybe (Store.PreserveActions Set.empty) (\dto -> Store.PreserveActions dto.actions) mPreserveActions
  withBundleImporter <- view withBundleImporterL
  withBundleImporter \importer -> runUVerbT do
    let
      importBundles :: Producer
        (Map Label ContractHash)
        (UVerbT '[Web.PostContractSourceResponse] ServerM)
        (Either Store.ImportError (Map Label ContractHash))
      importBundles =
        hoist liftIO (Right mempty <$ bundles)
          >-> hoist lift (Store.runBundleImporter importer (MainLabel main) preserveActions)
    (intermediate, result) <- Pipes.fold' (<>) mempty id importBundles
    case (intermediate <>) <$> result of
      Left err -> case err of
        Store.ContinuationNotInStore hash ->
          lift $ throwM $ badRequest'' "Merkleized continuation not in store." "BadRequest" hash
        Store.LinkError (UnknownSymbol s) ->
          lift $ throwM $ badRequest'' "Symbol not defined." "BadRequest" s
        Store.LinkError (DuplicateLabel s) ->
          lift $ throwM $ badRequest'' "Duplicate label." "BadRequest" s
        Store.LinkError (TypeMismatch expected actual) ->
          lift $ throwM $
            badRequest'' "Type mismatch." "BadRequest" $
              A.object
                [ "expected" .= expected
                , "actual" .= actual
                ]
      Right ids -> case Map.lookup main ids of
        Nothing -> lift $ throwM $ badRequest' "Main contract not defined."
        Just mainId -> pure $
            Web.PostContractSourceResponse
              { contractSourceId = toSourceId mainId
              , intermediateIds = toSourceId <$> ids
              }
