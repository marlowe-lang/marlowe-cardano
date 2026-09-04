{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeApplications #-}

module Language.Marlowe.Runtime.Web.Contract.Source.Server
  ( post
  , server
  , fromSourceId
  , toSourceId
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Control.Exception qualified as Ex
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Language.Marlowe.Object.Link (LinkError (UnknownSymbol, DuplicateLabel, TypeMismatch))
import Language.Marlowe.Object.Types (ContractHash (ContractHash, unContractHash), Label, ObjectBundle)
import Language.Marlowe.Object.Types qualified as O
import Language.Marlowe.Runtime.ChainSync.Api qualified as Chain
import Language.Marlowe.Runtime.Core.Api qualified as Api
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag (V1))
import Language.Marlowe.Runtime.Contract.Store qualified as Store
import Language.Marlowe.Runtime.Contract.TransferServer qualified as TS
import Language.Marlowe.Runtime.Web.Adapter.Servant qualified as Adapter
import Language.Marlowe.Runtime.Web.Contract.API qualified as Web
import Language.Marlowe.Runtime.Web.Server.ApiError (badRequest', badRequest'', notFound')
import Marlowe.ContractStore.Protocol.Transfer.Types qualified as T
import Marlowe.Plutus.Merkle (Continuations, deepDemerkleize, demerkleizeContract)
import Marlowe.Plutus.Semantics.Types qualified as Core
import Pipes (Producer, (>->), hoist)
import Pipes.Prelude qualified as Pipes
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant (HasServer (ServerT), err404, type (:<|>) ((:<|>)), Union)
import System.IO.Error (userError)
import Control.Lens (view)
import Language.Marlowe.Runtime.Web.Server.Monad (getContractSourceL, withBundleImporterL, ServerM, runEff0, runEff1)
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (runUVerbT, UVerbT)
import Control.Monad.Trans.Class (lift)
import Language.Marlowe.Runtime.Contract.TransferServer (MainLabel(MainLabel))

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
  let preserveActions = maybe Set.empty Web.unPreserveActions mPreserveActions
  withBundleImporter <- view withBundleImporterL
  withBundleImporter \importBundle -> runUVerbT do
    let
      importBundles :: Producer
        (Map Label ContractHash)
        (UVerbT '[Web.PostContractSourceResponse] ServerM)
        (Either T.ImportError (Map Label ContractHash))
      importBundles =
        hoist liftIO (Right mempty <$ bundles)
          >-> hoist lift (importBundle (MainLabel main) preserveActions)
    (intermediate, result) <- Pipes.fold' (<>) mempty id importBundles
    case (intermediate <>) <$> result of
      Left err -> case err of
        T.ContinuationNotInStore hash ->
          lift $ throwM $ badRequest'' "Merkleized continuation not in store." "BadRequest" hash
        T.LinkError (UnknownSymbol s) ->
          lift $ throwM $ badRequest'' "Symbol not defined." "BadRequest" s
        T.LinkError (DuplicateLabel s) ->
          lift $ throwM $ badRequest'' "Duplicate label." "BadRequest" s
        T.LinkError (TypeMismatch expected actual) ->
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
