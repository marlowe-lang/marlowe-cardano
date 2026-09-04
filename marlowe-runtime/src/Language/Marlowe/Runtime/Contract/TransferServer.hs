{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.TransferServer (
  ImportBundle,
  MainLabel (..),
  TransferServerDependencies (..),
  runTransferServer,
  runImport,
  mkImportBundle,
  merkleizeAndStoreContracts,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWST, evalRWST)
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Aeson as Aeson
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Foldable (traverse_, find)
import Data.Maybe (mapMaybe)
import qualified Language.Marlowe.Object.Link as O
import qualified Language.Marlowe.Object.Types as O
import Language.Marlowe.Runtime.Core.Api (ContractWithAdjacency (..))
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..), ContractStore (..))
import Language.Marlowe.Object.Types
    ( ContractHash(ContractHash, unContractHash),
      Label,
      ObjectBundle,
      LabelledObject(LabelledObject),
      ObjectType(ContractType),
      pattern SomeObjectType,
      ObjectBundle(ObjectBundle) )
import Marlowe.ContractStore.Protocol.Transfer.Server (
  ServerStCanDownload (..),
  ServerStCanUpload (..),
  ServerStExport (..),
  ServerStIdle (..),
  ServerStDownload (..),
  ServerStUpload (..),
 )
import Marlowe.ContractStore.Protocol.Transfer.Types (ImportError (..))
import Marlowe.Plutus.Semantics.Types qualified as Core
import qualified PlutusLedgerApi.V2 as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Pipes (Pipe, await, yield, void)
import Data.Map (Map)
import Language.Marlowe.Object.Link (LinkError(TypeMismatch), linkBundle')
import Debug.Trace (traceM)

-- | Dependencies of the transfer server.
newtype TransferServerDependencies m = TransferServerDependencies
  { contractStore :: ContractStore m
  }

-- | Drive the transfer server algebra. Returns the initial `ServerStIdle`
-- state; the caller can then invoke `recvMsgStartImport`,
-- `recvMsgUpload`, etc. to feed bundles and observe results.
--
-- This is the web-server-friendly equivalent of the original
-- `transferServer :: TransferServerDependencies m -> ServerSource
-- MarloweTransferServer m ()`. The bodies of `idleServer`,
-- `uploadServer`, and `downloadServer` are preserved from the original.
runTransferServer
  :: forall m a
   . (MonadFail m)
  => TransferServerDependencies m
  -> Set.Set Aeson.Value
  -> m (ServerStIdle m a)
runTransferServer deps@TransferServerDependencies{contractStore} preserveActions = do
  stage <- createContractStagingArea contractStore
  pure $ idleServer deps preserveActions stage

idleServer :: forall m a. (MonadFail m) => TransferServerDependencies m -> Set.Set Aeson.Value -> ContractStagingArea m -> ServerStIdle m a
idleServer deps preserveActions stage =
  ServerStIdle
    { recvMsgStartImport = pure $ uploadServer deps preserveActions mempty stage
    , recvMsgRequestExport = \rootHash -> do
        hashesO <- getClosureInExportOrder (contractStore deps) rootHash
        let toContractHash (ContractHash bs) = ContractHash bs
        pure $
          maybe
            (SendMsgContractNotFound (idleServer deps preserveActions stage))
            (SendMsgStartExport . downloadServer deps preserveActions stage)
            (map toContractHash <$> hashesO)
    , recvMsgDone = error "runTransferServer: recvMsgDone reached; pass the resulting ServerStIdle back to your driver."
    }

uploadServer
  :: forall m a
   . (MonadFail m)
  => TransferServerDependencies m
  -> Set.Set Aeson.Value
  -> O.SymbolTable
  -> ContractStagingArea m
  -> ServerStCanUpload m a
uploadServer deps preserveActions objects stage =
  ServerStCanUpload
    { recvMsgImported = do
        _ <- commit stage
        pure $ idleServer deps preserveActions stage
    , recvMsgUpload = \bundle -> do
        result <-
          runExceptT $
            O.linkBundle' bundle (merkleizeAndStoreContracts preserveActions stage) objects
        case result of
          Left err -> pure $ SendMsgUploadFailed err (idleServer deps preserveActions stage)
          Right (Left err) -> pure $ SendMsgUploadFailed (LinkError err) (idleServer deps preserveActions stage)
          Right (Right (linked, objects')) -> do
            _ <- flush stage
            _ <- commit stage
            pure $
              SendMsgUploaded (Map.fromList $ mapMaybe sequence linked) $
                uploadServer deps preserveActions objects' stage
    }

downloadServer
  :: forall m a
   . MonadFail m
  => TransferServerDependencies m
  -> Set.Set Aeson.Value
  -> ContractStagingArea m
  -> [ContractHash]
  -> ServerStCanDownload m a
downloadServer deps preserveActions stage hashes =
  ServerStCanDownload
    { recvMsgCancel = pure $ idleServer deps preserveActions stage
    , recvMsgDownload = \i -> do
        let (batchHashes, hashes') = splitAt (fromIntegral i) hashes
        case batchHashes of
          [] -> pure $ SendMsgExported (idleServer deps preserveActions stage)
          _ -> do
            let getHashContract (ContractHash bs) =
                  loadContract (contractStore deps) (ContractHash bs)
            batches <- traverse getHashContract batchHashes
            pure $
              SendMsgDownloaded (O.ObjectBundle batches) $
                downloadServer deps preserveActions stage hashes'
    }

getClosureInExportOrder :: forall m. Monad m => ContractStore m -> ContractHash -> m (Maybe [O.ContractHash])
getClosureInExportOrder ContractStore{getContract} rootHash = runMaybeT $ DList.toList . snd <$> evalRWST (writeClosureInExportOrder rootHash) () mempty
  where
    writeClosureInExportOrder :: ContractHash -> RWST () (DList.DList O.ContractHash) (Set.Set O.ContractHash) (MaybeT m) ()
    writeClosureInExportOrder hash = do
      visited <- RWS.get
      let hashO = O.fromCoreContractHash (BuiltinByteString (unContractHash hash))
      if Set.member hashO visited
        then pure ()
        else do
          ContractWithAdjacency{..} <- lift $ MaybeT $ getContract hash
          let toContractHash (O.ContractHash bs) = ContractHash bs
          traverse_ writeClosureInExportOrder
            (Set.toList $ Set.map toContractHash adjacency)
          RWS.tell $ pure hashO
          RWS.modify $ Set.insert hashO

loadContract :: forall m. MonadFail m => ContractStore m -> ContractHash -> m O.LabelledObject
loadContract ContractStore{getContract} hash = do
  result <- getContract hash
  case result of
    Nothing -> fail "Contract not found"
    Just ContractWithAdjacency{contract} -> do
      let hashO = O.fromCoreContractHash (BuiltinByteString (unContractHash hash))
      pure $ O.LabelledObject (O.Label $ T.pack $ show hashO) O.ContractType $ O.fromCoreContract contract

merkleizeAndStoreContracts
  :: (Monad m)
  => Set.Set Aeson.Value
  -> ContractStagingArea m
  -> O.LinkedObject
  -> ExceptT ImportError m (O.LinkedObject, Maybe ContractHash)
merkleizeAndStoreContracts preserveActions stage = \case
  O.LinkedContract contract -> do
    merkleizedContract <- merkleizeAndStore preserveActions stage contract
    hash <- lift $ stageContract stage merkleizedContract
    pure (O.LinkedContract merkleizedContract, Just hash)
  obj -> pure (obj, Nothing)

merkleizeAndStore
  :: (Monad m)
  => Set.Set Aeson.Value
  -> ContractStagingArea m
  -> Core.Contract
  -> ExceptT ImportError m Core.Contract
merkleizeAndStore preserveActions stage = \case
  Core.Close -> pure Core.Close
  Core.Pay account payee token value contract ->
    Core.Pay account payee token value <$> merkleizeAndStore preserveActions stage contract
  Core.If obs c1 c2 ->
    Core.If obs <$> merkleizeAndStore preserveActions stage c1 <*> merkleizeAndStore preserveActions stage c2
  Core.When cases timeout contract ->
    Core.When
      <$> traverse (merkleizeAndStoreCase preserveActions stage) cases
      <*> pure timeout
      <*> merkleizeAndStore preserveActions stage contract
  Core.Let valueId value contract ->
    Core.Let valueId value <$> merkleizeAndStore preserveActions stage contract
  Core.Assert obs contract ->
    Core.Assert obs <$> merkleizeAndStore preserveActions stage contract

-- | Compare an `Action` against the preserve-set by JSON shape.
-- FIXME: paluh: a more robust check would canonicalize both sides (e.g.
-- alphabetically-sort object keys, drop `null`s) so that minor formatting
-- differences don't defeat the match.
actionPreserved :: Set.Set Aeson.Value -> Core.Action -> Bool
actionPreserved preserveActions action =
  Set.member (jsonShape action) preserveActions

-- | Encode an `Action` to its canonical JSON shape (`Data.Aeson.encode` is
-- deterministic enough for our purposes; the `Action`'s `ToJSON` instance
-- emits a single object so we can compare the resulting `Value` directly).
jsonShape :: Core.Action -> Aeson.Value
jsonShape action = Aeson.toJSON action

merkleizeAndStoreCase
  :: (Monad m)
  => Set.Set Aeson.Value
  -> ContractStagingArea m
  -> Core.Case Core.Contract
  -> ExceptT ImportError m (Core.Case Core.Contract)
merkleizeAndStoreCase preserveActions stage@ContractStagingArea{..} = \case
  Core.Case action contract | actionPreserved preserveActions action ->
    -- Preserve the action: keep the `Case` constructor (the action stays
    -- readable on-chain) but recursively process the continuation so the
    -- rest of the contract is still merkleized.
    Core.Case action <$> merkleizeAndStore preserveActions stage contract
  Core.Case action Core.Close -> pure $ Core.Case action Core.Close
  Core.Case action contract -> do
    contract' <- merkleizeAndStore preserveActions stage contract
    ContractHash hash <- lift $ stageContract contract'
    pure $ Core.MerkleizedCase action $ BuiltinByteString hash
  Core.MerkleizedCase action hash -> do
    exists <- lift $ doesContractExist $ ContractHash (PV2.fromBuiltin @PV2.BuiltinByteString hash)
    if exists
      then pure $ Core.MerkleizedCase action hash
      else throwE $ ContinuationNotInStore $ O.fromCoreContractHash hash

-- | One-shot wrapper around `runTransferServer`: opens a staging area,
-- links the bundle, merkleizes every linked contract, commits, and
-- returns the final `Map Label ContractHash`. This is the convenience entry
-- point the web handler uses to drive the import; the original
-- `transferServer` returned a `ServerSource` instead, but for the web
-- case a single-shot result is the right shape.
runImport
  :: forall m
   . (Monad m)
  => TransferServerDependencies m
  -> Set.Set Aeson.Value
  -> O.ObjectBundle
  -> m (Either ImportError (Map.Map O.Label ContractHash))
runImport TransferServerDependencies{contractStore} preserveActions bundle = do
  stage <- createContractStagingArea contractStore
  result <-
    runExceptT $
      O.linkBundle' bundle (merkleizeAndStoreContracts preserveActions stage) mempty
  case result of
    Left err -> pure (Left err)
    Right (Left err) -> pure (Left (LinkError err))
    Right (Right (linked, _)) -> do
      _ <- flush stage
      _ <- commit stage
      pure $ Right $ Map.fromList $ mapMaybe sequence linked

-- watchForMain :: (Monad m) => Label -> Pipe ObjectBundle BundlePart m (Either ImportError (Map Label DatumHash))
-- watchForMain main = do
--   ObjectBundle bundle <- await
--   case find ((main ==) . _label) bundle of
--     Nothing -> do
--       yield $ IntermediatePart $ ObjectBundle bundle
--       watchForMain main
--     Just (LabelledObject _ ContractType _) -> do
--       yield $ FinalPart $ ObjectBundle bundle
--       pure $ Right mempty
--     Just (LabelledObject _ t _) ->
--       pure $
--         Left $
--           LinkError $
--             TypeMismatch
--               (UnsafeSomeObjectType $ unsafeCoerce ContractType)
--               (UnsafeSomeObjectType $ unsafeCoerce t)
-- 
-- data BundlePart
--   = Intermediate
--   | Final
--   | MainTypeMismatch O.SomeObjectType O.SomeObjectType
--   deriving (Show, Eq)
--
newtype MainLabel = MainLabel {label :: Label}
  deriving (Show, Eq, Ord)


-- data Proxy a' a b' b m r
-- A Proxy is a monad transformer that receives and sends information on both an upstream and downstream interface.
--
-- The type variables signify:
--
-- a' and a - The upstream interface, where (a')s go out and (a)s come in
-- b' and b - The downstream interface, where (b')s go out and (b')s come in
-- m - The base monad
-- r - The return value
--
-- type Pipe a b = Proxy () a () b
-- type Producer b = Proxy X () () b
--
type ImportBundle m
  = MainLabel
  -> Set.Set Aeson.Value
  -> Pipe ObjectBundle (Map Label ContractHash) m (Either ImportError (Map Label ContractHash))

mkImportBundle
  :: Monad m
  => ContractStagingArea m
  -> ImportBundle m
mkImportBundle stage@ContractStagingArea{..} (MainLabel main) preserveActions =
  loop mempty
  where
    loop objects = do
      bundle@(ObjectBundle bundleObjects) <- await
      case find (\LabelledObject { _label } -> _label == main) bundleObjects of
        Nothing ->
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, objects') -> do
              yield hashes
              loop objects'

        Just (LabelledObject _ ContractType _) -> do
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, _) -> do
              traceM "COMMITTING"
              lift $ void commit
              pure (Right hashes)

        Just (LabelledObject _ actual _) ->
          pure $ Left $ LinkError $ TypeMismatch (SomeObjectType ContractType) (SomeObjectType actual)

    step objects bundle = lift $ runExceptT $ do
      linked <- linkBundle' bundle (merkleizeAndStoreContracts preserveActions stage) objects
      case linked of
        Left linkError ->
          throwE $ LinkError linkError
        Right (hashPairs, objects') -> do
          lift $ void flush
          pure (Map.fromList (mapMaybe sequence hashPairs), objects')

