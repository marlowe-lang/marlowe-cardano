module Language.Marlowe.Runtime.Contract.Store.File (
  ContractStoreOptions (..),
  createContractStore,
) where

import Codec.Compression.GZip (compress, decompress)
import Control.Monad (guard, unless, when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Base16.Types (extractBase16)
import Data.Binary (Word64, get, put)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.ByteString.Base16 (decodeBase16Untyped, encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold, foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import GHC.IO (mkUserError)
import Language.Marlowe.Object.Types (ContractHash (..))
import Language.Marlowe.Runtime.Contract.Store
import Language.Marlowe.Runtime.Contract.Store.Memory (merkleizeInputsDefault)
import Language.Marlowe.Runtime.Core.Api (ContractWithAdjacency (..))
import Marlowe.Plutus.Merkle (dataHash)
import Marlowe.Plutus.Semantics.Types (Case (..), Contract (..))
import qualified PlutusLedgerApi.V2 as PV2
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.LockFile (LockingParameters (..), RetryStrategy (NumberOfTimes), withLockFile)
import UnliftIO
import UnliftIO.Directory (
  copyFile,
  createDirectory,
  createDirectoryIfMissing,
  doesFileExist,
  getFileSize,
  getModificationTime,
  listDirectory,
  removeFile,
  removePathForcibly,
 )
import Control.DeepSeq (force)

-- | Options to configure a file-based contract store.
data ContractStoreOptions = ContractStoreOptions
  { contractStoreDirectory :: FilePath
  -- ^ The directory in which to store contract files.
  , contractStoreStagingDirectory :: FilePath
  -- ^ The directory in which to create staging areas.
  , lockingMicrosecondsBetweenRetries :: Word64
  , maxContractAge :: NominalDiffTime
  -- ^ Maximum time to retain a contract file. Files at least this old may be deleted.
  , maxStoreSize :: Integer
  -- ^ The maximum size, in bytes, of the contract store.
  }

-- | The internal state of a staging area's buffer.
type BufferState = (HashMap ContractHash (HashSet ContractHash), HashMap ContractHash ContractRecord)

-- | Create a contract store that uses the file system.
--
-- N.B. This implementation requires merkleized contract fragments to be added to the staging area
-- in a bottom-up, depth-first manner (i.e. all sub-contracts of any contract c
-- must be staged before c itself). This allows for a significant performance
-- optimization for computing closures that uses dynamic programming.
createContractStore
  :: forall m
   . (MonadUnliftIO m, MonadMask m)
  => ContractStoreOptions
  -> m (ContractStore m)
createContractStore ContractStoreOptions{..} = do
  -- Create the root directories if they do not exist.
  createDirectoryIfMissing True contractStoreDirectory
  createDirectoryIfMissing True contractStoreStagingDirectory
  -- Used to obtain exclusive write access to the store.
  let lockfile = contractStoreDirectory </> "lockfile"
  gcRootsVar <- newTVarIO Nothing
  pure
    ContractStore
      { createContractStagingArea = createContractStagingArea gcRootsVar lockfile
      , getContract = getContractImpl True lockfile
      , merkleizeInputs = \c state input ->
          withLockFile lockingParameters lockfile $
            merkleizeInputsDefault
              ((fmap . fmap) (\ContractWithAdjacency{..} -> contract) . getContractImpl False lockfile)
              c
              state
              input
      , setGCRoots = \roots -> do
          roots' <- liftIO $ evaluate $ force roots
          atomically $ writeTVar gcRootsVar $ Just roots'
      }
  where
    lockingParameters =
      LockingParameters
        { sleepBetweenRetries = lockingMicrosecondsBetweenRetries
        , retryToAcquireLock = NumberOfTimes 20
        }

    getStoreSize = do
      -- get the file names from the store
      storeFiles <- listDirectory contractStoreDirectory
      -- concurrently get the size of all files from the store
      sum <$> pooledMapConcurrently (getFileSize . (contractStoreDirectory </>)) storeFiles

    -- Try to free space by deleting dead contract files.
    freeSpace availableSpace requiredSpace gcRootsVar = do
      gcRoots <- atomically do
        roots <- readTVar gcRootsVar
        maybe retrySTM pure roots
      let getClosure (ContractHash hash) = runMaybeT do
            let path = contractStoreDirectory </> (T.unpack $ extractBase16 $ encodeBase16 hash) <.> "closure"
            guard =<< doesFileExist path
            liftIO $ Set.fromList . runGet get <$> LBS.readFile path
      liveContracts <- foldMap fold <$> pooledMapConcurrently getClosure (Set.toList @ContractHash gcRoots)
      storeFiles <- listDirectory contractStoreDirectory
      freedSpace <-
        sum . catMaybes <$> pooledForConcurrently storeFiles \file -> runMaybeT do
          let path = contractStoreDirectory </> file
          fileHash <- MaybeT $ pure $ either (const Nothing) (Just . ContractHash) $ decodeBase16Untyped $ encodeUtf8 $ T.pack $ takeBaseName path
          lastModified <- getModificationTime path
          now <- liftIO getCurrentTime
          guard $ now `diffUTCTime` lastModified >= maxContractAge
          guard $ not $ Set.member fileHash liveContracts
          fileSize <- liftIO $ getFileSize path
          liftIO $ removeFile path
          pure fileSize
      when (availableSpace + freedSpace < requiredSpace) do
        throwIO $ mkUserError "Unable to free enough space in contract store."

    getContractImpl lock lockfile contractHash
      | contractHash == closeHash =
          pure $
            Just
              ContractWithAdjacency
                { contract = Close
                , contractHash = closeHash
                , adjacency = mempty
                , closure = Set.singleton closeHash
                }
      | otherwise = (if lock then withLockFile lockingParameters lockfile else id) $ runMaybeT do
          let hexName = T.unpack $ extractBase16 $ encodeBase16 $ unContractHash contractHash
          let contractFilePath = contractStoreDirectory </> hexName <.> "contract"
          let adjacencyFilePath = contractStoreDirectory </> hexName <.> "adjacency"
          let closureFilePath = contractStoreDirectory </> hexName <.> "closure"
          guard =<< doesFileExist contractFilePath
          guard =<< doesFileExist adjacencyFilePath
          guard =<< doesFileExist closureFilePath
          contractBytesCompressed <- liftIO $ LBS.readFile contractFilePath
          adjacencyBytes <- liftIO $ LBS.readFile adjacencyFilePath
          closureBytes <- liftIO $ LBS.readFile closureFilePath
          let contractBytes = decompress contractBytesCompressed
          let contract = runGet get contractBytes
          --- Must decode as list, not set. Set's Get instance apparently uses fromDistinctAscList.
          let adjacency = Set.fromList $ runGet get adjacencyBytes
          let closure = Set.fromList $ runGet get closureBytes
          pure ContractWithAdjacency{..}

    createContractStagingArea gcRootsVar storeLockfile = do
      -- Use a UUID as the staging area directory name.
      uuid <- liftIO nextRandom
      let directory = contractStoreStagingDirectory </> "staging-area-" <> show uuid
      -- Create the staging directory
      createDirectory directory
      -- State variable to control when the staging area can be accessed.
      open <- newMVar True
      -- State variable for buffering file writes.
      mBuffer <- newMVar (mempty :: BufferState)
      pure
        ContractStagingArea
          { stageContract = stageContract' open mBuffer directory
          , flush = flushBuffer open mBuffer directory
          , commit = commitBuffer open mBuffer directory gcRootsVar storeLockfile
          , discard = discardBuffer open directory
          , doesContractExist = doesContractExistImpl open mBuffer directory
          }

    -- Runs the given action only if the store is open.
    whenOpen :: MVar Bool -> FilePath -> Bool -> m a -> m a
    whenOpen open directory leaveOpen ma = modifyMVar open \case
      False -> throwIO $ mkUserError "Staging area is no longer open"
      True -> do
        a <- ma
        unless leaveOpen $ removePathForcibly directory
        pure (leaveOpen, a)

    stageContract' :: MVar Bool -> MVar BufferState -> FilePath -> Contract -> m ContractHash
    stageContract' open mBuffer directory contract = whenOpen open directory True case contract of
      Close -> pure closeHash
      c -> do
        let hash = ContractHash $ PV2.fromBuiltin $ dataHash c
        modifyMVar mBuffer $ \(closuresMap, buffer) ->
          if HashMap.member hash closuresMap
            then pure ((closuresMap, buffer), hash)
            else do
              record <- computeRecord closuresMap hash c
              pure ((HashMap.insert hash (closureHM record) closuresMap, HashMap.insert hash record buffer), hash)

    flushBuffer :: MVar Bool -> MVar BufferState -> FilePath -> m (Set ContractHash)
    flushBuffer open mBuffer directory = whenOpen open directory True do
      modifyMVar mBuffer $ \(closures, buffer) -> do
        pooledMapConcurrently_ (flushContractRecord directory) buffer
        pure ((closures, mempty), Set.fromList $ HashMap.keys buffer)

    -- Writes a contract record to disk.
    flushContractRecord :: FilePath -> ContractRecord -> m ()
    flushContractRecord directory ContractRecord{..} = do
      let basePath = directory </> T.unpack (extractBase16 $ encodeBase16 $ unContractHash hash)
          writeIndex name hashes = do
            let filePath = basePath <.> name
            -- Do not compress the index files for speed.
            liftIO $ LBS.writeFile filePath $ runPut $ put $ HashSet.toList hashes
      let contractFilePath = basePath <.> "contract"
      -- Compress the contract file for size efficiency.
      liftIO $ LBS.writeFile contractFilePath $ compress $ runPut $ put contract
      writeIndex "adjacency" adjacencyHM
      writeIndex "closure" closureHM

    moveStagingFile :: FilePath -> FilePath -> m (Maybe ContractHash)
    moveStagingFile directory file = do
      let oldName = directory </> file
      let newName = contractStoreDirectory </> file
      fileExists <- doesFileExist newName
      if fileExists
        then pure Nothing
        else do
          copyFile oldName newName
          removeFile oldName
          pure $ either (const Nothing) (Just . ContractHash) $ decodeBase16Untyped $ encodeUtf8 $ T.pack $ takeBaseName file

    commitBuffer :: MVar Bool -> MVar BufferState -> FilePath -> TVar (Maybe (Set ContractHash)) -> FilePath -> m (Set ContractHash)
    commitBuffer open mBuffer directory gcRootsVar storeLockfile = whenOpen open directory False do
      -- flush the buffer
      modifyMVar mBuffer $ \(closures, buffer) -> do
        pooledMapConcurrently_ (flushContractRecord directory) buffer
        pure ((closures, mempty), Set.fromList $ HashMap.keys buffer)
      files <- listDirectory directory
      withLockFile lockingParameters storeLockfile do
        requiredSpace <- sum <$> pooledMapConcurrently (getFileSize . (directory </>)) files
        storeSize <- getStoreSize
        let availableSpace = maxStoreSize - storeSize
        when (availableSpace < requiredSpace) $ freeSpace availableSpace requiredSpace gcRootsVar
        results <- pooledMapConcurrently (moveStagingFile directory) files
        pure $ Set.fromList $ catMaybes results

    discardBuffer :: MVar Bool -> FilePath -> m ()
    discardBuffer open directory = modifyMVar open \case
      False -> pure (False, ())
      True -> do
        removePathForcibly directory
        pure (False, ())

    doesContractExistImpl :: MVar Bool -> MVar BufferState -> FilePath -> ContractHash -> m Bool
    doesContractExistImpl open mBuffer directory hash = whenOpen open directory True $ withMVar mBuffer $ \(_, buffer) ->
      if HashMap.member hash buffer
        then pure True
        else do
          let hexName = T.unpack $ extractBase16 $ encodeBase16 $ unContractHash hash
          let contractFileName = hexName <.> "contract"
          let stagingAreaPath = directory </> contractFileName
          let storePath = contractStoreDirectory </> contractFileName
          liftA2 (||) (doesFileExist stagingAreaPath) (doesFileExist storePath)

-- | Computes the adjacency and closure information for a merkleized contract.
computeRecord :: (MonadIO m) => HashMap ContractHash (HashSet ContractHash) -> ContractHash -> Contract -> m ContractRecord
computeRecord closures hash contract = do
  let adjacencyHM = computeAdjacency mempty contract
  closureHM <- computeClosure hash closures adjacencyHM
  pure ContractRecord{..}

-- | Computes the adjacency information for a merkleized contract.
computeAdjacency :: HashSet ContractHash -> Contract -> HashSet ContractHash
computeAdjacency acc = \case
  Close -> acc
  Pay _ _ _ _ c -> computeAdjacency acc c
  If _ c1 c2 -> computeAdjacency (computeAdjacency acc c1) c2
  When cases _ c -> computeAdjacency (foldl' computeCasesAdjacency acc cases) c
  Let _ _ c -> computeAdjacency acc c
  Assert _ c -> computeAdjacency acc c

-- | Computes the adjacency information for a case in a when contract.
computeCasesAdjacency
  :: HashSet ContractHash
  -> Case Contract
  -> HashSet ContractHash
computeCasesAdjacency acc = \case
  Case _ c -> computeAdjacency acc c
  MerkleizedCase _ hash -> HashSet.insert (ContractHash $ PV2.fromBuiltin hash) acc

-- | Expands the adjacency information of a contract into a closure.
computeClosure
  :: (MonadIO m)
  => ContractHash
  -- ^ The hash of the contract.
  -> HashMap ContractHash (HashSet ContractHash)
  -- ^ Lookup for existing closures.
  -> HashSet ContractHash
  -- ^ Set contract hashes to compute the closure for.
  -- Must all be members of the closure map argument.
  -> m (HashSet ContractHash)
computeClosure rootHash closures = fmap (HashSet.insert rootHash . fold) . traverse expand . HashSet.toList
  where
    expand hash
      | hash == closeHash = pure $ HashSet.singleton hash
      | otherwise = case HashMap.lookup hash closures of
          Nothing -> throwIO $ mkUserError $ "No closure found for" <> show hash
          Just transitiveClosure -> pure transitiveClosure

-- | Static hash for the close contract.
closeHash :: ContractHash
closeHash = ContractHash $ PV2.fromBuiltin $ dataHash Close

-- | A contract with its adjacency and closure information. Like ContractWithAdjacency but uses Hash Maps.
data ContractRecord = ContractRecord
  { contract :: Contract
  -- ^ The contract.
  , hash :: ContractHash
  -- ^ The hash of the contract (script datum hash)
  , adjacencyHM :: HashSet ContractHash
  -- ^ The set of continuation hashes explicitly contained in the contract.
  , closureHM :: HashSet ContractHash
  -- ^ The set of hashes contained in the contract and all recursive continuations of the contract.
  -- includes the hash of the contract itself.
  -- Does not contain the hash of the close contract.
  }
