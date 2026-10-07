module Language.Marlowe.Runtime.Contract.StoreSpec (spec) where

import Control.Exception (bracket)
import qualified Data.Base16.Types
import qualified Data.ByteString.Base16 as BB16
import qualified Data.Set as Set
import qualified Data.Text as T
import Language.Marlowe.Object.Types (ContractHash (..))
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
import Language.Marlowe.Runtime.Contract.Store
import Language.Marlowe.Runtime.Contract.Store.File qualified as StoreFile
import Language.Marlowe.Runtime.Contract.Store.Memory qualified as StoreMemory
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag (V1))
import Marlowe.Plutus.Merkle (dataHash)
import Marlowe.Plutus.Semantics.Types (Action (Notify), Case (..), Contract (..), Observation (TrueObs))
import qualified PlutusLedgerApi.V2 as PV2
import qualified System.Directory as DS
import System.FilePath (takeBaseName, (</>))
import qualified System.IO.Temp as Temp
import qualified UnliftIO.STM as STM
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "ContractStore" do
  describe "in-memory backend" inMemorySpec
  describe "file backend" fileSpec

mkCloseWithAdjacency :: ContractHash -> ContractWithAdjacency 'V1
mkCloseWithAdjacency hash =
  ContractWithAdjacency
    { contractHash = hash
    , contract = Close
    , adjacency = Set.empty
    , closure = Set.singleton hash
    }

closeHashValue :: Contract -> ContractHash
closeHashValue c = ContractHash $ PV2.fromBuiltin $ dataHash c

-- | A non-Close contract for use in tests that need to distinguish "the
-- contract is genuinely missing from the store" from "the close short-circuit
-- would return Just regardless".
notifyClose :: Contract
notifyClose =
  When [Case (Notify TrueObs) Close] 100 Close

withStaging :: ContractStore IO -> (ContractStagingArea IO -> IO a) -> IO a
withStaging store action =
  bracket
    (createContractStagingArea store)
    discard
    action

-- | Run the given scenario against an in-memory store.
withInMemory :: (ContractStore IO -> IO ()) -> IO ()
withInMemory act = do
  stmStore <- STM.atomically StoreMemory.createContractStoreInMemory
  let store = hoistContractStore STM.atomically stmStore
  act store

-- | Run the given scenario against a file-backed store inside a temp directory.
withFileStore :: (ContractStore IO -> IO ()) -> IO ()
withFileStore act =
  Temp.withSystemTempDirectory "marlowe-store-spec" $ \tmpDir -> do
    store <-
      StoreFile.createContractStore
        StoreFile.ContractStoreOptions
          { contractStoreDirectory = tmpDir </> "store"
          , contractStoreStagingDirectory = tmpDir </> "staging"
          , lockingMicrosecondsBetweenRetries = 500_000
          , minContractAge = 0
          , maxStoreSize = 1024 * 1024 * 1024 * 1024
          }
    act store

scenarioStageAndCommit :: Spec
scenarioStageAndCommit = do
  it "stage + commit a single Close contract then lookup" $ withInMemory $ \store -> do
    hash <- withStaging store $ \staging -> do
      h <- stageContract staging Close
      _ <- commit staging
      pure h
    result <- getContract store hash
    result `shouldBe` Just (mkCloseWithAdjacency hash)
  it "stage + commit a single Close contract then lookup (file)" $ withFileStore $ \store -> do
    hash <- withStaging store $ \staging -> do
      h <- stageContract staging Close
      _ <- commit staging
      pure h
    result <- getContract store hash
    result `shouldBe` Just (mkCloseWithAdjacency hash)

scenarioCloseHash :: Spec
scenarioCloseHash = do
  it "Close resolves via static close hash" $ withInMemory $ \store -> do
    result <- getContract store (closeHashValue Close)
    result `shouldBe` Just (mkCloseWithAdjacency (closeHashValue Close))
  it "Close resolves via static close hash (file)" $ withFileStore $ \store -> do
    result <- getContract store (closeHashValue Close)
    result `shouldBe` Just (mkCloseWithAdjacency (closeHashValue Close))

scenarioDiscard :: Spec
scenarioDiscard = do
  it "discarding a staging area does not persist the contract" $ withInMemory $ \store -> do
    withStaging store $ \staging -> do
      _ <- stageContract staging notifyClose
      discard staging
    let unknownHash = ContractHash "0000000000000000000000000000000000000000000000000000000000000000"
    result <- getContract store unknownHash
    result `shouldBe` Nothing
  it "discarding a staging area does not persist the contract (file)" $ withFileStore $ \store -> do
    withStaging store $ \staging -> do
      _ <- stageContract staging notifyClose
      discard staging
    let unknownHash = ContractHash "0000000000000000000000000000000000000000000000000000000000000000"
    result <- getContract store unknownHash
    result `shouldBe` Nothing

scenarioDoesExist :: Spec
scenarioDoesExist = do
  it "getContract returns the contract after commit" $ withInMemory $ \store -> do
    hash <- withStaging store $ \staging -> do
      h <- stageContract staging Close
      _ <- commit staging
      pure h
    after <- getContract store hash
    after `shouldBe` Just (mkCloseWithAdjacency hash)
  it "getContract returns the contract after commit (file)" $ withFileStore $ \store -> do
    hash <- withStaging store $ \staging -> do
      h <- stageContract staging Close
      _ <- commit staging
      pure h
    after <- getContract store hash
    after `shouldBe` Just (mkCloseWithAdjacency hash)

scenarioCommitThenDiscard :: Spec
scenarioCommitThenDiscard = do
  it "discarding a staging area after commit does not throw" $ withInMemory $ \store -> do
    withStaging store $ \staging -> do
      _ <- stageContract staging notifyClose
      _ <- commit staging
      pure ()
  it "discarding a staging area after commit does not throw (file)" $ withFileStore $ \store -> do
    withStaging store $ \staging -> do
      _ <- stageContract staging notifyClose
      _ <- commit staging
      pure ()

inMemorySpec :: Spec
inMemorySpec = do
  scenarioStageAndCommit
  scenarioCloseHash
  scenarioDiscard
  scenarioDoesExist
  scenarioCommitThenDiscard

fileSpec :: Spec
fileSpec = do
  scenarioStageAndCommit
  scenarioCloseHash
  scenarioDiscard
  scenarioDoesExist
  scenarioCommitThenDiscard
  it "files for a committed contract appear on disk" $ do
    Temp.withSystemTempDirectory "marlowe-store-spec" $ \tmpDir -> do
      hash <- do
        store <-
          StoreFile.createContractStore
            StoreFile.ContractStoreOptions
              { contractStoreDirectory = tmpDir </> "store"
              , contractStoreStagingDirectory = tmpDir </> "staging"
              , lockingMicrosecondsBetweenRetries = 500_000
              , minContractAge = 0
              , maxStoreSize = 1024 * 1024 * 1024 * 1024
              }
        withStaging store $ \staging -> do
          h <- stageContract staging notifyClose
          _ <- commit staging
          pure h
      files <- DS.listDirectory (tmpDir </> "store")
      let hashPrefix = T.unpack $ Data.Base16.Types.extractBase16 $ BB16.encodeBase16 $ unContractHash hash
      files `shouldSatisfy` any (\f -> takeBaseName f == hashPrefix)
  it "a fresh store pointed at the same directory reads the committed contract back" $ do
    Temp.withSystemTempDirectory "marlowe-store-spec" $ \tmpDir -> do
      hash <- do
        store <-
          StoreFile.createContractStore
            StoreFile.ContractStoreOptions
              { contractStoreDirectory = tmpDir </> "store"
              , contractStoreStagingDirectory = tmpDir </> "staging"
              , lockingMicrosecondsBetweenRetries = 500_000
              , minContractAge = 0
              , maxStoreSize = 1024 * 1024 * 1024 * 1024
              }
        withStaging store $ \staging -> do
          h <- stageContract staging Close
          _ <- commit staging
          pure h
      store2 <-
        StoreFile.createContractStore
          StoreFile.ContractStoreOptions
            { contractStoreDirectory = tmpDir </> "store"
            , contractStoreStagingDirectory = tmpDir </> "staging"
            , lockingMicrosecondsBetweenRetries = 500_000
            , minContractAge = 0
            , maxStoreSize = 1024 * 1024 * 1024 * 1024
            }
      result <- getContract store2 hash
      result `shouldBe` Just (mkCloseWithAdjacency hash)