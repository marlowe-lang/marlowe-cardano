{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Text as T
import qualified Paths_marlowe_transactions as Paths
import System.Environment (lookupEnv)
import Test.Hspec (describe, hspec)
import Language.Marlowe.Runtime.Core.ScriptRegistry
  ( MarloweScripts
  , ScriptRegistry
  , ScriptRegistryError (..)
  , ScriptSuiteName (..)
  , getCurrentScripts
  , loadScriptRegistry
  , pattern ScriptRegistry, defaultRegistry
  )
import Language.Marlowe.Runtime.Core.ScriptRegistry.JSONSpec
import Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec

-- The test suite honours the following environment variables so it can be
-- pointed at a custom registry/release from outside (e.g. from the dev-env):
--
--   * @MARLOWE_SCRIPT_REGISTRY@ – path to a JSON registry file. When
--     unset/empty, the registry shipped with the package is used.
--
--   * @MARLOWE_SCRIPT_SUITE_NAME@ – name of the suite to treat as the
--     "current" one. When unset/empty, the registry's own @currentRelease@
--     is used.
registryPathFromEnv :: IO FilePath
registryPathFromEnv = lookupEnv "MARLOWE_SCRIPT_REGISTRY" >>= \case
  Just (trim -> path) | not (null path) -> pure path
  _ -> Paths.getDataFileName defaultRegistry

loadRegistryAndScriptsFromEnv :: FilePath -> IO (Either ScriptRegistryError MarloweScripts)
loadRegistryAndScriptsFromEnv path = do
  registryResult <- loadScriptRegistry path
  case registryResult of
    Left err -> pure $ Left err
    Right registry -> fmap Right (pickReleaseFromEnv registry)

pickReleaseFromEnv :: ScriptRegistry -> IO MarloweScripts
pickReleaseFromEnv registry = lookupEnv "MARLOWE_SCRIPT_SUITE_NAME" >>= \case
  Just (trim -> name) -> do
    let
      ScriptRegistry _ scripts = registry
    case NEMap.lookup (ScriptSuiteName (T.pack name)) scripts of
      Just release -> pure release
      Nothing -> fail $ "Failed to find Marlowe scripts for suite name: " <> name
  _ -> pure $ getCurrentScripts registry

trim :: String -> String
trim = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  registryPath <- registryPathFromEnv
  scriptsResult <- loadRegistryAndScriptsFromEnv registryPath
  case scriptsResult of
    Left err -> fail $ "Failed to load Marlowe scripts for the test suite: " <> show err
    Right scripts -> hspec $ do
      describe "Language.Marlowe.Runtime.Core.ScriptRegistry" (spec registryPath)
      describe "Language.Marlowe.Runtime.Transaction" (buildConstraintsSpec scripts)
