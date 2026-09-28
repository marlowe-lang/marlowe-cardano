{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Runtime.Core.ScriptRegistry.JSONSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import Test.Hspec (Spec, describe, it, shouldBe)

import Language.Marlowe.Runtime.Core.ScriptRegistry

spec :: FilePath -> Spec
spec registryPath = do
  describe "ScriptRegistry JSON" $ do
    it "round-trips a singleton registry" $ do
      originalText <- TIO.readFile registryPath
      let originalBytes = BSL.fromStrict (Text.encodeUtf8 originalText)
      case Aeson.decode originalBytes of
        Nothing -> fail "Failed to parse the original JSON"
        Just registry -> do
          -- Re-encode and verify that the re-encoded JSON parses to the same value
          let reencoded = Aeson.encode registry
          let reparsed = Aeson.decode reencoded :: Maybe ScriptRegistry
          reparsed `shouldBe` Just registry

