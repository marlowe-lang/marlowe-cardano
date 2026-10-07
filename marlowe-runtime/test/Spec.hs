{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec (Spec, describe, hspec)
import Language.Marlowe.Runtime.Contract.StoreSpec qualified as StoreSpec
import Language.Marlowe.Runtime.Web.Contract.APISpec qualified as ContractAPISpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Language.Marlowe.Runtime.Contract.Store" StoreSpec.spec
  describe "Language.Marlowe.Runtime.Web.Contract.API" ContractAPISpec.spec
