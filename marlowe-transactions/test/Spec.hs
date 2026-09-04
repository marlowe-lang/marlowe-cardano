{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec (Spec, describe, hspec)
import Language.Marlowe.Runtime.Core.ScriptRegistry.JSONSpec
import Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Language.Marlowe.Runtime.Transaction" buildConstraintsSpec
