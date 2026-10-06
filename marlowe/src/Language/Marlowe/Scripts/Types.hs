{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Marlowe validators.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.Scripts.Types (
  -- * Types
  MarloweInput,
  MarloweTxInput (..),

  -- * Utilities
  marloweTxInputsFromInputs,

  -- * TH utilities
  readPlutusScript,
) where

import Cardano.Api (
  AsType (
    AsPlutusScript,
    AsPlutusScriptV1,
    AsPlutusScriptV2,
    AsPlutusScriptV3
  ),
  File (File),
  FromSomeType (FromSomeType),
  IsPlutusScriptLanguage (..),
  PlutusScript,
  PlutusScriptV1,
  PlutusScriptV2,
  PlutusScriptVersion (
    PlutusScriptV1,
    PlutusScriptV2,
    PlutusScriptV3
  ),
  readFileTextEnvelopeAnyOf,
 )
import Cardano.Api.Plutus (PlutusScript (..))
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Internal qualified as B
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Unsafe qualified as BS
import GHC.Generics (Generic)
import Language.Haskell.TH (
  Exp (AppE, AppTypeE, ConE, LitE, VarE),
  Lit (IntegerL),
  Q,
  Type (ConT),
  bytesPrimL,
  mkBytes,
  runIO,
 )
import Language.Haskell.TH.Syntax (qAddDependentFile)
import Language.Marlowe.Core.V1.Semantics.Types as Semantics (
  Input (..),
  InputContent,
 )
import Language.Marlowe.Pretty (Pretty (..))
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude as PlutusTxPrelude (
  BuiltinByteString,
  Functor (fmap),
  either,
  id,
  ($),
  (.),
 )
import System.IO.Unsafe (unsafePerformIO)
import Prelude qualified as Haskell

-- | Input to a Marlowe transaction.
type MarloweInput = [MarloweTxInput]

-- | A single input applied in the Marlowe semantics validator.
data MarloweTxInput
  = Input InputContent
  | MerkleizedTxInput InputContent BuiltinByteString
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (Pretty)

-- | Convert semantics input to transaction input.
marloweTxInputFromInput :: Input -> MarloweTxInput
marloweTxInputFromInput (NormalInput i) = Input i
marloweTxInputFromInput (MerkleizedInput i h _) = MerkleizedTxInput i h

-- | Convert semantics inputs to transaction inputs.
marloweTxInputsFromInputs :: [Input] -> [MarloweTxInput]
marloweTxInputsFromInputs = fmap marloweTxInputFromInput

-- Lifting data types to Plutus Core
makeLift ''MarloweTxInput
makeIsDataIndexed ''MarloweTxInput [('Input, 0), ('MerkleizedTxInput, 1)]

readPlutusScript :: Haskell.FilePath -> Q Exp
readPlutusScript fp = do
  qAddDependentFile fp
  runIO
    $ either (Haskell.error . Haskell.show) id
    Haskell.<$> readFileTextEnvelopeAnyOf
      [ FromSomeType (AsPlutusScript AsPlutusScriptV1) plutusScriptToExpr
      , FromSomeType (AsPlutusScript AsPlutusScriptV2) plutusScriptToExpr
      , FromSomeType (AsPlutusScript AsPlutusScriptV3) plutusScriptToExpr
      ]
      (File fp)

plutusScriptToExpr :: forall lang. (IsPlutusScriptLanguage lang) => PlutusScript lang -> Exp
plutusScriptToExpr (PlutusScriptSerialised (SBS.fromShort -> script)) =
  ConE 'PlutusScriptSerialised
    `AppTypeE` ConT case plutusScriptVersion @lang of
      PlutusScriptV1 -> ''PlutusScriptV1
      PlutusScriptV2 -> ''PlutusScriptV2
      PlutusScriptV3 -> Haskell.error "PlutusScriptV3 type constructor not exposed by cardano-api!"
    `AppE` ( AppE (VarE 'SBS.toShort)
               $ AppE (VarE 'unsafePerformIO)
               $ VarE 'BS.unsafePackAddressLen
               `AppE` LitE (IntegerL $ Haskell.fromIntegral $ B8.length script)
               `AppE` LitE
                 ( bytesPrimL
                     ( let B.PS ptr off sz = script
                        in mkBytes ptr (Haskell.fromIntegral off) (Haskell.fromIntegral sz)
                     )
                 )
           )
