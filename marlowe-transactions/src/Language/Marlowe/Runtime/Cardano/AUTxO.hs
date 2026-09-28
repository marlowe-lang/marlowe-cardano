module Language.Marlowe.Runtime.Cardano.AUTxO
  ( AUTxO (..)
  , aUTxOValue
  , fromUTxO
  , toUTxO
  ) where

import Cardano.Api qualified as C
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)

-- | A single UTxO. We preserve the `Tuple` structure for consistency with `UTxO`.
newtype AUTxO era = AUTxO {unAUTxO :: (C.TxIn, C.TxOut C.CtxUTxO era)}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

fromUTxO :: C.UTxO era -> [AUTxO era]
fromUTxO (Map.toList . C.unUTxO -> items) = map AUTxO items

toUTxO :: [AUTxO era] -> C.UTxO era
toUTxO (map unAUTxO -> utxos) = C.UTxO . Map.fromList $ utxos

aUTxOValue :: forall era. AUTxO era -> C.Value
aUTxOValue (AUTxO (_, C.TxOut _ v _ _)) = C.txOutValueToValue v
