{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Adapter.Cardano.Api.ProtocolParameters where

import Cardano.Api.Era (ShelleyLedgerEra)
import Cardano.Api.Ledger
import Cardano.Api.ProtocolParameters (LedgerProtocolParameters (..))
import Data.Aeson
import GHC.Generics

-- Nicolas Henin N.B : Missing instances for the following types in the Cardano API, once the instances are added, the code should be removed
deriving instance Generic (LedgerProtocolParameters era)
deriving instance (ToJSON (PParams (ShelleyLedgerEra era))) => ToJSON (LedgerProtocolParameters era)
deriving instance (FromJSON (PParams (ShelleyLedgerEra era))) => FromJSON (LedgerProtocolParameters era)
