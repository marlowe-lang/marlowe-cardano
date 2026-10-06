-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Reference golden output for the Trivial contract.
module Spec.Marlowe.Semantics.Golden.Trivial (
  -- * Contracts
  contract,

  -- * Test cases
  invalids,
  valids,
) where

import Language.Marlowe.Core.V1.Semantics (Payment (Payment), TransactionInput (..), TransactionOutput (..))
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Deposit, Notify),
  Case (Case),
  Contract (Close, Pay, When),
  Input (NormalInput),
  InputContent (IDeposit, INotify),
  Observation (TrueObs),
  Party,
  Payee (Party),
  State (State, accounts, boundValues, choices, minTime),
  Value (Constant),
  ada,
 )
import Language.Marlowe.Util ()
import PlutusLedgerApi.V2 (POSIXTime (..))

import qualified PlutusTx.AssocMap as AM (Map, unsafeFromList)

party :: Party
party = "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"

-- | The Trivial contract.
contract :: Contract
contract =
  let deposit = 40_000_000
      withdrawal = 10_000_000
      timeout = POSIXTime 4_000
   in When
        [ Case (Deposit party party ada (Constant deposit)) $
            When
              [ Case (Notify TrueObs) $
                  Pay party (Party party) ada (Constant withdrawal) $
                    When
                      [ Case
                          (Notify TrueObs)
                          Close
                      ]
                      timeout
                      Close
              ]
              (timeout - 1_000)
              Close
        ]
        (timeout - 2_000)
        Close

-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}

-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.unsafeFromList . unMap

-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    ( POSIXTime{getPOSIXTime = 0}
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 2_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 40_000_000)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 3_000}, POSIXTime{getPOSIXTime = 3_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment party (Party party) (ada) 40_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 3_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 40_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 4_000}, POSIXTime{getPOSIXTime = 4_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment party (Party party) (ada) 10_000_000, Payment party (Party party) (ada) 30_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 4_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 40_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment party (Party party) (ada) 10_000_000, Payment party (Party party) (ada) 30_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ]

-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    ( POSIXTime{getPOSIXTime = 0}
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 1_000}, POSIXTime{getPOSIXTime = 1_000}), txInputs = []}]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 2_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 40_000_001)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 3_000}, POSIXTime{getPOSIXTime = 3_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment party (Party party) (ada) 40_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 3_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party party ada 40_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 5_000}, POSIXTime{getPOSIXTime = 5_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment party (Party party) (ada) 10_000_000, Payment party (Party party) (ada) 30_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 4_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit party "Z" ada 40_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput INotify]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment party (Party party) (ada) 10_000_000, Payment party (Party party) (ada) 30_000_000]
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 0}
              }
        , txOutContract = Close
        }
    )
  ]
