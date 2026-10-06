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

-- | Reference golden output for the Zero Coupon Bond contract.
module Spec.Marlowe.Semantics.Golden.ZeroCouponBond (
  -- * Contracts
  contract,

  -- * Test cases
  invalids,
  valids,
) where

import Language.Marlowe.Core.V1.Semantics (Payment (Payment), TransactionInput (..), TransactionOutput (..))
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Deposit),
  Case (Case),
  Contract (Close, Pay, When),
  Input (NormalInput),
  InputContent (IDeposit),
  Party,
  Payee (Party),
  State (State, accounts, boundValues, choices, minTime),
  Value (AddValue, Constant),
  ada,
  mkRoleUtf8,
 )
import Language.Marlowe.Util ()
import PlutusLedgerApi.V2 (POSIXTime (..))

import qualified PlutusTx.AssocMap as AM (Map, unsafeFromList)

lender :: Party
lender = mkRoleUtf8 "Lender"

borrower :: Party
borrower = mkRoleUtf8 "Borrower"

-- | The Zero-Coupon Bond contract.
contract :: Contract
contract =
  let principal' = Constant 100_000_000
      interest' = Constant 5_000_000
      lendingDeadline = 1_000
      paybackDeadline = 2_000
   in When
        [ Case (Deposit lender lender ada principal') $
            Pay lender (Party borrower) ada principal' $
              When
                [ Case (Deposit borrower borrower ada (principal' `AddValue` interest')) $
                    Pay
                      borrower
                      (Party lender)
                      ada
                      (principal' `AddValue` interest')
                      Close
                ]
                paybackDeadline
                Close
        ]
        lendingDeadline
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
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 1_000}, POSIXTime{getPOSIXTime = 1_000}), txInputs = []}]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 1_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit lender lender ada 100_000_000)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment lender (Party borrower) (ada) 100_000_000]
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
          , txInputs = [NormalInput (IDeposit lender lender ada 100_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit borrower borrower ada 105_000_000)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment lender (Party borrower) (ada) 100_000_000, Payment borrower (Party lender) (ada) 105_000_000]
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
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 100}, POSIXTime{getPOSIXTime = 1_000}), txInputs = []}]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = []
        , txOutState =
            State
              { accounts = toAM $ Map{unMap = []}
              , choices = toAM $ Map{unMap = []}
              , boundValues = toAM $ Map{unMap = []}
              , minTime = POSIXTime{getPOSIXTime = 1_000}
              }
        , txOutContract = Close
        }
    )
  ,
    ( POSIXTime{getPOSIXTime = 0}
    ,
      [ TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit lender lender ada 100_000_001)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = [Payment lender (Party borrower) (ada) 100_000_000]
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
          , txInputs = [NormalInput (IDeposit lender borrower ada 100_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit borrower borrower ada 105_000_000)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment lender (Party borrower) (ada) 100_000_000, Payment borrower (Party lender) (ada) 105_000_000]
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
