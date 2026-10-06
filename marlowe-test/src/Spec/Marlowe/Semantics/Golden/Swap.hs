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

-- | Reference golden output for the Swap contract.
module Spec.Marlowe.Semantics.Golden.Swap (
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
  Token (Token),
  Value (Constant),
  mkRoleUtf8,
  mkTokenNameUtf8,
  unsafeMkCurrencySymbolHex,
 )
import Language.Marlowe.Util ()
import PlutusLedgerApi.V2 (CurrencySymbol, POSIXTime (..))

import qualified PlutusTx.AssocMap as AM (Map, unsafeFromList)

aSymbol, bSymbol :: CurrencySymbol
aSymbol = unsafeMkCurrencySymbolHex "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8"
bSymbol = unsafeMkCurrencySymbolHex "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741"

aParty :: Party
aParty = mkRoleUtf8 "Party A"

bParty :: Party
bParty = mkRoleUtf8 "Party B"

aToken :: Token
aToken = Token aSymbol (mkTokenNameUtf8 "Token A")

bToken :: Token
bToken = Token bSymbol (mkTokenNameUtf8 "Token B")

-- | The Swap contract.
contract :: Contract
contract =
  let aAmount = 300_000_000
      bAmount = 500_000_000
      aTimeout = 1_000
      bTimeout = 2_000
   in When
        [ Case (Deposit aParty aParty aToken $ Constant aAmount) $
            When
              [ Case (Deposit bParty bParty bToken $ Constant bAmount) $
                  Pay aParty (Party bParty) aToken (Constant aAmount) $
                    Pay
                      bParty
                      (Party aParty)
                      bToken
                      (Constant bAmount)
                      Close
              ]
              bTimeout
              Close
        ]
        aTimeout
        Close

-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}

-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.unsafeFromList . unMap

-- | A list of Pangram test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
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
          , txInputs = [NormalInput (IDeposit aParty aParty aToken 300_000_000)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments = do
            [Payment aParty (Party aParty) aToken 300_000_000]
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
          , txInputs = [NormalInput (IDeposit aParty aParty aToken 300_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit bParty bParty bToken 500_000_000)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [ Payment aParty (Party bParty) aToken 300_000_000
            , Payment bParty (Party aParty) bToken 500_000_000
            ]
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

-- | A list of Pangram test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    ( POSIXTime{getPOSIXTime = 0}
    , [TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 900}, POSIXTime{getPOSIXTime = 900}), txInputs = []}]
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
          , txInputs = [NormalInput (IDeposit aParty aParty aToken 500_000_000)]
          }
      , TransactionInput{txInterval = (POSIXTime{getPOSIXTime = 2_000}, POSIXTime{getPOSIXTime = 2_000}), txInputs = []}
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [Payment aParty (Party aParty) aToken 300_000_000]
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
          , txInputs = [NormalInput (IDeposit aParty aParty aToken 300_000_000)]
          }
      , TransactionInput
          { txInterval = (POSIXTime{getPOSIXTime = 0}, POSIXTime{getPOSIXTime = 0})
          , txInputs = [NormalInput (IDeposit aParty bParty bToken 500_000_000)]
          }
      ]
    , TransactionOutput
        { txOutWarnings = []
        , txOutPayments =
            [ Payment aParty (Party bParty) aToken 300_000_000
            , Payment bParty (Party aParty) bToken 500_000_000
            ]
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
