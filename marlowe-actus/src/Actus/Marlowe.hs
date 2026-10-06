{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | = Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Actus.Marlowe (
  CashFlowMarlowe,
  ContractTermsMarlowe,
  RiskFactorsMarlowe,
  defaultRiskFactors,
  genContract,
  genContract',
  -- == Conversion from Double to Marlowe representation
  -- re-export
  module Actus.Domain,
  genProjectedCashflows,
  -- utility
  fromMarloweFixedPoint,
  toMarlowe,
  toMarloweFixedPoint,
) where

import Actus.Domain
import Actus.Marlowe.Instance (
  CashFlowMarlowe,
  ContractTermsMarlowe,
  RiskFactorsMarlowe,
  fromMarloweFixedPoint,
  reduceContract,
  toMarloweFixedPoint,
 )

import Actus.Core (Event, genCashflow, genPayoffs, genSchedule, genStates)
import Actus.Model (
  CtxPOF (CtxPOF),
  CtxSTF (CtxSTF, contractTerms, referenceStates, riskFactors),
  initializeState,
  maturity,
  schedule,
  validateTerms,
 )
import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.Reader (Reader, runReader, withReader)
import Data.List as L (foldl', groupBy)
import Data.Maybe (isNothing)
import Data.Time (LocalTime (..), UTCTime (UTCTime), nominalDiffTimeToSeconds, timeOfDayToTime)
import Data.Time.Clock.POSIX
import Data.Validation (Validation (..))
import Language.Marlowe.Extended.V1
import PlutusTx.Builtins.HasOpaque

-- | 'genContract' validates the applicability of the contract terms in order
-- to generate a Marlowe contract with risk factors observed at a given point
-- in time
genContract
  :: (Party, Party)
  -- ^ Party and Counter-party for the contract
  -> (String -> EventType -> LocalTime -> RiskFactorsMarlowe)
  -- ^ Risk factors per event and time
  -> ContractTerms Double
  -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract
  -- ^ Marlowe contract or applicability errors
genContract p rf = fmap (genContract' p rf) . validateTerms

-- | Same as 'getContract', but does not validate the applicability of the contract
-- terms.
genContract'
  :: (Party, Party)
  -- ^ Party and Counter-party for the contract
  -> (String -> EventType -> LocalTime -> RiskFactorsMarlowe)
  -- ^ Risk factors per event and time
  -> ContractTerms Double
  -- ^ ACTUS contract terms
  -> Contract
  -- ^ Marlowe contract
genContract' (party, counterparty) rf ct =
  let cfs = genProjectedCashflows rf ct
   in foldl' gen Close $ reverse cfs
  where
    gen :: Contract -> CashFlow Value -> Contract
    gen cont cf@CashFlow{..}
      | hasRiskFactor cf =
          When
            [ Case
                (Choice (cashFlowToChoiceId cf) [Bound 0 100_000_000_000])
                (stub cont cf)
            ]
            (toTimeout cashPaymentDay)
            Close
    gen cont cf = stub cont cf

    stub cont CashFlow{..} =
      let t = toTimeout cashPaymentDay
          c = reduceContract cont
       in reduceContract $
            If
              (0 `ValueLT` amount)
              ( invoice
                  counterparty
                  party
                  amount
                  t
                  c
              )
              ( If
                  (amount `ValueLT` 0)
                  ( invoice
                      party
                      counterparty
                      (NegValue amount)
                      t
                      c
                  )
                  c
              )

    invoice :: Party -> Party -> Value -> Timeout -> Contract -> Contract
    invoice a b amount timeout continue =
      When
        [ Case
            (Deposit a a ada amount)
            ( Pay
                a
                (Party b)
                ada
                amount
                continue
            )
        ]
        timeout
        Close

cashFlowToChoiceId :: CashFlow a -> ChoiceId
cashFlowToChoiceId CashFlow{..} =
  let l = show cashEvent <> show cashPaymentDay
   in ChoiceId (stringToBuiltinByteString l) (Role "RiskFactor")

hasRiskFactor :: CashFlow Value -> Bool
hasRiskFactor cf = hasRiskFactor' (amount cf)
  where
    hasRiskFactor' :: Value -> Bool
    hasRiskFactor' (ChoiceValue j) | cashFlowToChoiceId cf == j = True
    hasRiskFactor' (ChoiceValue _) = False
    hasRiskFactor' (Constant _) = False
    hasRiskFactor' (AvailableMoney _ _) = False
    hasRiskFactor' (UseValue _) = False
    hasRiskFactor' (AddValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (SubValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (MulValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (DivValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (NegValue a) = hasRiskFactor' a
    hasRiskFactor' TimeIntervalStart = False
    hasRiskFactor' TimeIntervalEnd = False
    hasRiskFactor' (ConstantParam _) = False
    hasRiskFactor' (Cond _ a b) = hasRiskFactor' a || hasRiskFactor' b

defaultRiskFactors :: String -> EventType -> LocalTime -> RiskFactors Value
defaultRiskFactors _ ev t =
  let choiceId = ChoiceId (stringToBuiltinByteString $ show ev <> show t) (Role "RiskFactor")
      value = ChoiceValue choiceId
   in mkRiskFactor ev value

mkRiskFactor :: EventType -> Value -> RiskFactors Value
mkRiskFactor PP value =
  RiskFactors
    { o_rf_CURS = 1
    , o_rf_RRMO = 1
    , o_rf_SCMO = 1
    , pp_payoff = value
    , xd_payoff = 0
    , dv_payoff = 0
    }
mkRiskFactor XD value =
  RiskFactors
    { o_rf_CURS = 1
    , o_rf_RRMO = 1
    , o_rf_SCMO = 1
    , pp_payoff = 0
    , xd_payoff = value
    , dv_payoff = 0
    }
mkRiskFactor DV value =
  RiskFactors
    { o_rf_CURS = 1
    , o_rf_RRMO = 1
    , o_rf_SCMO = 1
    , pp_payoff = 0
    , xd_payoff = 0
    , dv_payoff = value
    }
mkRiskFactor _ _ =
  RiskFactors
    { o_rf_CURS = 1
    , o_rf_RRMO = 1
    , o_rf_SCMO = 1
    , pp_payoff = 0
    , xd_payoff = 0
    , dv_payoff = 0
    }

constant :: Double -> Value
constant = Constant . toMarloweFixedPoint

toTimeout :: LocalTime -> Timeout
toTimeout LocalTime{..} =
  let secs = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in POSIXTime (floor $ 1_000 * secs)

-- | 'genProjectedCashflows' generates a list of projected cashflows for
--  given contract terms and provided risk factors. The function returns
--  an empty list, if building the initial state given the contract terms
--  fails or in case there are no cash flows.
genProjectedCashflows
  :: (String -> EventType -> LocalTime -> RiskFactors Value)
  -- ^ Risk factors as a function of event type and time
  -> ContractTerms Double
  -- ^ Contract terms
  -> [CashFlow Value]
  -- ^ List of projected cash flows
genProjectedCashflows rf ct =
  let ctx = buildCtx rf ct
      sched = genSchedule ct []
   in check (toMarlowe ct) $ genCashflow (toMarlowe ct) <$> runReader (genProjectedPayoffs ct sched) ctx
  where
    check :: (Fractional a) => ContractTerms a -> [CashFlow a] -> [CashFlow a]
    check ContractTerms{deliverySettlement = Just DS_S} = netCashflows
    check _ = id

    netCashflows :: (Fractional a) => [CashFlow a] -> [CashFlow a]
    netCashflows cf = foldl1 plus <$> groupBy f cf
      where
        f a b =
          cashEvent a == cashEvent b
            && cashPaymentDay a == cashPaymentDay b
            && cashParty a == cashParty b
            && cashCounterParty a == cashCounterParty b
            && cashCurrency a == cashCurrency b
        plus a b =
          a
            { amount = amount a + amount b
            , notional = notional a + notional b
            }

-- | Generate projected cash flows
genProjectedPayoffs
  :: ContractTerms Double
  -- ^ Contract terms
  -> [Event]
  -- ^ Events
  -> Reader (CtxSTF Value) [(Event, ContractState Value, Value)]
  -- ^ Projected cash flows
genProjectedPayoffs ct@ContractTerms{..} events =
  do
    states <- initializeState >>= genStates events
    (eventTypes, filteredStates) <- unzip <$> filterM filtersStates (zip (tail events) states)
    payoffs <- trans $ genPayoffs eventTypes filteredStates
    pure $ zip3 eventTypes filteredStates payoffs
  where
    trans :: Reader (CtxPOF a) b -> Reader (CtxSTF a) b
    trans = withReader (\c -> CtxPOF (contractTerms c) (riskFactors c) (referenceStates c))

    filtersStates
      :: ((String, EventType, ShiftedDay), ContractState Value)
      -> Reader (CtxSTF Value) Bool
    filtersStates ((_, ev, ShiftedDay{..}), _) =
      do
        return $ case contractType of
          PAM -> isNothing purchaseDate || Just calculationDay >= purchaseDate
          LAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
          NAM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
          ANN ->
            let b1 = isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
                b2 = let m = maturityDate <|> amortizationDate <|> maturity ct in isNothing m || Just calculationDay <= m
             in b1 && b2
          SWPPV -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
          SWAPS -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
          CLM -> isNothing purchaseDate || ev == PRD || Just calculationDay > purchaseDate
          _ -> True

-- | Bulid the context allowing to perform state transitions
buildCtx
  :: (String -> EventType -> LocalTime -> RiskFactors Value)
  -- ^ Risk factors as a function of event type and time
  -> ContractTerms Double
  -- ^ Contract terms
  -> CtxSTF Value
  -- ^ Context
buildCtx rf ct =
  CtxSTF
    (toMarlowe ct)
    (calculationDay . snd <$> schedule FP ct) -- init & stf rely on the fee payment schedule
    (calculationDay . snd <$> schedule PR ct) -- init & stf rely on the principal redemption schedule
    (calculationDay . snd <$> schedule IP ct) -- init & stf rely on the interest payment schedule
    (maturity ct)
    rf
    []

toMarlowe :: ContractTerms Double -> ContractTermsMarlowe
toMarlowe ct =
  ContractTerms
    { contractId = contractId ct
    , contractType = contractType ct
    , contractStructure = map trans (contractStructure ct)
    , contractRole = contractRole ct
    , settlementCurrency = settlementCurrency ct
    , initialExchangeDate = initialExchangeDate ct
    , dayCountConvention = dayCountConvention ct
    , scheduleConfig = scheduleConfig ct
    , statusDate = statusDate ct
    , marketObjectCodeRef = Nothing
    , contractPerformance = contractPerformance ct
    , creditEventTypeCovered = creditEventTypeCovered ct
    , coverageOfCreditEnhancement = constant <$> coverageOfCreditEnhancement ct
    , guaranteedExposure = guaranteedExposure ct
    , cycleOfFee = cycleOfFee ct
    , cycleAnchorDateOfFee = cycleAnchorDateOfFee ct
    , feeAccrued = constant <$> feeAccrued ct
    , feeBasis = feeBasis ct
    , feeRate = constant <$> feeRate ct
    , cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment ct
    , cycleOfInterestPayment = cycleOfInterestPayment ct
    , accruedInterest = constant <$> accruedInterest ct
    , capitalizationEndDate = capitalizationEndDate ct
    , cycleAnchorDateOfInterestCalculationBase = cycleAnchorDateOfInterestCalculationBase ct
    , cycleOfInterestCalculationBase = cycleOfInterestCalculationBase ct
    , interestCalculationBase = interestCalculationBase ct
    , interestCalculationBaseA = constant <$> interestCalculationBaseA ct
    , nominalInterestRate = constant <$> nominalInterestRate ct
    , nominalInterestRate2 = constant <$> nominalInterestRate2 ct
    , interestScalingMultiplier = constant <$> interestScalingMultiplier ct
    , notionalPrincipal = constant <$> notionalPrincipal ct
    , premiumDiscountAtIED = constant <$> premiumDiscountAtIED ct
    , maturityDate = maturityDate ct
    , amortizationDate = amortizationDate ct
    , exerciseDate = exerciseDate ct
    , cycleAnchorDateOfPrincipalRedemption = cycleAnchorDateOfPrincipalRedemption ct
    , cycleOfPrincipalRedemption = cycleOfPrincipalRedemption ct
    , nextPrincipalRedemptionPayment = constant <$> nextPrincipalRedemptionPayment ct
    , purchaseDate = purchaseDate ct
    , priceAtPurchaseDate = constant <$> priceAtPurchaseDate ct
    , terminationDate = terminationDate ct
    , priceAtTerminationDate = constant <$> priceAtTerminationDate ct
    , quantity = constant <$> quantity ct
    , currency = currency ct
    , currency2 = currency2 ct
    , scalingIndexAtStatusDate = constant <$> scalingIndexAtStatusDate ct
    , cycleAnchorDateOfScalingIndex = cycleAnchorDateOfScalingIndex ct
    , cycleOfScalingIndex = cycleOfScalingIndex ct
    , scalingEffect = scalingEffect ct
    , scalingIndexAtContractDealDate = constant <$> scalingIndexAtContractDealDate ct
    , marketObjectCodeOfScalingIndex = marketObjectCodeOfScalingIndex ct
    , notionalScalingMultiplier = constant <$> notionalScalingMultiplier ct
    , cycleOfOptionality = cycleOfOptionality ct
    , cycleAnchorDateOfOptionality = cycleAnchorDateOfOptionality ct
    , optionType = optionType ct
    , optionStrike1 = constant <$> optionStrike1 ct
    , optionExerciseType = optionExerciseType ct
    , settlementPeriod = settlementPeriod ct
    , deliverySettlement = deliverySettlement ct
    , exerciseAmount = constant <$> exerciseAmount ct
    , futuresPrice = constant <$> futuresPrice ct
    , penaltyRate = constant <$> penaltyRate ct
    , penaltyType = penaltyType ct
    , prepaymentEffect = prepaymentEffect ct
    , cycleOfRateReset = cycleOfRateReset ct
    , cycleAnchorDateOfRateReset = cycleAnchorDateOfRateReset ct
    , nextResetRate = constant <$> nextResetRate ct
    , rateSpread = constant <$> rateSpread ct
    , rateMultiplier = constant <$> rateMultiplier ct
    , periodFloor = constant <$> periodFloor ct
    , periodCap = constant <$> periodCap ct
    , lifeCap = constant <$> lifeCap ct
    , lifeFloor = constant <$> lifeFloor ct
    , marketObjectCodeOfRateReset = marketObjectCodeOfRateReset ct
    , cycleOfDividend = cycleOfDividend ct
    , cycleAnchorDateOfDividend = cycleAnchorDateOfDividend ct
    , nextDividendPaymentAmount = constant <$> nextDividendPaymentAmount ct
    , enableSettlement = enableSettlement ct
    , constraints = constraints ct
    }
  where
    trans :: ContractStructure Double -> ContractStructure Value
    trans cs =
      cs
        { reference = case reference cs of
            ReferenceId r -> ReferenceId r
            ReferenceTerms t -> ReferenceTerms $ toMarlowe t
        }
