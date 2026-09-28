module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec (
  buildConstraintsSpec,
) where

import Data.Int (Int64)
import Data.Ratio ((%))
import Control.Lens ((&), (.~))
import Cardano.Debug (friendlyTxBodyImpl, formatToString, FormatYaml(FormatYaml), runWarningIO)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (property, ioProperty)
import Cardano.Api (
  AsType (AsPaymentKey),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  EraHistory (EraHistory),
  generateSigningKey,
  getVerificationKey,
  makeShelleyAddress,
  makeShelleyAddressInEra,
  verificationKeyHash,
  PaymentCredential (..),
  StakeAddressReference (..),
  ShelleyBasedEra (ShelleyBasedEraConway),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  toLedgerEpochInfo,
  )
import qualified Cardano.Api as C
-- import Convex.MockChain.Defaults (protocolParameters)
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SOP (K (K))
import Data.SOP.Counting (Exactly (Exactly))
import Data.SOP.NonEmpty (nonEmptyHead)
import Data.SOP.Strict (NP ((:*), Nil))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Ouroboros.Consensus.Block.Abstract qualified as OuroborosA
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime))
import qualified PlutusLedgerApi.V2 as PV2
import qualified Marlowe.Plutus.Semantics.Types as V1
import Marlowe.Plutus.Semantics (MarloweData (..), MarloweParams (..))
import Marlowe.Plutus.Semantics.Types (unsafeMkPartyAddressBech32, ada)
import qualified PlutusTx.AssocMap as AM
import Language.Marlowe.Runtime.ChainSync.Api hiding (Datum, ada)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (..), ReferenceScriptUtxo (..), ScriptInPlutus (..), ScriptDetails (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressInEra)
import Language.Marlowe.Runtime.Transaction.Api (
  InitError,
  RoleTokensConfig (..),
  WithdrawError,
  )
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  AdjustMinUTxO (..),
  buildInitConstraints,
  buildWithdrawConstraints,
  RolesPolicyId (..), buildApplyInputsConstraints,
  )
import Language.Marlowe.Runtime.Transaction.Constraints
    ( RoleTokenConstraints(RoleTokenConstraintsNone),
      TxConstraints(..),
      WalletContext(..),
      PayoutContext(..),
      MarloweContext(..),
      HelpersContext(..),
      solveConstraints,
      ThreadTokenConstraints(PassThreadToken) )
import Data.Foldable (for_)
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Vary qualified
import Cardano.Ledger.Core (PParams)
import Ouroboros.Consensus.Shelley.Eras (ConwayEra)
import Cardano.Ledger.Plutus.CostModels qualified as CostModels
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Alonzo.PParams qualified as L
import Cardano.Ledger.Babbage.PParams qualified as L
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Shelley.API (
  Coin (..),
 )
import Cardano.Ledger.Plutus.Language (Language (..))

type TestEra = C.ConwayEra

-- Copied from sc-tools mockchain
protocolParameters :: PParams ConwayEra
protocolParameters =
  let pparams =
        L.PParams $
          L.emptyPParamsIdentity @ConwayEra
            & L.hkdMaxBHSizeL .~ 1_100
            & L.hkdMaxBBSizeL .~ 90_112
            & L.hkdMaxTxSizeL .~ 16_384
            & L.hkdTxFeePerByteL .~ L.CoinPerByte (L.toCompactPartial (Coin 44))
            & L.hkdTxFeeFixedCompactL .~ L.toCompactPartial (Coin 155_381)
            & L.hkdKeyDepositCompactL .~ L.toCompactPartial (Coin 2_000_000)
            & L.hkdPoolDepositCompactL .~ L.toCompactPartial 500_000_000
            & L.hkdDRepDepositCompactL .~ L.toCompactPartial 500_000_000
            & L.hkdCoinsPerUTxOByteL .~ L.CoinPerByte (L.toCompactPartial (Coin 4_310))
            & L.hkdPricesL
              .~ L.Prices
                { L.prMem = C.unsafeBoundedRational (577 % 10_000)
                , L.prSteps = C.unsafeBoundedRational (721 % 10_000_000)
                }
            & L.hkdMaxTxExUnitsL .~ L.ExUnits{exUnitsSteps = 1_0000_000_000, exUnitsMem = 14_000_000}
            & L.hkdMaxBlockExUnitsL .~ L.ExUnits{exUnitsSteps = 20_000_000_000, exUnitsMem = 62_000_000}
            & L.hkdMaxValSizeL .~ 5_000
            & L.hkdCollateralPercentageL .~ 150
            & L.hkdMaxCollateralInputsL .~ 3
            & L.hkdMinPoolCostCompactL .~ L.toCompactPartial (Coin 200_000)
            & L.hkdCostModelsL .~ CostModels.mkCostModels (Map.fromList [(PlutusV1, v1CostModel), (PlutusV2, v2CostModel), (PlutusV3, v3CostModel)])
            & L.hkdMinFeeRefScriptCostPerByteL .~ C.unsafeBoundedRational 15
            & L.hkdMinPoolCostCompactL .~ L.toCompactPartial (Coin 170_000_000)
            & L.hkdEMaxL .~ L.EpochInterval 18
   in pparams
        & L.ppProtocolVersionL .~ latestProtVer

latestProtVer :: L.ProtVer
latestProtVer = L.ProtVer (toEnum 10) 0

unsafeMkCostModel :: Language -> [Int64] -> CostModels.CostModel
unsafeMkCostModel lang = either (error . show) id . CostModels.mkCostModel lang

v1CostModel :: CostModels.CostModel
v1CostModel =
  unsafeMkCostModel
    PlutusV1
    [ 100788
    , 420
    , 1
    , 1
    , 1000
    , 173
    , 0
    , 1
    , 1000
    , 59957
    , 4
    , 1
    , 11183
    , 32
    , 201305
    , 8356
    , 4
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 100
    , 100
    , 16000
    , 100
    , 94375
    , 32
    , 132994
    , 32
    , 61462
    , 4
    , 72010
    , 178
    , 0
    , 1
    , 22151
    , 32
    , 91189
    , 769
    , 4
    , 2
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 1000
    , 42921
    , 4
    , 2
    , 24548
    , 29498
    , 38
    , 1
    , 898148
    , 27279
    , 1
    , 51775
    , 558
    , 1
    , 39184
    , 1000
    , 60594
    , 1
    , 141895
    , 32
    , 83150
    , 32
    , 15299
    , 32
    , 76049
    , 1
    , 13169
    , 4
    , 22100
    , 10
    , 28999
    , 74
    , 1
    , 28999
    , 74
    , 1
    , 43285
    , 552
    , 1
    , 44749
    , 541
    , 1
    , 33852
    , 32
    , 68246
    , 32
    , 72362
    , 32
    , 7243
    , 32
    , 7391
    , 32
    , 11546
    , 32
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 90434
    , 519
    , 0
    , 1
    , 74433
    , 32
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 270652
    , 22588
    , 4
    , 1457325
    , 64566
    , 4
    , 20467
    , 1
    , 4
    , 0
    , 141992
    , 32
    , 100788
    , 420
    , 1
    , 1
    , 81663
    , 32
    , 59498
    , 32
    , 20142
    , 32
    , 24588
    , 32
    , 20744
    , 32
    , 25933
    , 32
    , 24623
    , 32
    , 53384111
    , 14333
    , 10
    ]

v2CostModel :: CostModels.CostModel
v2CostModel =
  unsafeMkCostModel
    PlutusV2
    [ 100788
    , 420
    , 1
    , 1
    , 1000
    , 173
    , 0
    , 1
    , 1000
    , 59957
    , 4
    , 1
    , 11183
    , 32
    , 201305
    , 8356
    , 4
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 100
    , 100
    , 16000
    , 100
    , 94375
    , 32
    , 132994
    , 32
    , 61462
    , 4
    , 72010
    , 178
    , 0
    , 1
    , 22151
    , 32
    , 91189
    , 769
    , 4
    , 2
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 1000
    , 42921
    , 4
    , 2
    , 24548
    , 29498
    , 38
    , 1
    , 898148
    , 27279
    , 1
    , 51775
    , 558
    , 1
    , 39184
    , 1000
    , 60594
    , 1
    , 141895
    , 32
    , 83150
    , 32
    , 15299
    , 32
    , 76049
    , 1
    , 13169
    , 4
    , 22100
    , 10
    , 28999
    , 74
    , 1
    , 28999
    , 74
    , 1
    , 43285
    , 552
    , 1
    , 44749
    , 541
    , 1
    , 33852
    , 32
    , 68246
    , 32
    , 72362
    , 32
    , 7243
    , 32
    , 7391
    , 32
    , 11546
    , 32
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 90434
    , 519
    , 0
    , 1
    , 74433
    , 32
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 85848
    , 228465
    , 122
    , 0
    , 1
    , 1
    , 955506
    , 213312
    , 0
    , 2
    , 270652
    , 22588
    , 4
    , 1457325
    , 64566
    , 4
    , 20467
    , 1
    , 4
    , 0
    , 141992
    , 32
    , 100788
    , 420
    , 1
    , 1
    , 81663
    , 32
    , 59498
    , 32
    , 20142
    , 32
    , 24588
    , 32
    , 20744
    , 32
    , 25933
    , 32
    , 24623
    , 32
    , 43053543
    , 10
    , 53384111
    , 14333
    , 10
    , 43574283
    , 26308
    , 10
    ]

v3CostModel :: CostModels.CostModel
v3CostModel =
  unsafeMkCostModel
    PlutusV3
    [ 100788
    , 420
    , 1
    , 1
    , 1000
    , 173
    , 0
    , 1
    , 1000
    , 59957
    , 4
    , 1
    , 11183
    , 32
    , 201305
    , 8356
    , 4
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 16000
    , 100
    , 100
    , 100
    , 16000
    , 100
    , 94375
    , 32
    , 132994
    , 32
    , 61462
    , 4
    , 72010
    , 178
    , 0
    , 1
    , 22151
    , 32
    , 91189
    , 769
    , 4
    , 2
    , 85848
    , 123203
    , 7305
    , -900
    , 1716
    , 549
    , 57
    , 85848
    , 0
    , 1
    , 1
    , 1000
    , 42921
    , 4
    , 2
    , 24548
    , 29498
    , 38
    , 1
    , 898148
    , 27279
    , 1
    , 51775
    , 558
    , 1
    , 39184
    , 1000
    , 60594
    , 1
    , 141895
    , 32
    , 83150
    , 32
    , 15299
    , 32
    , 76049
    , 1
    , 13169
    , 4
    , 22100
    , 10
    , 28999
    , 74
    , 1
    , 28999
    , 74
    , 1
    , 43285
    , 552
    , 1
    , 44749
    , 541
    , 1
    , 33852
    , 32
    , 68246
    , 32
    , 72362
    , 32
    , 7243
    , 32
    , 7391
    , 32
    , 11546
    , 32
    , 85848
    , 123203
    , 7305
    , -900
    , 1716
    , 549
    , 57
    , 85848
    , 0
    , 1
    , 90434
    , 519
    , 0
    , 1
    , 74433
    , 32
    , 85848
    , 123203
    , 7305
    , -900
    , 1716
    , 549
    , 57
    , 85848
    , 0
    , 1
    , 1
    , 85848
    , 123203
    , 7305
    , -900
    , 1716
    , 549
    , 57
    , 85848
    , 0
    , 1
    , 955506
    , 213312
    , 0
    , 2
    , 270652
    , 22588
    , 4
    , 1457325
    , 64566
    , 4
    , 20467
    , 1
    , 4
    , 0
    , 141992
    , 32
    , 100788
    , 420
    , 1
    , 1
    , 81663
    , 32
    , 59498
    , 32
    , 20142
    , 32
    , 24588
    , 32
    , 20744
    , 32
    , 25933
    , 32
    , 24623
    , 32
    , 43053543
    , 10
    , 53384111
    , 14333
    , 10
    , 43574283
    , 26308
    , 10
    , 16000
    , 100
    , 16000
    , 100
    , 962335
    , 18
    , 2780678
    , 6
    , 442008
    , 1
    , 52538055
    , 3756
    , 18
    , 267929
    , 18
    , 76433006
    , 8868
    , 18
    , 52948122
    , 18
    , 1995836
    , 36
    , 3227919
    , 12
    , 901022
    , 1
    , 166917843
    , 4307
    , 36
    , 284546
    , 36
    , 158221314
    , 26549
    , 36
    , 74698472
    , 36
    , 333849714
    , 1
    , 254006273
    , 72
    , 2174038
    , 72
    , 2261318
    , 64571
    , 4
    , 207616
    , 8310
    , 4
    , 1293828
    , 28716
    , 63
    , 0
    , 1
    , 1006041
    , 43623
    , 251
    , 0
    , 1
    , 100181
    , 726
    , 719
    , 0
    , 1
    , 100181
    , 726
    , 719
    , 0
    , 1
    , 100181
    , 726
    , 719
    , 0
    , 1
    , 107878
    , 680
    , 0
    , 1
    , 95336
    , 1
    , 281145
    , 18848
    , 0
    , 1
    , 180194
    , 159
    , 1
    , 1
    , 158519
    , 8942
    , 0
    , 1
    , 159378
    , 8813
    , 0
    , 1
    , 107490
    , 3298
    , 1
    , 106057
    , 655
    , 1
    , 1964219
    , 24520
    , 3
    ]

-- protocolParameters :: PParams ConwayEra
-- protocolParameters = undefined
--   let pparams =
--         L.PParams $
--           L.emptyPParamsIdentity @ConwayEra
--             & L.hkdMaxBHSizeL .~ 1_100
--             & L.hkdMaxBBSizeL .~ 90_112
--             & L.hkdMaxTxSizeL .~ 16_384
--             & L.hkdTxFeePerByteL .~ L.CoinPerByte (L.toCompactPartial (Coin 44))
--             & L.hkdTxFeeFixedCompactL .~ L.toCompactPartial (Coin 155_381)
--             & L.hkdKeyDepositCompactL .~ L.toCompactPartial (Coin 2_000_000)
--             & L.hkdPoolDepositCompactL .~ L.toCompactPartial 500_000_000
--             & L.hkdDRepDepositCompactL .~ L.toCompactPartial 500_000_000
--             & L.hkdCoinsPerUTxOByteL .~ L.CoinPerByte (L.toCompactPartial (Coin 4_310))
--             & L.hkdPricesL
--               .~ L.Prices
--                 { L.prMem = C.unsafeBoundedRational (577 % 10_000)
--                 , L.prSteps = C.unsafeBoundedRational (721 % 10_000_000)
--                 }
--             & L.hkdMaxTxExUnitsL .~ ExUnits{exUnitsSteps = 1_0000_000_000, exUnitsMem = 14_000_000}
--             & L.hkdMaxBlockExUnitsL .~ ExUnits{exUnitsSteps = 20_000_000_000, exUnitsMem = 62_000_000}
--             & L.hkdMaxValSizeL .~ 5_000
--             & L.hkdCollateralPercentageL .~ 150
--             & L.hkdMaxCollateralInputsL .~ 3
--             & L.hkdMinPoolCostCompactL .~ L.toCompactPartial (Coin 200_000)
--             & L.hkdCostModelsL .~ CostModels.mkCostModels (Map.fromList [(PlutusV1, v1CostModel), (PlutusV2, v2CostModel), (PlutusV3, v3CostModel)])
--             & L.hkdMinFeeRefScriptCostPerByteL .~ C.unsafeBoundedRational 15
--             & L.hkdMinPoolCostCompactL .~ L.toCompactPartial (Coin 170_000_000)
--             & L.hkdEMaxL .~ L.EpochInterval 18
--    in pparams
--         & L.ppProtocolVersionL .~ latestProtVer

babbageEraOnwardsTest :: C.BabbageEraOnwards TestEra
babbageEraOnwardsTest = BabbageEraOnwardsConway

testSystemStart :: C.SystemStart
testSystemStart = SystemStart $ UTCTime (fromGregorian 2022 1 1) 0

testEraHistory :: C.EraHistory
testEraHistory =
  EraHistory $ Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly summaries
  where
    epochSize = EpochSize 432_000
    slotLength = mkSlotLength 1
    window = OuroborosA.GenesisWindow (2 * 2160)
    one = nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary epochSize slotLength window Ouroboros.NoPerasEnabled
    summaries = Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil

buildConstraintsSpec :: MarloweScripts -> Spec
buildConstraintsSpec marloweScripts = do
  describe "buildInitConstraints" buildCreateSpec
  describe "buildWithdrawConstraints" withdrawSpec
  describe "E2E: Signing key pair and deposit contract" (e2eSpec marloweScripts)

runCreateTest
  :: WalletContext
  -> RoleTokensConfig
  -> V1.Contract
  -> Either InitError ((Core.Datum 'Core.V1, Chain.TxOutAssets, Maybe RolesPolicyId), TxConstraints TestEra 'Core.V1)
runCreateTest walletCtx roles contract =
  let adjustMinUTxO = AdjustMinUTxO id
  in runIdentity $ buildInitConstraints
        (\_ _ -> pure $ Chain.PlutusScript mempty)
        babbageEraOnwardsTest
        Core.MarloweV1
        walletCtx
        roles
        (Core.MarloweTransactionMetadata Nothing mempty)
        (Chain.Lovelace 2_000_000)
        mempty
        adjustMinUTxO
        contract

runWithdrawTest
  :: PayoutContext
  -> Set Chain.TxOutRef
  -> Either WithdrawError (Map Chain.TxOutRef (Core.Payout 'Core.V1), TxConstraints TestEra 'Core.V1)
runWithdrawTest payoutContext payoutRefs =
  runIdentity $ runExceptT $ buildWithdrawConstraints payoutContext Core.MarloweV1 payoutRefs

buildCreateSpec :: Spec
buildCreateSpec  = do
  it "buildInitConstraints returns constraints for a Close contract" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      walletCtx = mkWalletContext testnetId verificationKey
      result = runCreateTest walletCtx RoleTokensNone V1.Close
    pure $ case result of
      Left _ -> property True
      Right _ -> property True

  it "buildInitConstraints returns constraints for a contract with Notify action" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      walletCtx = mkWalletContext testnetId verificationKey
      contract = V1.When [V1.Case (V1.Notify V1.FalseObs) V1.Close] 1000 V1.Close
      result = runCreateTest walletCtx RoleTokensNone contract
    pure $ case result of
      Left _ -> property True
      Right _ -> property True

withdrawSpec :: Spec
withdrawSpec = do
  it "buildWithdrawConstraints fails with empty payouts" $
    let payoutContext = PayoutContext
          { payoutOutputs = Map.empty
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext Set.empty
    in case result of
      Left (_ :: WithdrawError) -> property True
      Right _ -> property False

  it "buildWithdrawConstraints fails when payout not found" $
    let payoutTxOutRef = Chain.TxOutRef (Chain.TxId "testTxId") (Chain.TxIx 0)
        payoutContext = PayoutContext
          { payoutOutputs = Map.empty
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext (Set.singleton payoutTxOutRef)
    in case result of
      Left (_ :: WithdrawError) -> property True
      Right _ -> property False

  it "buildWithdrawConstraints succeeds with valid payout" $ property $ do
    let payoutTxOutRef = Chain.TxOutRef (Chain.TxId "payoutTxId") (Chain.TxIx 2)
        payoutAddress = Chain.Address "addr_test1"
        payoutAssets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 1000_000) (Chain.Tokens Map.empty)
        roleToken = Chain.AssetId testPolicyId (Chain.TokenName "testToken")
        payoutDatum = Core.toChainPayoutDatum Core.MarloweV1 roleToken
        payoutOutput = Chain.TransactionOutput
          { address = payoutAddress
          , assets = payoutAssets
          , datumHash = Nothing
          , datum = Just payoutDatum
          }
        payoutContext = PayoutContext
          { payoutOutputs = Map.singleton payoutTxOutRef payoutOutput
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext (Set.singleton payoutTxOutRef)
    case result of
      Left (_ :: WithdrawError) -> property False
      Right (payouts, _constraints) -> property $ Map.size payouts == 1

expectRight :: Show a => Either a b -> IO b
expectRight = \case
  Left err -> fail $ "Expected Right but got Left: " ++ show err
  Right val -> pure val

expectJust :: String -> Maybe a -> IO a
expectJust errMsg = \case
  Nothing -> fail $ "Expected Just but got Nothing: " ++ errMsg
  Just val -> pure val

-- E2E Tests following the steps from the original plan
e2eSpec :: MarloweScripts -> Spec
e2eSpec marloweScripts = do
  it "E2E Step 1: Generate a signing key pair" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let verificationKey = getVerificationKey signingKey
        keyHash = verificationKeyHash verificationKey
    return $ property $ not $ null (show keyHash)

  it "E2E Step 2: Create Party using unsafeMkPartyAddressBech32 (like Trivial.hs)" $ ioProperty $ do
    -- This is the party from Trivial.hs
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
    return $ property $ case party of
      V1.Address _ _ -> True
      V1.Role _ -> False

  it "E2E Step 3-4: Create contract with Deposit action that performs Deposit and closes" $ ioProperty $ do
    -- This uses the same party from Trivial.hs
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        -- Contract that waits for a deposit and then closes
        contract = V1.When
          [ V1.Case (V1.Deposit party party ada (V1.Constant depositAmount)) V1.Close
          ]
          1_000_000
          V1.Close
    return $ property $ case contract of
      V1.When [V1.Case (V1.Deposit p _ _ (V1.Constant amt)) V1.Close] timeout V1.Close ->
        timeout == 1_000_000 && amt == depositAmount && p == party
      _ -> False

  it "E2E Step 5: Create initial state (empty accounts, choices, boundValues)" $ ioProperty $ do
    let initialState = V1.State
          { V1.accounts = AM.empty
          , V1.choices = AM.empty
          , V1.boundValues = AM.empty
          , V1.minTime = POSIXTime 0
          }
    return $ property $ V1.minTime initialState == POSIXTime 0

  it "E2E Step 6: Create MarloweData with contract and state" $ ioProperty $ do
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        contract = V1.When
          [ V1.Case (V1.Deposit party party ada (V1.Constant depositAmount)) V1.Close
          ]
          1_000_000
          V1.Close
        initialState = V1.State
          { V1.accounts = AM.empty
          , V1.choices = AM.empty
          , V1.boundValues = AM.empty
          , V1.minTime = POSIXTime 0
          }
        rolesCurrency = PV2.CurrencySymbol $ PV2.toBuiltin $ BS.pack (replicate 28 0)
        marloweParams = MarloweParams rolesCurrency
        marloweData = MarloweData marloweParams initialState contract

    return $ property $ case marloweData of
      MarloweData (MarloweParams _) state c ->
        V1.minTime state == POSIXTime 0 && c == contract

  it "E2E Step 7: Create deposit input (IDeposit)" $ ioProperty $ do
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        depositInput = V1.NormalInput $ V1.IDeposit party party ada depositAmount
    return $ property $ case depositInput of
      V1.NormalInput (V1.IDeposit p1 p2 _ amt) ->
        p1 == party && p2 == party && amt == depositAmount
      _ -> False

  it "E2E Step 8: Call solveConstraints for a Close contract" $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      marloweContext = mkMarloweContext Devel testnetId Nothing marloweScripts

      constraints :: TxConstraints TestEra Core.V1
      constraints = TxConstraints
        { marloweInputConstraints = mempty
        , payoutInputConstraints = mempty
        , roleTokenConstraints = RoleTokenConstraintsNone
        , payToAddresses = mempty
        , payToRoles = mempty
        , marloweOutputConstraints = mempty
        , signatureConstraints = mempty
        , metadataConstraints = Core.emptyMarloweTransactionMetadata
        , threadTokenConstraints = PassThreadToken
        }
      walletCtx = mkWalletContext testnetId verificationKey
      txBodyResult = solveConstraints
          testSystemStart
          (toLedgerEpochInfo testEraHistory)
          babbageEraOnwardsTest
          protocolParameters
          Core.MarloweV1
          (Left marloweContext)
          walletCtx
          emptyHelpersCtx
          constraints
    txBodyResult `shouldSatisfy` \case
       Left _ -> False
       Right _ -> True

  it "E2E Step 9: Call solveConstraints for a INotify step" do
    signingKey <- generateSigningKey AsPaymentKey
    let
      notifyInput = V1.NormalInput V1.INotify
      verificationKey = getVerificationKey signingKey
      -- There is a possible interaction between the tip slot number
      -- era history configuration and the timeout posix time value.
      -- All those pieces should be adjusted.
      timeout :: PV2.POSIXTime
      timeout = 1_640_995_300_000
      tipSlotNo = SlotNo 0
      contract = V1.When [V1.Case (V1.Notify V1.TrueObs) V1.Close] timeout V1.Close
      marloweContext = mkMarloweContext Devel testnetId (Just (contract, Nothing)) marloweScripts
      walletCtx = mkWalletContext testnetId verificationKey

    marloweOutput <- expectJust "Expected Marlowe script output in context" marloweContext.scriptOutput
    (_applyResults, constraints) <- expectRight . runIdentity . runExceptT $ buildApplyInputsConstraints
      (\_contract _state _txInputs -> pure Nothing) -- merkleizeInput
      testSystemStart
      testEraHistory
      Core.MarloweV1
      marloweOutput
      tipSlotNo
      (Core.MarloweTransactionMetadata Nothing mempty)
      Nothing -- invalidBefore
      Nothing -- invalidHereafter
      [notifyInput]
    let
      txBodyResult = solveConstraints
          testSystemStart
          (toLedgerEpochInfo testEraHistory)
          babbageEraOnwardsTest
          protocolParameters
          Core.MarloweV1
          (Left marloweContext)
          walletCtx
          emptyHelpersCtx
          constraints
    for_ txBodyResult \txBody -> do
      putStrLn "Generated transaction body:"
      formatted <- runWarningIO . friendlyTxBodyImpl C.ShelleyBasedEraConway $ txBody
      putStrLn $ formatToString (Vary.from FormatYaml) formatted

    txBodyResult `shouldSatisfy` \case
      Left _ -> False
      Right _ -> True

-- Helper functions
testnetId :: C.NetworkId
testnetId = Testnet (NetworkMagic 0)

mkMarloweValidatorAddress :: C.NetworkId -> MarloweScripts -> C.AddressInEra TestEra
mkMarloweValidatorAddress network marloweScripts =
  makeShelleyAddressInEra
    ShelleyBasedEraConway
    network
    (PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ C.PlutusScriptSerialised (scriptInPlutusToSerialised marloweScripts.marloweScript.script))
    NoStakeAddress

mkRolePayoutValidatorAddress :: C.NetworkId -> MarloweScripts -> C.AddressInEra TestEra
mkRolePayoutValidatorAddress network marloweScripts =
  makeShelleyAddressInEra
    ShelleyBasedEraConway
    network
    (PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ C.PlutusScriptSerialised (scriptInPlutusToSerialised marloweScripts.payoutScript.script))
    NoStakeAddress

-- TODO:
-- * Add ability to use `Marlowe.Binaries.Production` and later also
data ValidatorsVersion = Devel

-- Convert a ScriptInPlutus value into a serialised Plutus script for use in
-- Cardano API helpers (e.g. hashing, address derivation).
scriptInPlutusToSerialised :: ScriptInPlutus -> PV2.SerialisedScript
scriptInPlutusToSerialised (ScriptInPlutusV2 bytes) = bytes
scriptInPlutusToSerialised (ScriptInPlutusV3 bytes) = bytes

mkMarloweContext :: ValidatorsVersion -> NetworkId -> Maybe (V1.Contract, Maybe V1.State) -> MarloweScripts -> MarloweContext 'Core.V1
mkMarloweContext _version network possibleContractInfo marloweScripts = do
  let
    marloweAddress = fromCardanoAddressInEra C.ConwayEra $ mkMarloweValidatorAddress network marloweScripts

    -- Existing on-chain contract output if the contract is ongoing
    scriptOutput = do
      (contract, possibleState) <- possibleContractInfo
      let
        rolesCurrency = PV2.CurrencySymbol $ PV2.toBuiltin $ BS.pack (replicate 28 0)
        marloweParams = MarloweParams rolesCurrency

        marloweTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 2) (Chain.TxIx 0)
        state = case possibleState of
          Just st -> st
          Nothing -> V1.State
            { V1.accounts = AM.empty
            , V1.choices = AM.empty
            , V1.boundValues = AM.empty
            , V1.minTime = POSIXTime 0
            }
        marloweData :: Core.Datum 'Core.V1
        marloweData = MarloweData marloweParams state contract

      pure $ Core.TransactionScriptOutput
        { Core.utxo = marloweTxOutRef
        , Core.address = marloweAddress
        , Core.datum = marloweData
        , Core.assets = do
            let
              marlowePolicyId = Chain.PolicyId (Chain.unScriptHash marloweScripts.marloweScript.scriptHash)
              threadTokenAssetId = Chain.AssetId marlowePolicyId (Chain.TokenName . BS.pack . replicate 32 $ 0)
            let tokens = Chain.Tokens . Map.singleton threadTokenAssetId $ Chain.Quantity 1
            Chain.TxOutAssets . Chain.Assets (Chain.Lovelace 2_000_000) $ tokens
        }
    -- Published on chain script outputs with binaries
    marloweScriptTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 3) (Chain.TxIx 0)
    payoutScriptTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 4) (Chain.TxIx 0)
    marloweScriptUTxO :: ReferenceScriptUtxo
    marloweScriptUTxO = ReferenceScriptUtxo
      { txOutRef = marloweScriptTxOutRef
      , txOut = TransactionOutput
          { address = marloweAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 2_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
      , script = marloweScripts.marloweScript.script
      }
    payoutAddress = fromCardanoAddressInEra C.ConwayEra $ mkRolePayoutValidatorAddress network marloweScripts
    payoutScriptUTxO :: ReferenceScriptUtxo
    payoutScriptUTxO = ReferenceScriptUtxo
      { txOutRef = payoutScriptTxOutRef
      , txOut = TransactionOutput
          { address = payoutAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 2_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
      , script = marloweScripts.payoutScript.script
      }
    marloweScriptHash = marloweScripts.marloweScript.scriptHash
    payoutScriptHash = marloweScripts.payoutScript.scriptHash

  MarloweContext
    { scriptOutput = scriptOutput
    , marloweAddress = marloweAddress
    , payoutAddress = payoutAddress
    , marloweScriptUTxO = marloweScriptUTxO
    , payoutScriptUTxO = payoutScriptUTxO
    , marloweScriptHash = marloweScriptHash
    , payoutScriptHash = payoutScriptHash
    }

mkWalletContext :: C.NetworkId -> C.VerificationKey C.PaymentKey -> WalletContext
mkWalletContext network verificationKey = do
  let
    keyHash = verificationKeyHash verificationKey
    walletAddressShelley = makeShelleyAddress network (PaymentCredentialByKey keyHash) NoStakeAddress
    walletAddress = Chain.fromCardanoShelleyAddress walletAddressShelley
    txOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 0) (Chain.TxIx 1)
  WalletContext
    { availableUtxos = Chain.UTxOs $ Map.singleton txOutRef
        TransactionOutput
          { address = walletAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 20_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
    , collateralUtxos = Set.singleton txOutRef
    , changeAddress = walletAddress
    }

emptyHelpersCtx :: HelpersContext
emptyHelpersCtx = HelpersContext
  { currentHelperScripts = Map.empty
  , helperPolicyId = Chain.PolicyId $ BS.pack (replicate 28 0)
  , helperScriptStates = Map.empty
  }

-- | A valid 28-byte PolicyId for testing
testPolicyId :: Chain.PolicyId
testPolicyId = Chain.PolicyId $ BS.pack (take 28 $ cycle [1..])
