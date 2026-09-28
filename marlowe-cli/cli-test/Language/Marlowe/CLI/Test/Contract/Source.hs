{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.Source where

import Marlowe.Contrib.Foldable (foldMapMFlipped)
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmptyList
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Marlowe.CLI.IO (liftCliMaybe)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError, rethrowCliError, testExecutionFailed')
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Wallet.Interpret (
  assetIdToToken,
  assetToPlutusValue,
  getSingletonCurrency,
  getWallet,
  getWalletByUniqueToken,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  Asset (Asset),
  AssetId,
  CurrencyNickname,
  Wallet (_waAddress),
  WalletNickname (WalletNickname),
  adaToken,
  faucetNickname,
  parseTokenNameJSON,
  tokenNameToJSON,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (CliError, SomeTimeout, toMarloweExtendedTimeout, toMarloweTimeout)
import Language.Marlowe.Core.V1.Semantics.Types qualified as C
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32, serialiseAddressBech32)
import Language.Marlowe.Core.V1.Semantics.Types.Address qualified as V1
import Language.Marlowe.Extended.V1 (toCore)
import Language.Marlowe.Extended.V1 qualified as E
import Marlowe.Contracts (coveredCall, escrow, swap, trivial, zeroCouponBond)
import Marlowe.Contracts.ChunkedValueTransfer (chunkedValueTransfer)
import Marlowe.Contracts.ChunkedValueTransfer qualified as ChunkedValueTransfer
import Marlowe.Contracts.Raffle (raffle)
import Marlowe.Contracts.Raffle qualified as Raffle
import PlutusLedgerApi.V1 (TokenName)
import PlutusLedgerApi.V2 qualified as Ledger

-- | We encode `PartyRef` as `Party` so we can use role based contracts
-- | without any change in the JSON structure.
-- | In the case of the `Address` you should use standard encoding but
-- | reference a wallet instead of providing hash value:
-- | ```
-- |  "address": "Wallet-1"
-- | ```
data PartyRef
  = WalletRef !WalletNickname
  | KnownAddress !(V1.Network, Ledger.Address)
  | RoleRef !TokenName
  deriving stock (Eq, Generic, Show)

instance FromJSON PartyRef where
  parseJSON = \case
    A.Object (KeyMap.toList -> [("address", A.String addr)]) ->
      case deserialiseAddressBech32 addr of
        Just parts -> pure $ KnownAddress parts
        Nothing ->
          pure . WalletRef . WalletNickname . T.unpack $ addr
    A.Object (KeyMap.toList -> [("role_token", roleTokenJSON)]) -> do
      roleToken <- parseTokenNameJSON roleTokenJSON
      pure $ RoleRef roleToken
    _ -> fail "Expecting a Party object."

instance ToJSON PartyRef where
  toJSON (WalletRef (WalletNickname walletNickname)) =
    A.object
      ["address" .= A.String (T.pack walletNickname)]
  toJSON (KnownAddress (network, address)) = do
    let address' = serialiseAddressBech32 network address
    A.object
      ["address" .= A.String address']
  toJSON (RoleRef tokenName) =
    A.object
      ["role_token" .= tokenNameToJSON tokenName]

data ValueTransferRecipient = ValueTransferRecipient
  { vtrRecipient :: !PartyRef
  , vtrAssets :: ![Asset]
  }
  deriving stock (Eq, Generic, Show)

instance A.FromJSON ValueTransferRecipient where
  parseJSON json = do
    obj <- parseJSON json
    vtrRecipient <- obj A..: "recipient"
    vtrAssets <- obj A..: "assets"
    pure ValueTransferRecipient{..}

instance A.ToJSON ValueTransferRecipient where
  toJSON ValueTransferRecipient{..} =
    A.object
      [ "recipient" .= vtrRecipient
      , "assets" .= vtrAssets
      ]

data UseTemplate
  = UseTrivial
      { utParty :: Maybe PartyRef
      -- ^ The party. Falls back to the faucet wallet pkh.
      , utDepositLovelace :: Integer
      -- ^ Lovelace in the deposit.
      , utWithdrawalLovelace :: Integer
      -- ^ Lovelace in the withdrawal.
      , utTimeout :: SomeTimeout
      -- ^ The timeout.
      }
  | -- | Use for escrow contract.
    UseEscrow
      { utPrice :: Integer
      -- ^ Price of the item for sale, in lovelace.
      , utSeller :: PartyRef
      -- ^ Defaults to a wallet with the "Buyer" nickname.
      , utBuyer :: PartyRef
      -- ^ Defaults to a wallet with the "Seller" nickname.
      , utMediator :: PartyRef
      -- ^ The mediator.
      , utPaymentDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to pay.
      , utComplaintDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to complain.
      , utDisputeDeadline :: SomeTimeout
      -- ^ The deadline for the seller to dispute a complaint.
      , utMediationDeadline :: SomeTimeout
      -- ^ The deadline for the mediator to decide.
      }
  | -- | Use for swap contract.
    UseSwap
      { utAParty :: PartyRef
      -- ^ First party
      , utAAsset :: Asset
      , utATimeout :: SomeTimeout
      -- ^ Timeout for first party's deposit.
      , utBParty :: PartyRef
      -- ^ Second party. Falls back to wallet with "B" nickname.
      , utBAsset :: Asset
      , utBTimeout :: SomeTimeout
      -- ^ Timeout for second party's deposit.
      }
  | -- | Use for zero-coupon bond.
    UseZeroCouponBond
      { utLender :: PartyRef
      -- ^ The lender.
      , utBorrower :: PartyRef
      -- ^ The borrower.
      , utPrincipal :: Integer
      -- ^ The principal.
      , utInterest :: Integer
      -- ^ The interest.
      , utLendingDeadline :: SomeTimeout
      -- ^ The lending deadline.
      , utPaybackDeadline :: SomeTimeout
      -- ^ The payback deadline.
      }
  | -- | Use for covered call.
    UseCoveredCall
      { utIssuer :: PartyRef
      -- ^ The issuer.
      , utCounterParty :: PartyRef
      -- ^ The counter-party.
      , utCurrency :: AssetId
      -- ^ The currency token.
      , utStrike :: Integer
      -- ^ The strike in currency.
      , utUnderlying :: AssetId
      -- ^ The underlying token.
      , utAmount :: Integer
      -- ^ The amount of underlying.
      , utIssueDate :: SomeTimeout
      -- ^ The issue date.
      , utMaturityDate :: SomeTimeout
      -- ^ The maturity date.
      , utSettlementDate :: SomeTimeout
      -- ^ The settlement date.
      }
  | -- | Use for actus contracts.
    UseActus
      { utParty :: Maybe PartyRef
      -- ^ The party. Falls back to the faucet wallet.
      , utCounterParty :: PartyRef
      -- ^ The counter-party.
      , utActusTermsFile :: FilePath
      -- ^ The Actus contract terms.
      }
  | UseRaffle
      { utSponsor :: PartyRef
      , utOracle :: PartyRef
      , utChunkSize :: Raffle.ChunkSize
      , utParties :: NonEmpty PartyRef
      , utprizeNFTPerRound :: [AssetId]
      , utDepositDeadline :: SomeTimeout
      , utSelectDeadline :: SomeTimeout
      , utPayoutDeadline :: SomeTimeout
      }
  | UseChunkedValueTransfer
      { utSender :: PartyRef
      , utRecipientsAmounts :: [ValueTransferRecipient]
      , utTimeout :: SomeTimeout
      , utPayoutChunkSize :: ChunkedValueTransfer.PayoutChunkSize
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON UseTemplate where
  parseJSON = Operation.parseConstructorBasedJSON' "ut"

instance ToJSON UseTemplate where
  toJSON = Operation.toConstructorBasedJSON "ut"

data Source
  = InlineContract ParametrizedMarloweJSON
  | UseTemplate UseTemplate
  deriving stock (Eq, Generic, Show)

instance ToJSON Source where
  toJSON (InlineContract c) = A.object [("inline", toJSON c)]
  toJSON (UseTemplate templateCommand) = A.object [("template", toJSON templateCommand)]

instance FromJSON Source where
  parseJSON json = case json of
    A.Object (KeyMap.toList -> [("inline", contractJson)]) -> do
      parsedContract <- parseJSON contractJson
      pure $ InlineContract parsedContract
    A.Object (KeyMap.toList -> [("template", templateCommandJson)]) -> do
      parsedTemplateCommand <- parseJSON templateCommandJson
      pure $ UseTemplate parsedTemplateCommand
    _ -> fail "Expected object with a single field of either `inline` or `template`"

makeContract' :: (MonadError InterpreterError m) => E.Contract -> m C.Contract
makeContract' = rethrowCliError . makeContract

-- | Conversion from Extended to Core Marlowe.
makeContract :: (MonadError CliError m) => E.Contract -> m C.Contract
makeContract = liftCliMaybe "Conversion from Extended to Core Marlowe failed!" . toCore

buildParty
  :: (Wallet.InterpretMonad env st m era)
  => Maybe CurrencyNickname
  -> PartyRef
  -> m M.Party
buildParty mRoleCurrency = \case
  WalletRef nickname -> do
    wallet <- getWallet nickname
    uncurry M.Address <$> rethrowCliError (marloweAddressFromCardanoAddress (_waAddress wallet))
  KnownAddress (n, a) -> pure $ M.Address n a
  RoleRef token -> do
    -- Consistency check
    currency <- case mRoleCurrency of
      Nothing -> fst <$> getSingletonCurrency
      Just cn -> pure cn
    void $ getWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token

useTemplate
  :: (Wallet.InterpretMonad env st m era)
  => Maybe CurrencyNickname
  -> UseTemplate
  -> m M.Contract
useTemplate currency = \case
  UseTrivial{..} -> do
    timeout' <- toMarloweExtendedTimeout utTimeout
    let partyRef = fromMaybe (WalletRef faucetNickname) utParty
    party <- buildParty currency partyRef
    makeContract' $
      trivial
        party
        utDepositLovelace
        utWithdrawalLovelace
        timeout'
  UseSwap{..} -> do
    aTimeout' <- toMarloweExtendedTimeout utATimeout
    bTimeout' <- toMarloweExtendedTimeout utBTimeout

    let Asset aAssetId aAmount = utAAsset
        Asset bAssetId bAmount = utBAsset

    aToken <- assetIdToToken aAssetId
    bToken <- assetIdToToken bAssetId

    aParty <- buildParty currency utAParty
    bParty <- buildParty currency utBParty

    makeContract' $
      swap
        aParty
        aToken
        (E.Constant aAmount)
        aTimeout'
        bParty
        bToken
        (E.Constant bAmount)
        bTimeout'
        E.Close
  UseEscrow{..} -> do
    paymentDeadline' <- toMarloweExtendedTimeout utPaymentDeadline
    complaintDeadline' <- toMarloweExtendedTimeout utComplaintDeadline
    disputeDeadline' <- toMarloweExtendedTimeout utDisputeDeadline
    mediationDeadline' <- toMarloweExtendedTimeout utMediationDeadline

    seller <- buildParty currency utSeller
    buyer <- buildParty currency utBuyer
    mediator <- buildParty currency utMediator

    makeContract' $
      escrow
        (E.Constant utPrice)
        seller
        buyer
        mediator
        paymentDeadline'
        complaintDeadline'
        disputeDeadline'
        mediationDeadline'
  UseCoveredCall{..} -> do
    issueDate <- toMarloweExtendedTimeout utIssueDate
    maturityDate <- toMarloweExtendedTimeout utMaturityDate
    settlementDate <- toMarloweExtendedTimeout utSettlementDate
    issuer <- buildParty currency utIssuer
    counterParty <- buildParty currency utCounterParty

    ccCurrency <- assetIdToToken utCurrency
    underlying <- assetIdToToken utUnderlying

    makeContract' $
      coveredCall
        issuer
        counterParty
        Nothing
        ccCurrency
        underlying
        (E.Constant utStrike)
        (E.Constant utAmount)
        issueDate
        maturityDate
        settlementDate
  UseZeroCouponBond{..} -> do
    lendingDeadline <- toMarloweExtendedTimeout utLendingDeadline
    paybackDeadline <- toMarloweExtendedTimeout utPaybackDeadline

    lender <- buildParty currency utLender
    borrower <- buildParty currency utBorrower

    makeContract' $
      zeroCouponBond
        lender
        borrower
        lendingDeadline
        paybackDeadline
        (E.Constant utPrincipal)
        (E.Constant utPrincipal `E.AddValue` E.Constant utInterest)
        adaToken
        E.Close
  UseRaffle{..} -> do
    depositDeadline' <- toMarloweTimeout utDepositDeadline
    selectDeadline' <- toMarloweTimeout utSelectDeadline
    payoutDeadline' <- toMarloweTimeout utPayoutDeadline

    sponsor <- buildParty currency utSponsor
    oracle <- buildParty currency utOracle
    parties <- traverse (buildParty currency) utParties

    possiblePrizeNFTPerRound <- traverse assetIdToToken utprizeNFTPerRound
    prizeNFTPerRound <- case NonEmptyList.nonEmpty possiblePrizeNFTPerRound of
      Nothing -> throwError $ testExecutionFailed' "Price NFT per round must be specified"
      Just nfts -> pure nfts

    pure $
      raffle
        (Raffle.Sponsor sponsor)
        (Raffle.Oracle oracle)
        utChunkSize
        parties
        prizeNFTPerRound
        depositDeadline'
        selectDeadline'
        payoutDeadline'
  UseChunkedValueTransfer{..} -> do
    timeout' <- toMarloweTimeout utTimeout
    sender <- buildParty currency utSender
    recipientsAmounts <- foldMapMFlipped utRecipientsAmounts \ValueTransferRecipient{vtrRecipient, vtrAssets} -> do
      recipient <- buildParty currency vtrRecipient
      value <- foldMapMFlipped vtrAssets assetToPlutusValue
      pure $ Map.fromList [(ChunkedValueTransfer.Recipient recipient, value)]
    pure $
      chunkedValueTransfer
        (ChunkedValueTransfer.Sender sender)
        (ChunkedValueTransfer.RecipientsAmounts recipientsAmounts)
        utPayoutChunkSize
        timeout'
  template -> throwError $ testExecutionFailed' $ "Template not implemented: " <> show template
