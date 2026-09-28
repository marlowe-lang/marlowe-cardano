{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Wallet.Interpret where

import Cardano.Api (
  AddressInEra,
  AsType (AsPaymentKey),
  BabbageEraOnwards,
  File (..),
  Key (verificationKeyHash),
  PaymentCredential (PaymentCredentialByKey),
  StakeAddressReference (NoStakeAddress),
  generateSigningKey,
  makeShelleyAddressInEra,
 )
import Cardano.Api qualified as C
import Cardano.Api qualified as CAS
import Contrib.Control.Monad.Except (note)
import Marlowe.Contrib.Foldable (foldMapFlipped, ifoldMapMFlipped)
import Control.Category ((>>>))
import Control.Error.Util qualified as Error
import Control.Lens (ifor_, modifying, use, view, (.=))
import Control.Monad (forM, unless, void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson qualified as A
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Base16.Aeson (EncodeBase16 (..))
import Data.Coerce (coerce)
import Data.Fixed qualified as F
import Data.Foldable (Foldable (fold), find, for_)
import Data.Foldable qualified as Foldable
import Data.Foldable.WithIndex (ifoldrM)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable (for)
import Data.Tuple.Extra (uncurry3)
import Language.Marlowe.CLI.Cardano.Api (toTxOutDatumInline)
import Language.Marlowe.CLI.Cardano.Api.Value (lovelaceToPlutusValue, toCurrencySymbol, toPlutusValue)
import Language.Marlowe.CLI.Cardano.Api.Value qualified as CV
import Language.Marlowe.CLI.IO (readSigningKey, submitTxBody')
import Language.Marlowe.CLI.Run (toCardanoPolicyId, toPlutusAddress)
import Language.Marlowe.CLI.Test.CLI.Monad (runLabeledCli)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (
  ParametrizedMarloweJSON,
  decodeParametrizedContractJSON,
  decodeParametrizedInputJSON,
  decodeParametrizedStateJSON,
  now,
 )
import Language.Marlowe.CLI.Test.ExecutionMode (queryByAddress, queryUTxOs)
import Language.Marlowe.CLI.Test.ExecutionMode qualified as EM
import Language.Marlowe.CLI.Test.InterpreterError (assertionFailed', testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (Label, logStoreLabeledMsg, logStoreMsgWith, logTxBody, throwLabeledError)
import Language.Marlowe.CLI.Test.Report qualified as Report
import Language.Marlowe.CLI.Test.Wallet.Types (
  Asset (Asset),
  AssetId (AdaAssetId, AssetId),
  AssetsBalance (AssetsBalance),
  Currencies (Currencies),
  Currency (Currency, ccCurrencySymbol, ccIssuer),
  CurrencyNickname,
  HasInterpretEnv (..),
  InterpretMonad,
  IsExternalWallet (..),
  SomeTxBody (..),
  ThreadTokenName (..),
  TokenAssignment (TokenAssignment),
  TokenRecipient (..),
  TokenRecipientScript (..),
  Wallet (Wallet, _waAddress, _waBalanceCheckBaseline, _waSigningKey, _waSubmittedTransactions),
  WalletNickname (WalletNickname),
  WalletOperation (..),
  adaToken,
  allWalletsMap,
  checkBalance,
  currenciesL,
  emptyWallet,
  eraL,
  faucetNickname,
  insertWallet,
  mapWallets,
  printStatsL,
  unWalletNickname,
  walletTxFees,
  walletsL,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as T
import Language.Marlowe.CLI.Transaction (
  buildBodyWithContent,
  buildFaucetImpl,
  buildMintingImpl,
 )
import Language.Marlowe.CLI.Types (
  AUTxO (unAUTxO),
  CurrencyIssuer (CurrencyIssuer),
  MarloweScriptsRefs (..),
  PayFromScript,
  SigningKeyFile (..),
  SomePaymentSigningKey (..),
  TokensRecipient (..),
  defaultCoinSelectionStrategy,
  getVerificationKey,
  queryContextNetworkId,
  toPaymentVerificationKey,
  toQueryContext,
  validatorAddress,
 )
import Language.Marlowe.CLI.Types qualified as CT
import Language.Marlowe.Cardano (marloweNetworkFromCardanoNetworkId)
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Scripts (openRolesValidator)
import PlutusLedgerApi.V1 (CurrencySymbol, TokenName)
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V1.Value qualified as P
import PlutusLedgerApi.V2 qualified as P
import PlutusTx.AssocMap qualified as AssocMap

import Cardano.Api.Ledger qualified as Ledger
import PlutusTx.Builtins.HasOpaque
import PlutusTx.Monoid (Group (inv))
import System.IO.Temp (emptySystemTempFile, emptyTempFile)

findWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> m (Either String (Wallet era))
findWallet nickname = do
  wallets <- getAllWallets
  pure $ Error.note ("[getWallet] Unable to find wallet:" <> show nickname) (Map.lookup nickname wallets)

getWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> m (Wallet era)
getWallet nickname = findWallet nickname >>= (Bifunctor.first testExecutionFailed' >>> liftEither)

updateWallet
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> (Wallet era -> Wallet era)
  -> m ()
updateWallet nickname update = do
  wallet <- getWallet nickname
  let wallet' = update wallet
  wallets <- use walletsL
  case T.updateWallet nickname wallet' wallets of
    Nothing -> throwError $ testExecutionFailed' $ "[updateWallet] Unable to update wallet:" <> show nickname
    Just wallets' -> walletsL .= wallets'

updateWallets
  :: (InterpretMonad env st m era)
  => (Wallet era -> Wallet era)
  -> m ()
updateWallets f = do
  wallets <- use walletsL
  let wallets' = mapWallets f wallets
  walletsL .= wallets'

addWalletTransaction
  :: (InterpretMonad env st m era)
  => (Label l)
  => (C.IsShelleyBasedEra era)
  => WalletNickname
  -> l
  -> String
  -> C.TxBody era
  -> m ()
addWalletTransaction nickname label msg txBody = do
  era <- view eraL
  wallets <- use walletsL
  logTxBody label msg txBody (Report.rewriteAddress wallets)
  updateWallet nickname $ \wallet ->
    wallet
      { _waSubmittedTransactions = SomeTxBody era txBody : _waSubmittedTransactions wallet
      }

addPublishingCosts
  :: (InterpretMonad env st m era)
  => WalletNickname
  -> MarloweScriptsRefs lang era
  -> m ()
addPublishingCosts nickname marloweScriptsRefs = do
  let MarloweScriptsRefs{mrMarloweValidator, mrRolePayoutValidator, mrOpenRoleValidator} = marloweScriptsRefs
      total =
        foldMap
          (toPlutusValue . C.txOutValueToValue . CV.txOutValue . snd . unAUTxO . fst)
          [mrMarloweValidator, mrRolePayoutValidator, mrOpenRoleValidator]
  updateWallet nickname $ \wallet ->
    wallet
      { T._waPublishingCosts = T._waPublishingCosts wallet <> total
      }

getAllWallets
  :: (InterpretMonad env st m era)
  => m (Map WalletNickname (Wallet era))
getAllWallets = use walletsL <&> allWalletsMap

getFaucet
  :: (InterpretMonad env st m era)
  => m (Wallet era)
getFaucet =
  getWallet faucetNickname

updateFaucet
  :: (InterpretMonad env st m era)
  => (Wallet era -> Wallet era)
  -> m ()
updateFaucet = updateWallet faucetNickname

openRoleValidatorAddress
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m (AddressInEra era)
openRoleValidatorAddress = do
  era <- view eraL
  networkId <- getNetworkId
  pure $ validatorAddress openRolesValidator era networkId NoStakeAddress

getNetworkId
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m C.NetworkId
getNetworkId = do
  txBuildupCtx <- view txBuildupContextL
  pure $ queryContextNetworkId $ toQueryContext txBuildupCtx

getQueryContext
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m (CT.QueryExecutionContext era)
getQueryContext = do
  txBuildupCtx <- view txBuildupContextL
  pure $ toQueryContext txBuildupCtx

fetchWalletUTxOs
  :: (InterpretMonad env st m era)
  => Wallet era
  -> m [AUTxO era]
fetchWalletUTxOs (_waAddress -> address) = do
  utxo <- queryByAddress address
  pure $ CT.fromUTxO utxo

fetchWalletsUTxOs
  :: (InterpretMonad env st m era)
  => m (Map.Map WalletNickname [AUTxO era])
fetchWalletsUTxOs = do
  wallets <- getAllWallets
  let walletsAddresses = wallets <&> \wallet -> _waAddress wallet

  (CT.fromUTxO -> utxos) <-
    queryUTxOs
      . C.QueryUTxOByAddress
      . Set.fromList
      . fmap CT.toAddressAny'
      . Foldable.toList
      $ walletsAddresses

  let addressToUtxos =
        Map.fromListWith mappend $
          utxos <&> \anUtxo@(CT.AUTxO (_, C.TxOut addr _ _ _)) ->
            (toPlutusAddress addr, [anUtxo])

  pure $
    wallets <&> \wallet ->
      fromMaybe mempty $ Map.lookup (toPlutusAddress $ _waAddress wallet) addressToUtxos

fetchWalletsValue
  :: forall env st m era
   . (InterpretMonad env st m era)
  => m (Map.Map WalletNickname P.Value)
fetchWalletsValue = do
  (walletsUtxos :: Map.Map WalletNickname [CT.AUTxO era]) <- fetchWalletsUTxOs
  pure $
    walletsUtxos <&> foldMap \(CT.AUTxO (_, C.TxOut _ txOutValue _ _)) ->
      CV.toPlutusValue $ C.txOutValueToValue txOutValue

fetchWalletValue
  :: forall env st m era
   . (InterpretMonad env st m era)
  => Wallet era
  -> m P.Value
fetchWalletValue wallet = do
  walletUtxos <- fetchWalletUTxOs wallet
  pure $ foldMapFlipped walletUtxos \(CT.AUTxO (_, C.TxOut _ txOutValue _ _)) ->
    CV.toPlutusValue $ C.txOutValueToValue txOutValue

getSingletonCurrency
  :: (InterpretMonad env st m era)
  => m (CurrencyNickname, Currency)
getSingletonCurrency = do
  Currencies currencies <- use currenciesL
  case Map.toList currencies of
    [c] -> pure c
    _ -> throwError $ testExecutionFailed' "Ambiguous currency lookup."

getCurrency
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> m Currency
getCurrency nickname = do
  (Currencies currencies) <- use currenciesL
  note
    (testExecutionFailed' $ "[getCurrency] Unable to find currency:" <> show nickname)
    $ Map.lookup nickname currencies

findCurrency
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> m (Either String Currency)
findCurrency nickname = do
  (Currencies currencies) <- use currenciesL
  pure $ Error.note ("[getCurrency] Unable to find currency:" <> show nickname) $ Map.lookup nickname currencies

getCurrencyBySymbol
  :: (InterpretMonad env st m era)
  => CurrencySymbol
  -> m (CurrencyNickname, Currency)
getCurrencyBySymbol currencySymbol = do
  (Currencies currencies) <- use currenciesL
  let possibleCurrency =
        find
          (\(_, c) -> ccCurrencySymbol c == currencySymbol)
          (Map.toList currencies)
  note
    (testExecutionFailed' $ "[getCurrencyBySymbol] Unable to find currency:" <> show currencySymbol)
    possibleCurrency

getWalletByAddress
  :: (InterpretMonad env st m era)
  => C.AddressInEra era
  -> m (WalletNickname, Wallet era)
getWalletByAddress address = do
  wallets <- getAllWallets
  let wallet = find (\(_, w) -> address == _waAddress w) (Map.toList wallets)
  note
    ( testExecutionFailed' $
        "[getWalletByPkh] Wallet not found for a given address: " <> show address <> " in wallets: " <> show wallets
    )
    wallet

getWalletByUniqueToken
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> TokenName
  -> m (WalletNickname, Wallet era)
getWalletByUniqueToken currencyNickname tokenName =
  findWalletByUniqueToken currencyNickname tokenName >>= (Bifunctor.first testExecutionFailed' >>> liftEither)

findWalletByUniqueToken
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -> TokenName
  -> m (Either String (WalletNickname, Wallet era))
findWalletByUniqueToken currencyNickname tokenName = runExceptT do
  Currency{..} <- ExceptT (findCurrency currencyNickname)
  let check value = valueOf value ccCurrencySymbol tokenName == 1
      step n value Nothing =
        pure $
          if check value
            then Just n
            else Nothing
      step n value (Just res) =
        if check value
          then throwError $ "[findByUniqueToken] Token is not unique - found in two wallets: " <> show n <> " and " <> show res <> "."
          else pure $ Just res
  wallet2value <- lift fetchWalletsValue
  possibleOwnerNickname <- ifoldrM step Nothing wallet2value
  ownerNickname <-
    liftEither $
      Error.note
        ("[getWalletByUniqueToken] Wallet not found for a given token: " <> show ccCurrencySymbol <> ":" <> show tokenName)
        possibleOwnerNickname
  (ownerNickname,) <$> (ExceptT $ findWallet ownerNickname)

getWalletsByCurrencyTokens
  :: (InterpretMonad env st m era)
  => CurrencyNickname
  -- ^ Currency to look for
  -> Maybe [TokenName]
  -- ^ If Nothing, then we check if the wallet has any tokens of the given currency.
  -> m [(WalletNickname, Wallet era, List.NonEmpty P.TokenName)]
getWalletsByCurrencyTokens currencyNickname possibleTokenNames = do
  Currency{..} <- getCurrency currencyNickname
  let findTokens :: P.Value -> Maybe (List.NonEmpty P.TokenName)
      findTokens value@(P.Value valueMap) = do
        let valueTokenNames = case possibleTokenNames of
              Just tokenNames ->
                filter (\tokenName -> valueOf value ccCurrencySymbol tokenName > 0) tokenNames
              Nothing -> do
                case AssocMap.lookup ccCurrencySymbol valueMap of
                  Just tokenMap -> AssocMap.keys tokenMap
                  _ -> []
        List.NonEmpty.nonEmpty valueTokenNames

  wallet2value <- fetchWalletsValue
  let matches =
        catMaybes $
          Map.toList wallet2value <&> \(n, value) -> do
            tokenNames <- findTokens value
            pure (n, tokenNames)
  for matches $ \(n, tokenNames) -> do
    wallet <- getWallet n
    pure (n, wallet, tokenNames)

assetToMarloweValuePair :: (InterpretMonad env st m era) => Asset -> m (M.Token, Integer)
assetToMarloweValuePair (Asset AdaAssetId amount) = pure (M.Token "" "", amount)
assetToMarloweValuePair (Asset (AssetId currencyNickname tokenName) amount) = do
  Currency{ccCurrencySymbol} <- getCurrency currencyNickname
  pure (M.Token ccCurrencySymbol tokenName, amount)

assetToPlutusValue
  :: (InterpretMonad env st m era)
  => Asset
  -> m P.Value
assetToPlutusValue (Asset AdaAssetId amount) = pure $ P.singleton P.adaSymbol P.adaToken amount
assetToPlutusValue (Asset (AssetId currencyNickname tokenName) amount) = do
  Currency{ccCurrencySymbol} <- getCurrency currencyNickname
  pure $ P.singleton ccCurrencySymbol tokenName amount

plutusValueToAssets
  :: (InterpretMonad env st m era)
  => P.Value
  -> m [Asset]
plutusValueToAssets value = do
  let valueToAsset (currencySymbol, tokenName, _)
        | currencySymbol == P.adaSymbol && tokenName == P.adaToken =
            pure (Asset AdaAssetId (P.fromBuiltin $ P.valueOf value currencySymbol tokenName))
      valueToAsset (cCurrencySymbol, tokenName, amount) = do
        let
        (currencyNickname, _) <- getCurrencyBySymbol cCurrencySymbol
        pure (Asset (AssetId currencyNickname tokenName) amount)
  for (P.flattenValue value) valueToAsset

assetIdToToken
  :: (InterpretMonad env st m era)
  => AssetId
  -> m M.Token
assetIdToToken (AssetId currencyNickname currencyToken) = do
  Currency{ccCurrencySymbol = currencySymbol} <- getCurrency currencyNickname
  pure $ M.Token currencySymbol currencyToken
assetIdToToken AdaAssetId = pure adaToken

-- TODO: Move these three to a separate Interpret helpers module.
-- They have NOTHING to do with Wallet interpretation.
decodeInputJSON
  :: (InterpretMonad env st m era)
  => ParametrizedMarloweJSON
  -> m M.Input
decodeInputJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- getNetworkId <&> marloweNetworkFromCardanoNetworkId
  n <- now
  case decodeParametrizedInputJSON network wallets currencies n json of
    Left err -> throwError $ testExecutionFailed' $ "Failed to decode input: " <> show err
    Right i -> pure i

decodeContractJSON
  :: (InterpretMonad env st m era)
  => ParametrizedMarloweJSON
  -> m M.Contract
decodeContractJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- getNetworkId <&> marloweNetworkFromCardanoNetworkId
  n <- now
  case decodeParametrizedContractJSON network wallets currencies n json of
    Left err -> throwError $ testExecutionFailed' $ "Failed to decode contract: " <> show err
    Right c -> pure c

decodeStateJSON
  :: (InterpretMonad env st m era)
  => ParametrizedMarloweJSON
  -> m M.State
decodeStateJSON json = do
  currencies <- use currenciesL
  wallets <- use walletsL
  network <- getNetworkId <&> marloweNetworkFromCardanoNetworkId
  case decodeParametrizedStateJSON network wallets currencies json of
    Left err -> throwError $ testExecutionFailed' $ "Failed to decode state: " <> show err
    Right c -> pure c

interpret
  :: forall env era st m
   . (C.IsShelleyBasedEra era)
  => (InterpretMonad env st m era)
  => WalletOperation
  -> m ()
interpret so@CheckBalance{..} = do
  wallet@Wallet{..} <- getWallet woWalletNickname
  utxos <- fetchWalletUTxOs wallet :: m [AUTxO era]
  let onChainTotal = CV.toPlutusValue $ foldMap CT.aUTxOValue utxos
      fees = walletTxFees wallet
      administrativeCosts = lovelaceToPlutusValue fees <> _waPublishingCosts

  logStoreLabeledMsg so $ "Checking balance of a wallet: " <> show case woWalletNickname of WalletNickname n -> n
  logStoreLabeledMsg so $ "Number of already submitted transactions: " <> show (length _waSubmittedTransactions)
  logStoreLabeledMsg so $ "Balance check baseline: " <> show _waBalanceCheckBaseline
  logStoreLabeledMsg so $ "Publishing costs: " <> show _waPublishingCosts
  logStoreLabeledMsg so $
    "Total transaction fees amount: " <> do
      let Ledger.Coin amount = fees
      show ((fromInteger amount / 1_000_000) :: F.Micro) <> " ADA"

  actualBalance <- plutusValueToAssets $ onChainTotal <> administrativeCosts <> inv _waBalanceCheckBaseline
  let AssetsBalance expectedBalance = woBalance

  -- Iterate over assets in the wallet and check if we meet the expected balance.
  for_ actualBalance \(Asset assetId v) -> do
    case Map.lookup assetId expectedBalance of
      Nothing ->
        when (v /= 0) do
          throwLabeledError so $
            assertionFailed' ("Unexpected asset in balance check:" <> show v <> " of asset: " <> show assetId)
      Just expectedValue ->
        unless (checkBalance expectedValue v) do
          logStoreLabeledMsg so $ "Actual balance of asset " <> show assetId <> " is:" <> show v
          logStoreLabeledMsg so $ "Expected balance of asset " <> show assetId <> " is: " <> show expectedValue
          throwLabeledError so $ assertionFailed' $ "Balance check failed for an asset: " <> show assetId

  -- Iterate over expected assets and check if we have them in the wallet.
  --  * If we have them then we already checked the balance above.
  --  * If we miss an asset, we check if the expected balance is 0.
  for_ (Map.toList expectedBalance) \(assetId, expectedValue) -> do
    let actualAssets = map (\(Asset k _) -> k) actualBalance
    unless (assetId `elem` actualAssets || checkBalance expectedValue 0) do
      logStoreLabeledMsg so $ "Expected balance of an asset " <> show assetId <> " is:" <> show expectedValue
      throwLabeledError so $ assertionFailed' $ "Asset is missing from the wallet on chain balance:" <> show assetId
interpret so@(CreateWallet woWalletNickname possibleUTxOs) = do
  let WalletNickname rawNickname = woWalletNickname
  era <- view eraL
  wallet@Wallet{_waAddress = address} <- createWallet era

  logStoreLabeledMsg so $
    "Wallet " <> show rawNickname <> " created with an address: " <> Text.unpack (C.serialiseAddress address)

  (addrFile, CT.SigningKeyFile skeyFile) <- liftIO $ saveWalletFiles woWalletNickname wallet Nothing
  logStoreLabeledMsg so $ "Wallet info stored in " <> addrFile <> " and " <> skeyFile

  wallets <- use walletsL
  case insertWallet woWalletNickname wallet wallets of
    Nothing -> throwError $ testExecutionFailed' $ "Wallet with nickname " <> show rawNickname <> " already exists"
    Just wallets' -> walletsL .= wallets'

  case possibleUTxOs of
    Nothing -> pure ()
    Just utxos -> do
      logStoreLabeledMsg so $ "Funding wallet " <> show rawNickname <> " with " <> show (length utxos) <> " UTxOs"
      fundWallets so [woWalletNickname] utxos
interpret so@ExternalWallet{..} = do
  era <- view eraL
  skey <- runLabeledCli era so $ readSigningKey (SigningKeyFile woSigningKeyFile)
  networkId <- getNetworkId
  let vkey = toPaymentVerificationKey $ getVerificationKey skey
      (address :: AddressInEra era) =
        makeShelleyAddressInEra
          (C.babbageEraOnwardsToShelleyBasedEra era)
          networkId
          (PaymentCredentialByKey (verificationKeyHash vkey))
          NoStakeAddress

  utxo <- EM.queryByAddress address
  let wallet = T.fromUTxO address skey utxo (IsExternalWallet True)
  wallets <- use walletsL
  case insertWallet woWalletNickname wallet wallets of
    Nothing ->
      throwError $
        testExecutionFailed' $
          "Wallet with nickname " <> show (unWalletNickname woWalletNickname) <> " already exists"
    Just wallets' -> do
      logStoreLabeledMsg so $
        "Wallet "
          <> show (unWalletNickname woWalletNickname)
          <> " loaded with an address: "
          <> Text.unpack (C.serialiseAddress address)
      walletsL .= wallets'
interpret so@BurnAll{..} = do
  Currencies (Map.toList -> currencies) <- use currenciesL
  allWalletsUtxos <- fold <$> fetchWalletsUTxOs
  wallets <- getAllWallets
  let initialTotal = toPlutusValue $ foldMap CT.aUTxOValue allWalletsUtxos
      initialSymbols = Set.fromList $ P.symbols initialTotal
  for_ currencies \(currencyNickname, Currency{ccIssuer = possibleIssuer, ccCurrencySymbol, ccMintingExpirationSlot}) -> case possibleIssuer of
    Just issuer ->
      if not (ccCurrencySymbol `Set.member` initialSymbols)
        then logStoreLabeledMsg so $ "Currency " <> show currencyNickname <> " tokens not found, skipping."
        else do
          Wallet{_waAddress = issuerAddress, _waSigningKey = issuerSigningKey} <- getWallet issuer
          let providers = Map.elems wallets <&> \Wallet{_waAddress, _waSigningKey} -> (_waAddress, _waSigningKey)
              currencyIssuer = CT.CurrencyIssuer issuerAddress issuerSigningKey
              providers' = (issuerAddress, issuerSigningKey) :| filter ((/=) issuerAddress . fst) providers
              mintingAction = CT.BurnAll currencyIssuer providers'
          printStats <- view printStatsL
          era <- view eraL
          (burningTx, _) <- do
            txBuildupCtx <- view txBuildupContextL
            runLabeledCli era so $
              buildMintingImpl
                txBuildupCtx
                mintingAction
                woMetadata
                ccMintingExpirationSlot
                printStats

          -- This is a bit counterintuitive but we have to actually update the initial state
          -- of the wallets if they contained tokens before the test run.
          -- We burn *all* the tokens so we have to drop them from the wallet balance baseline as well.
          updateWallets \wallet@Wallet{_waBalanceCheckBaseline} -> do
            let balanceCheckBaseline' = do
                  let check (cs, _, _) = cs /= ccCurrencySymbol
                  foldMap (uncurry3 P.singleton) . filter check . P.flattenValue $ _waBalanceCheckBaseline
            wallet{_waBalanceCheckBaseline = balanceCheckBaseline'}

          addWalletTransaction issuer so "" burningTx
    Nothing -> logStoreLabeledMsg so $ "Currency " <> show currencyNickname <> " was minted externally, skipping."
interpret wo@Fund{..} =
  fundWallets wo woWalletNicknames woUTxOs
interpret wo@Mint{..} = do
  era <- view eraL

  let issuerNickname = fromMaybe faucetNickname woIssuer

  (Currencies currencies) <- use currenciesL
  case Map.lookup woCurrencyNickname currencies of
    Just Currency{ccIssuer = ci} -> when (ci /= Just issuerNickname) do
      throwError $ testExecutionFailed' "Currency with a given nickname already exist and is minted by someone else."
    Nothing -> pure ()

  ifor_ currencies \currencyNickname Currency{ccIssuer, ccMintingExpirationSlot} -> do
    when
      ( currencyNickname == woCurrencyNickname
          && (ccIssuer /= Just issuerNickname || ccMintingExpirationSlot /= woMintingExpirationSlot)
      )
      $ throwError
      $ testExecutionFailed'
      $ "Currency with the same nickname but different issuer or minting slot already exists. "
        <> " You can re-mint some tokens but you have to use the same issuer and minting slot."

  Wallet issuerAddress _ issuerSigningKey _ _ _ <- getWallet issuerNickname
  tokenDistribution <- forM woTokenDistribution \(TokenAssignment recipient tokens) -> do
    destAddress <- case recipient of
      AddressRecipient bech32 -> do
        case C.deserialiseFromBech32 C.AsShelleyAddress bech32 of
          Left err -> throwError $ testExecutionFailed' $ "Failed to parse address: " <> show err
          Right addr -> pure $ RegularAddressRecipient $ C.shelleyAddressInEra (C.babbageEraOnwardsToShelleyBasedEra era) addr
      WalletRecipient walletNickname -> do
        Wallet addr _ _ _ _ _ <- getWallet walletNickname
        pure $ RegularAddressRecipient addr
      ScriptRecipient (OpenRoleScript (ThreadTokenName bs)) -> do
        let datum = toTxOutDatumInline era $ P.Datum . P.toBuiltinData . stringToBuiltinByteString $ bs
        addr <- openRoleValidatorAddress
        pure $ ScriptAddressRecipient addr datum
    pure (destAddress, Just woMinLovelace, tokens)

  logStoreMsgWith
    ("Mint-Distribution" :: String)
    ("Minting currency " <> coerce woCurrencyNickname <> " with tokens distribution: " <> show woTokenDistribution)
    [("distribution", A.toJSON woTokenDistribution)]
  logStoreLabeledMsg wo $ "The issuer is " <> coerce issuerNickname
  let mintingAction =
        CT.Mint
          (CurrencyIssuer issuerAddress issuerSigningKey)
          tokenDistribution
  printStats <- view printStatsL
  txBuildupCtx <- view txBuildupContextL
  (mintingTx, policy) <-
    runLabeledCli era wo $
      buildMintingImpl
        txBuildupCtx
        mintingAction
        woMetadata
        woMintingExpirationSlot
        printStats

  logStoreMsgWith
    ("Mint-Policy-Id" :: String)
    ("This currency symbol is " <> show policy)
    [("policyId", A.toJSON policy)]
  let currencySymbol = toCurrencySymbol policy
      currency = Currency currencySymbol (Just issuerNickname) woMintingExpirationSlot policy

  addWalletTransaction issuerNickname wo "" mintingTx
  modifying currenciesL \(Currencies currencies') ->
    Currencies $ Map.insert woCurrencyNickname currency currencies'
interpret SplitWallet{..} = do
  Wallet address _ skey _ _ _ :: Wallet era <- getWallet woWalletNickname
  let values = [C.lovelaceToValue v | v <- woUTxOs]
  void $ buildFaucet ("[createCollaterals] " :: String) [address] values address skey
interpret wo@ReturnFunds{} = do
  (era :: BabbageEraOnwards era) <- view eraL
  (allWalletsUtxos :: Map.Map WalletNickname [AUTxO era]) <- fetchWalletsUTxOs
  (utxos, signingKeys) <- ifoldMapMFlipped allWalletsUtxos \n (utxos :: [CT.AUTxO era]) -> do
    if n /= faucetNickname
      then do
        (Wallet{_waSigningKey, _waExternal} :: Wallet era) <- getWallet n
        if _waExternal
          then pure mempty
          else pure (utxos, [_waSigningKey])
      else pure mempty

  Wallet{_waAddress} <- getFaucet
  let total = foldMap CT.aUTxOValue utxos
      inputs = map (fst . CT.unAUTxO) utxos
      outputs = []
      changeAddress = _waAddress

  if total == mempty
    then logStoreLabeledMsg wo "Nothing to refund"
    else do
      (_, tx) <- do
        txBuildupCtx <- view txBuildupContextL
        runLabeledCli era wo $ do
          let queryCtx = toQueryContext txBuildupCtx
          (bodyContent, body) <-
            buildBodyWithContent
              queryCtx
              ([] :: [PayFromScript C.PlutusScriptV1])
              Nothing
              []
              inputs
              outputs
              Nothing
              changeAddress
              Nothing
              []
              C.TxMintNone
              C.TxMetadataNone
              False
              False
              Nothing
          submitTxBody' txBuildupCtx body bodyContent changeAddress signingKeys
      addWalletTransaction faucetNickname wo "" tx
interpret ExternalCurrency{..} = do
  (currencySymbol, policyId) <- case (woCurrencySymbol, woPolicyId) of
    (Just _, Just _) -> throwError $ testExecutionFailed' "You can't specify both currency symbol and policy id."
    (Nothing, Nothing) -> throwError $ testExecutionFailed' "You must specify either currency symbol or policy id."
    (Just (EncodeBase16 bs), _) -> do
      let cs = P.currencySymbol bs

      case toCardanoPolicyId cs of
        Right policy -> pure (cs, policy)
        Left err -> throwError $ testExecutionFailed' $ "Failed to parse currency symbol: " <> show err
    (_, Just _) -> throwError $ testExecutionFailed' "External currency with policy id is not supported yet."

  let currency = Currency currencySymbol Nothing Nothing policyId
  Currencies currencies <- use currenciesL
  case Map.lookup woCurrencyNickname currencies of
    Just Currency{..} -> do
      when (isJust ccIssuer) $
        throwError $
          testExecutionFailed' "Currency with a given nickname already exist and is minted by someone else."
      when (ccCurrencySymbol /= currencySymbol) $
        throwError $
          testExecutionFailed' "Currency with a given nickname already exist and has different currency symbol."
    Nothing ->
      modifying currenciesL \(Currencies currencies') ->
        Currencies $ Map.insert woCurrencyNickname currency currencies'

saveWalletFiles
  :: (C.IsCardanoEra era) => WalletNickname -> Wallet era -> Maybe FilePath -> IO (FilePath, CT.SigningKeyFile)
saveWalletFiles walletNickname wallet dir = do
  let WalletNickname nickname = walletNickname
      Wallet{_waAddress, _waSigningKey = sskey} = wallet
      addrFileTpl = "wallet-of-" <> nickname <> "-" <> ".pay"
      skeyFileTpl = "wallet-of-" <> nickname <> "-" <> ".skey"
      writeEnvelope p = C.writeFileTextEnvelope p Nothing

  (addrFile, skeyFile) <- case dir of
    Nothing -> do
      a <- emptySystemTempFile addrFileTpl
      s <- emptySystemTempFile skeyFileTpl
      pure (a, s)
    Just dir' -> do
      a <- emptyTempFile dir' addrFileTpl
      s <- emptyTempFile dir' skeyFileTpl
      pure (a, s)
  writeFile addrFile (Text.unpack $ C.serialiseAddress _waAddress)
  void case sskey of
    SomePaymentSigningKeyPayment k -> writeEnvelope (File skeyFile) k
    SomePaymentSigningKeyPaymentExtended k -> writeEnvelope (File skeyFile) k
    SomePaymentSigningKeyGenesisUTxO k -> writeEnvelope (File skeyFile) k
  pure (addrFile, CT.SigningKeyFile skeyFile)

createWallet
  :: forall env era st m
   . (InterpretMonad env st m era)
  => BabbageEraOnwards era
  -> m (Wallet era)
createWallet era = do
  skey <- liftIO $ generateSigningKey AsPaymentKey
  networkId <- getNetworkId
  let vkey = C.getVerificationKey skey
      (address :: AddressInEra era) =
        makeShelleyAddressInEra
          (C.babbageEraOnwardsToShelleyBasedEra era)
          networkId
          (PaymentCredentialByKey (verificationKeyHash vkey))
          NoStakeAddress
  pure $ emptyWallet address (SomePaymentSigningKeyPayment skey)

fundWallets
  :: (InterpretMonad env st m era, CAS.IsShelleyBasedEra era)
  => (Label l)
  => l
  -> [WalletNickname]
  -> [Ledger.Coin]
  -> m ()
fundWallets label walletNicknames utxos = do
  let values = [C.lovelaceToValue v | v <- utxos]

  (Wallet faucetAddress _ faucetSigningKey _ _ _ :: Wallet era) <- getFaucet
  addresses <- for walletNicknames \walletNickname -> do
    (Wallet address _ _ _ _ _) <- getWallet walletNickname
    pure address
  era <- view eraL
  txBody <- buildFaucet label addresses values faucetAddress faucetSigningKey
  updateFaucet \faucet@(Wallet _ _ _ faucetTransactions _ _) ->
    faucet{_waSubmittedTransactions = SomeTxBody era txBody : faucetTransactions}

buildFaucet
  :: (InterpretMonad env st m era, CAS.IsShelleyBasedEra era)
  => (Label l)
  => l
  -> [AddressInEra era]
  -> [CAS.Value]
  -> AddressInEra era
  -> CT.SomePaymentSigningKey
  -> m (CAS.TxBody era)
buildFaucet label addresses values faucetAddress faucetSigningKey = do
  era <- view eraL
  txBuildupCtx <- view txBuildupContextL
  runLabeledCli era label $
    buildFaucetImpl
      txBuildupCtx
      (Just values)
      addresses
      faucetAddress
      faucetSigningKey
      defaultCoinSelectionStrategy
