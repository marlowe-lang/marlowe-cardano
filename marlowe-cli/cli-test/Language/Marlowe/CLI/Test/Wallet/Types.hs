{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Wallet.Types where

import Cardano.Api (
  AddressInEra,
  BabbageEraOnwards,
  PolicyId,
  TxBody,
  UTxO (UTxO),
 )
import Cardano.Api qualified as C
import Marlowe.Contrib.Foldable (foldMapFlipped)
import Control.Lens (Lens', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  ToJSONKey,
  (.:),
  (.:?),
 )
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (toJSONKeyText)
import Data.Aeson.Types qualified as A
import Data.Fixed qualified as F
import Data.Fixed qualified as Fixed
import Data.Foldable (fold)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Language.Marlowe qualified as M
import Language.Marlowe.CLI.Cardano.Api.Value (toPlutusValue, txOutValueValue)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError)
import Language.Marlowe.CLI.Test.Log qualified as Log
import Language.Marlowe.CLI.Test.Operation.Aeson (
  ConstructorName (ConstructorName),
  NewPropName (NewPropName),
  OldPropName (OldPropName),
  PropName (PropName),
  rewriteProp,
  rewritePropWith,
  rewriteToSingletonObject,
 )
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Types (PrintStats, SomePaymentSigningKey, TxBuildupContext)
import PlutusLedgerApi.V1 (CurrencySymbol, TokenName)
import PlutusLedgerApi.V1 qualified as P
import Text.Read (readMaybe)

import Cardano.Api.Ledger qualified as Ledger
import Control.Lens.Getter (Getter)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text qualified as A
import Data.ByteString.Base16.Aeson (EncodeBase16)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Language.Marlowe.CLI.Test.ExecutionMode qualified as EM
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)

-- | Runtime interaction (submission) works against specific era (Babbage).
-- | On the other hand CLI is more flexible and is able to decode and handle
-- | transactions from different eras.
data SomeTxBody
  = forall era.
    SomeTxBody
      (BabbageEraOnwards era)
      (TxBody era)

overSomeTxBody :: forall a. (forall era. BabbageEraOnwards era -> TxBody era -> a) -> SomeTxBody -> a
overSomeTxBody f (SomeTxBody era txBody) = f era txBody

overSomeTxBody' :: forall a. (forall era. TxBody era -> a) -> SomeTxBody -> a
overSomeTxBody' f (SomeTxBody _era txBody) = f txBody

instance Show SomeTxBody where
  show (SomeTxBody era txBody) = "SomeTxBody (" <> show era <> ")" <> "(" <> show txBody <> ")"

data Wallet era = Wallet
  { _waAddress :: AddressInEra era
  , _waBalanceCheckBaseline :: P.Value
  -- ^ This value should reflect all the assets from the wallet which
  -- were on the chain when we started a particular scenario. Currently
  -- it is only used in the case of the faucet wallet.
  , _waSigningKey :: SomePaymentSigningKey
  , _waSubmittedTransactions :: [SomeTxBody]
  -- ^ We keep track of all the transactions so we can
  -- discard fees from the balance check calculation.
  , _waPublishingCosts :: P.Value
  -- ^ We keep track of script publishing costs so we can
  -- discard them from the balance check calculation.
  , _waExternal :: Bool
  -- ^ We allow loading of external wallets from the file system.
  -- We keep track of them so we don't move funds out of these wallet
  -- at the end of the scenario.
  }
  deriving stock (Generic, Show)

makeLenses ''Wallet

emptyWallet :: AddressInEra era -> SomePaymentSigningKey -> Wallet era
emptyWallet address signingKey = Wallet address mempty signingKey mempty mempty False -- mempty

newtype IsExternalWallet = IsExternalWallet {unExternalWallet :: Bool}

fromUTxO :: AddressInEra era -> SomePaymentSigningKey -> UTxO era -> IsExternalWallet -> Wallet era
fromUTxO address signingKey (UTxO utxo) (IsExternalWallet isExternalWallet) = do
  let total = foldMap (toPlutusValue . txOutValueValue) (Map.elems utxo)
  Wallet address total signingKey mempty mempty isExternalWallet

-- | In many contexts this defaults to the `RoleName` but at some
-- | point we want to also support multiple marlowe contracts scenarios
-- | in a one transaction.
newtype WalletNickname = WalletNickname {unWalletNickname :: String}
  deriving stock (Eq, Ord, Generic, Show)

instance IsString WalletNickname where fromString = WalletNickname

instance ToJSON WalletNickname where
  toJSON = toJSON . unWalletNickname

instance FromJSON WalletNickname where
  parseJSON json = do
    str <- parseJSON json
    pure $ WalletNickname str

instance ToJSONKey WalletNickname where
  toJSONKey = toJSONKeyText $ T.pack . unWalletNickname

-- In the current setup we use really a unique currency
-- per issuer. Should we drop this and identify currencies
-- using `WalletNickname`?
newtype CurrencyNickname = CurrencyNickname {unCurrencyNickname :: String}
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON CurrencyNickname where
  parseJSON json = do
    str <- parseJSON json
    pure $ CurrencyNickname str

instance ToJSON CurrencyNickname where
  toJSON = toJSON . unCurrencyNickname

instance IsString CurrencyNickname where fromString = CurrencyNickname

instance ToJSONKey CurrencyNickname where
  toJSONKey = toJSONKeyText $ T.pack . unCurrencyNickname

-- Currently we mint using native script with minting expiration date.
data Currency = Currency
  { ccCurrencySymbol :: CurrencySymbol
  , ccIssuer :: Maybe WalletNickname
  , ccMintingExpirationSlot :: Maybe C.SlotNo
  , ccPolicyId :: PolicyId
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Currencies = Currencies
  { unCurrencies :: Map CurrencyNickname Currency
  }

data AssetId = AdaAssetId | AssetId CurrencyNickname TokenName
  deriving (Eq, Ord, Show)

parseTokenNameJSON :: Aeson.Value -> A.Parser P.TokenName
parseTokenNameJSON json = do
  parseJSON (A.object [("unTokenName", json)])

tokenNameToJSON :: TokenName -> Aeson.Value
tokenNameToJSON (P.TokenName tokenName) = toJSON tokenName

instance FromJSON AssetId where
  parseJSON = \case
    A.String (T.toLower -> "ada") -> pure AdaAssetId
    A.Array (V.toList -> [currencyNicknameJSON, tokenNameJSON]) -> do
      currencyNickname <- parseJSON currencyNicknameJSON
      tokenName <- parseTokenNameJSON tokenNameJSON
      pure $ AssetId currencyNickname tokenName
    _ -> fail "Expecting an AssetId \"tuple\" or \"ada\"."

instance ToJSON AssetId where
  toJSON AdaAssetId = A.String "ADA"
  toJSON (AssetId currencyNickname tokenName) =
    toJSON [toJSON currencyNickname, tokenNameToJSON tokenName]

data Asset = Asset AssetId Integer
  deriving (Eq, Ord, Show)

instance FromJSON Asset where
  parseJSON = \case
    A.Array (V.toList -> [assetJSON, amountJSON]) -> do
      -- Matches ADA asset
      parseJSON assetJSON >>= \case
        AdaAssetId -> pure ()
        _ -> fail "Expecting ADA asset"
      (MicroValue amount) <- parseJSON amountJSON
      pure $ Asset AdaAssetId (floor $ 1_000_000 * amount)
    A.Array (V.toList -> [assetNameJSON, tokenNameJSON, amountJSON]) -> do
      assetId <- parseJSON $ A.Array $ V.fromList [assetNameJSON, tokenNameJSON]
      amount <- parseJSON amountJSON
      pure $ Asset assetId amount
    _ -> fail "Expecting an assets map encoding."

instance ToJSON Asset where
  toJSON (Asset AdaAssetId amount) =
    toJSON
      [ toJSON ("ADA" :: String)
      , toJSON $ show (fromInteger amount :: F.Micro)
      ]
  toJSON (Asset (AssetId currencyNickname tokenName) amount) =
    toJSON
      [toJSON currencyNickname, toJSON tokenName, toJSON amount]

data Balance value
  = ExactValue value
  | ValueRange value value
  | AnyBalance
  deriving stock (Generic, Eq, Show)

instance Functor Balance where
  fmap f = \case
    ExactValue v -> ExactValue (f v)
    ValueRange v1 v2 -> ValueRange (f v1) (f v2)
    AnyBalance -> AnyBalance

newtype MicroValue = MicroValue {unMicroValue :: Fixed.Micro}
  deriving stock (Eq, Ord, Show)

instance FromJSON MicroValue where
  parseJSON = \case
    A.String str -> do
      let str' = T.replace "_" "" str
      case readMaybe (T.unpack str') of
        Just a -> pure $ MicroValue a
        Nothing -> fail "Unable to parse ADA amount"
    json -> MicroValue <$> parseJSON json

instance ToJSON MicroValue where
  toJSON (MicroValue amount) = toJSON $ show amount

instance (FromJSON value) => FromJSON (Balance value) where
  parseJSON json = do
    let parseValue = \case
          A.String str -> do
            let str' = T.replace "_" "" str
            parseJSON (A.String str')
          n@(A.Number _) -> parseJSON n
          _ -> fail $ "Expecting a number or a string but got:" <> TL.unpack (A.encodeToLazyText json)
    case json of
      A.String (T.toLower -> "*") -> pure AnyBalance
      A.Array (V.toList -> [minJson, maxJson]) -> ValueRange <$> parseValue minJson <*> parseValue maxJson
      _ -> ExactValue <$> parseValue json

type Balance' = Balance Integer

checkBalance :: (Ord value) => Balance value -> value -> Bool
checkBalance expected actual = case expected of
  ExactValue v -> v == actual
  ValueRange minValue maxValue -> minValue <= actual && actual <= maxValue
  AnyBalance -> True

-- | Yaml friendly representation of assets which we use in balance checking.
newtype AssetsBalance = AssetsBalance (Map AssetId (Balance Integer))
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

assetsSingleton :: AssetId -> Balance' -> AssetsBalance
assetsSingleton assetId = AssetsBalance . Map.singleton assetId

lovelaceAssets :: Balance' -> AssetsBalance
lovelaceAssets = assetsSingleton AdaAssetId

instance FromJSON AssetsBalance where
  parseJSON json = do
    (assetsEntries :: [A.Value]) <- parseJSON json
    assets <- for assetsEntries parseAssetEntry
    pure $ fold assets
    where
      parseAssetEntry = \case
        lovelaceJSON@(Aeson.Number _) -> lovelaceAssets <$> parseJSON lovelaceJSON
        loveLaceJSON@(Aeson.String _) -> lovelaceAssets <$> parseJSON loveLaceJSON
        A.Array (V.toList -> [assetJSON, balanceJSON]) -> do
          -- Matches ADA asset
          parseJSON assetJSON >>= \case
            AdaAssetId -> pure ()
            _ -> fail "Expecting ADA asset"
          (amount :: Balance MicroValue) <- parseJSON balanceJSON
          pure $ lovelaceAssets $ fmap (floor . (1_000_000 *) . unMicroValue) amount
        A.Array (V.toList -> [assetNameJSON, tokenNameJSON, balanceJSON]) -> do
          assetId <- parseJSON $ A.Array $ V.fromList [assetNameJSON, tokenNameJSON]
          balance <- parseJSON balanceJSON
          pure $ assetsSingleton assetId balance
        A.Object (KM.toList -> [(assetNameJSON, A.Array (V.toList -> tokenAmountPairsJSON))]) -> do
          let assetId = K.toString assetNameJSON
          m <- fmap Map.fromList $ for tokenAmountPairsJSON $ \case
            A.Array (V.toList -> [tokenNameJSON, amountJSON]) -> do
              tokenName <- parseTokenNameJSON tokenNameJSON
              amount <- parseJSON amountJSON
              pure (AssetId (CurrencyNickname assetId) tokenName, amount)
            _ -> fail "Expecting a token amount pair."
          pure $ AssetsBalance m
        _ -> fail "Expecting an assets map encoding."

instance ToJSON AssetsBalance where
  toJSON (AssetsBalance (Map.toList -> assetsList)) = toJSON $ map assetToJSON assetsList
    where
      assetToJSON (AdaAssetId, ExactValue amount) =
        toJSON
          [ toJSON ("ADA" :: String)
          , toJSON $ show (fromInteger amount :: F.Micro)
          ]
      assetToJSON (AssetId currencyNickname tokenName, ExactValue amount) =
        toJSON
          [toJSON currencyNickname, toJSON tokenName, toJSON amount]
      assetToJSON (AdaAssetId, AnyBalance) =
        toJSON
          [ toJSON ("ADA" :: String)
          , toJSON ("*" :: String)
          ]
      assetToJSON (AssetId currencyNickname tokenName, AnyBalance) =
        toJSON
          [toJSON currencyNickname, toJSON tokenName, toJSON ("*" :: String)]
      assetToJSON (AdaAssetId, ValueRange minValue maxValue) =
        toJSON
          [ toJSON ("ADA" :: String)
          , toJSON [toJSON minValue, toJSON maxValue]
          ]
      assetToJSON (AssetId currencyNickname tokenName, ValueRange minValue maxValue) =
        toJSON
          [toJSON currencyNickname, toJSON tokenName, toJSON [toJSON minValue, toJSON maxValue]]

newtype ThreadTokenName = ThreadTokenName String
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, ToJSON)

newtype TokenRecipientScript
  = OpenRoleScript ThreadTokenName
  deriving stock (Eq, Generic, Show)

instance FromJSON TokenRecipientScript where
  parseJSON json@(A.Object obj) = do
    script <- obj .: "script"
    case script of
      (T.toLower -> "openrole") -> do
        possibleThreadTokenName <- obj .:? "threadTokenName"
        pure $ OpenRoleScript $ fromMaybe (ThreadTokenName "") possibleThreadTokenName
      _ -> fail $ "Expecting a token recipient script: " <> TL.unpack (A.encodeToLazyText json)
  parseJSON _ = fail "Expecting a token recipient script"

-- Either a wallet or a script
data TokenRecipient
  = WalletRecipient WalletNickname
  | AddressRecipient Text
  | ScriptRecipient TokenRecipientScript
  deriving stock (Eq, Generic, Show)

-- We encode token recipient as:
--   * `String` - then it is wallet nickname
--   * `{ script: "openRole" }` - then it is a script
--   * `{ wallet: walletNickname }` - then it is a wallet
instance FromJSON TokenRecipient where
  parseJSON = \case
    walletNicknameJSON@(A.String txt) -> case deserialiseAddressBech32 txt of
      Just _ -> pure $ AddressRecipient txt
      Nothing -> WalletRecipient <$> parseJSON walletNicknameJSON
    json@(A.Object props) | (isJust $ List.find ((==) "script" . fst) (KeyMap.toList props)) -> ScriptRecipient <$> parseJSON json
    A.Object (KeyMap.toList -> [("wallet", walletNicknameJSON)]) -> WalletRecipient <$> parseJSON walletNicknameJSON
    A.Object (KeyMap.toList -> [("address", addressJSON)]) -> AddressRecipient <$> parseJSON addressJSON
    json -> fail $ "Expecting a `TokenRecipient` string or object: " <> TL.unpack (A.encodeToLazyText json)

instance ToJSON TokenRecipient where
  toJSON = \case
    WalletRecipient walletNickname -> toJSON walletNickname
    AddressRecipient address -> toJSON address
    ScriptRecipient (OpenRoleScript possibleName) ->
      A.object [("script", "OpenRole"), ("threadTokenName", toJSON possibleName)]

data TokenAssignment = TokenAssignment
  { taTokenRecipient :: TokenRecipient
  -- ^ Default to the same wallet nickname as a token name.
  , taTokens :: [(TokenName, Natural)]
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON TokenAssignment where
  parseJSON = do
    \case
      A.Array (V.toList -> [recipientJSON, tokenNameJSON, amountJSON]) -> do
        recipient <- parseJSON recipientJSON
        tokenName <- parseTokenNameJSON tokenNameJSON
        amount <- parseJSON amountJSON
        pure $ TokenAssignment recipient [(tokenName, amount)]
      A.Object (sort . KeyMap.toList -> [("recipient", recipientJSON), ("tokens", A.Array tokensJSONs)]) -> do
        recipient <- parseJSON recipientJSON
        tokensVector <- for tokensJSONs \case
          A.Array (V.toList -> [tokenNameJSON, amountJSON]) -> do
            tokenName <- parseTokenNameJSON tokenNameJSON
            amount <- parseJSON amountJSON
            pure (tokenName, amount)
          _ -> fail "Expecting a `TokenAssignment` tuple: `[TokenName, Integer]`."
        pure $ TokenAssignment recipient $ V.toList tokensVector
      _ -> fail "Expecting a `TokenAssignment` tuple: `[WalletNickname | Bech32, TokenName, Integer]`."

instance ToJSON TokenAssignment where
  toJSON (TokenAssignment recipient tokens) =
    A.object
      [ ("recipient", toJSON recipient)
      , ("tokens", A.Array $ V.fromList $ map tokenToJSON tokens)
      ]
    where
      tokenToJSON (tokenName, amount) =
        A.Array $ V.fromList [toJSON tokenName, toJSON amount]

-- Parts of this operation set is implemented in `marlowe-cli`. Should we extract
-- this to a separate package/tool like `cardano-testing-wallet`?
data WalletOperation
  = BurnAll
      { woMetadata :: Maybe Aeson.Object
      }
  | CreateWallet
      { woWalletNickname :: WalletNickname
      , woPossibleUTxOs :: Maybe [Ledger.Coin]
      }
  | CheckBalance
      { woWalletNickname :: WalletNickname
      , woBalance :: AssetsBalance
      -- ^ Expected delta of funds:
      -- * We exclude tx fees from this calculation.
      -- * We DON'T subtract the minimum lovelace amount (attached when tokens are
      --  sent or attached to the contract during the execution).
      -- * In the case of `Faucet` wallet we subtract the baseline
      --  funds so they are not included in this balance.
      }
  | Fund
      { woWalletNicknames :: [WalletNickname]
      , woUTxOs :: [Ledger.Coin]
      }
  | Mint
      { woCurrencyNickname :: CurrencyNickname
      , woIssuer :: Maybe WalletNickname
      -- ^ Fallbacks to faucet
      , woMetadata :: Maybe Aeson.Object
      , woTokenDistribution :: NonEmpty TokenAssignment
      , woMinLovelace :: Ledger.Coin
      , -- We should make this relative
        woMintingExpirationSlot :: Maybe C.SlotNo
      }
  | ExternalCurrency
      { woCurrencyNickname :: CurrencyNickname
      , woCurrencySymbol :: Maybe EncodeBase16
      , woPolicyId :: Maybe EncodeBase16
      }
  | ExternalWallet
      { woWalletNickname :: WalletNickname
      , woSigningKeyFile :: String
      }
  | SplitWallet
      { woWalletNickname :: WalletNickname
      , woUTxOs :: [Ledger.Coin]
      }
  | ReturnFunds
  deriving stock (Eq, Generic, Show)

mint :: CurrencyNickname -> NonEmpty TokenAssignment -> Ledger.Coin -> WalletOperation
mint currencyNickname tokenDistribution minLovelace =
  Mint currencyNickname Nothing Nothing tokenDistribution minLovelace Nothing

rewriteWalletProp :: (Ord k, IsString k) => Map k A.Value -> Bool -> Maybe (Map k A.Value)
rewriteWalletProp props toPlural = case Map.lookup "wallet" props of
  Just (A.String walletNickname) -> do
    let props' = Map.delete "wallet" props
        (key, value) =
          if toPlural
            then ("walletNicknames", toJSON [toJSON walletNickname])
            else ("walletNickname", toJSON walletNickname)
    Just $ Map.insert key value props'
  _ -> Nothing

instance A.FromJSON WalletOperation where
  parseJSON = do
    let preprocess = do
          let rewriteCreateWallet = do
                let constructorName = ConstructorName "CreateWallet"
                    rewriteUTxOs =
                      rewriteProp
                        constructorName
                        (OldPropName "utxos")
                        (NewPropName "possibleUTxOs")
                    rewriteUTxO =
                      rewritePropWith
                        constructorName
                        (OldPropName "utxo")
                        (NewPropName "possibleUTxOs")
                        (toJSON . List.singleton)
                    rewriteNickname =
                      rewriteProp
                        constructorName
                        (OldPropName "nickname")
                        (NewPropName "walletNickname")
                    rewriteToNickname =
                      rewriteToSingletonObject
                        constructorName
                        (PropName "walletNickname")
                rewriteUTxOs <> rewriteUTxO <> rewriteNickname <> rewriteToNickname

              rewriteFund = do
                let constructorName = ConstructorName "Fund"
                    rewriteUTxOs =
                      rewriteProp
                        constructorName
                        (OldPropName "utxos")
                        (NewPropName "uTxOs")
                    rewriteUTxO =
                      rewritePropWith
                        constructorName
                        (OldPropName "utxo")
                        (NewPropName "uTxOs")
                        (toJSON . List.singleton)
                    rewriteWallets =
                      rewriteProp
                        constructorName
                        (OldPropName "wallets")
                        (NewPropName "walletNicknames")
                    rewriteWallet =
                      rewritePropWith
                        constructorName
                        (OldPropName "wallet")
                        (NewPropName "walletNicknames")
                        (toJSON . List.singleton)
                rewriteUTxOs <> rewriteUTxO <> rewriteWallets <> rewriteWallet

              rewriteCheckBalance =
                rewriteProp
                  (ConstructorName "CheckBalance")
                  (OldPropName "wallet")
                  (NewPropName "walletNickname")

              rewriteMint =
                rewriteProp
                  (ConstructorName "Mint")
                  (OldPropName "nickname")
                  (NewPropName "currencyNickname")
          rewriteCreateWallet <> rewriteFund <> rewriteCheckBalance <> rewriteMint
    Operation.parseConstructorBasedJSON "wo" preprocess

instance A.ToJSON WalletOperation where
  toJSON = Operation.toConstructorBasedJSON "wo"

faucetNickname :: WalletNickname
faucetNickname = "Faucet"

data Wallets era = Wallets
  { _wsWallets :: Map WalletNickname (Wallet era)
  , _wsFaucet :: Wallet era
  }

mkWallets :: Wallet era -> Wallets era
mkWallets = Wallets Map.empty

mapWallets :: (Wallet era -> Wallet era) -> Wallets era -> Wallets era
mapWallets f (Wallets wallets faucet) = Wallets (Map.map f wallets) (f faucet)

mapWalletsWithKey :: (WalletNickname -> Wallet era -> Wallet era) -> Wallets era -> Wallets era
mapWalletsWithKey f (Wallets wallets faucet) = Wallets (Map.mapWithKey f wallets) (f faucetNickname faucet)

insertWallet :: WalletNickname -> Wallet era -> Wallets era -> Maybe (Wallets era)
insertWallet walletNickname wallet (Wallets wallets faucet)
  | walletNickname `Map.member` wallets = Nothing
  | walletNickname == faucetNickname = Nothing
  | otherwise = Just $ Wallets (Map.insert walletNickname wallet wallets) faucet

updateWallet :: WalletNickname -> Wallet era -> Wallets era -> Maybe (Wallets era)
updateWallet walletNickname wallet (Wallets wallets faucet)
  | walletNickname `Map.member` wallets = Just $ Wallets (Map.insert walletNickname wallet wallets) faucet
  | walletNickname == faucetNickname = Just $ Wallets wallets wallet
  | otherwise = Nothing

lookupWallet :: WalletNickname -> Wallets era -> Maybe (Wallet era)
lookupWallet walletNickname (Wallets wallets faucet)
  | walletNickname == faucetNickname = Just faucet
  | otherwise = Map.lookup walletNickname wallets

allWalletsMap :: Wallets era -> Map WalletNickname (Wallet era)
allWalletsMap (Wallets wallets faucet) = Map.insert faucetNickname faucet wallets

nonFaucetWalletsMap :: Wallets era -> Map WalletNickname (Wallet era)
nonFaucetWalletsMap (Wallets wallets _) = wallets

faucetWallet :: Wallets era -> Wallet era
faucetWallet (Wallets _ f) = f

makeLenses ''Wallets

class HasInterpretState st era | st -> era where
  walletsL :: Lens' st (Wallets era)
  currenciesL :: Lens' st Currencies

class HasInterpretEnv env era | env -> era where
  eraL :: Lens' env (BabbageEraOnwards era)
  printStatsL :: Lens' env PrintStats
  txBuildupContextL :: Getter env (TxBuildupContext era)

type InterpretMonad env st m era =
  ( MonadState st m
  , HasInterpretState st era
  , MonadReader env m
  , HasInterpretEnv env era
  , EM.HasInterpretEnv env era
  , MonadError InterpreterError m
  , MonadIO m
  , Log.InterpretMonad env st m era
  )

adaToken :: M.Token
adaToken = M.Token "" ""

txBodyFee :: forall era. TxBody era -> Ledger.Coin
txBodyFee (C.TxBody txBodyContent) =
  case C.txFee txBodyContent of
    C.TxFeeExplicit _ lovelace -> lovelace

walletTxFees :: Wallet era' -> Ledger.Coin
walletTxFees Wallet{..} = do
  let
  _waSubmittedTransactions `foldMapFlipped` \case
    (SomeTxBody _ txBody) -> txBodyFee txBody
