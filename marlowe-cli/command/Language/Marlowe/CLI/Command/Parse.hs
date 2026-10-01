{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--

-----------------------------------------------------------------------------

-- | Parsing Cardano types in command-line options.
module Language.Marlowe.CLI.Command.Parse (
  -- * Parsers
  ScriptFilesOptions (..),
  loadMarloweScripts,
  parseAddress,
  parseAssetId,
  parseByteString,
  parseCurrencySymbol,
  parseInput,
  parseInputContent,
  parseLovelaceValue,
  parseMarloweValuePair,
  parseNetworkId,
  parseOutputQuery,
  parsePOSIXTime,
  parseParty,
  parseProtocolVersion,
  parseRole,
  parseSecond,
  parseSlot,
  parseSlotNo,
  parseStakeAddressReference,
  parseTimeout,
  parseToken,
  parseTokenName,
  parseTxId,
  parseTxIn,
  parseTxIx,
  parseTxOut,
  parseUrl,
  parseValue,
  parseWallet,
  protocolVersionOpt,
  publishingStrategyOpt,
  readAddressEither,
  readByteStringEither,
  readPartyEither,
  readTokenName,
  requiredSignerOpt,
  requiredSignersOpt,
  scriptsFilesOptions,
  timeoutHelpMsg,
  txBodyFileOpt,
  outTxFileOpt,
  walletOpt,
) where

import Cardano.Api
    ( AddressInEra,
      AsType(..),
      AssetId(..),
      NetworkId(..),
      NetworkMagic(..),
      Quantity(..),
      SlotNo(..),
      StakeAddressReference(..),
      TxId(..),
      TxIn(..),
      TxIx(..),
      Value,
      deserialiseAddress,
      deserialiseFromRawBytesHex,
      lovelaceToValue,
      quantityToLovelace,
      StakeAddress(..),
      fromShelleyStakeCredential )
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Language.Marlowe.CLI.Types (
  OutputQuery (..),
  OutputQueryResult,
  PublishingStrategy (PublishAtAddress, PublishPermanently),
  SigningKeyFile (SigningKeyFile),
  SomeTimeout (..),
  TxBodyFile (TxBodyFile),
  TxFile (TxFile),
 )
import Marlowe.Plutus.Semantics.Types (ChoiceId (..), Input (..), InputContent (..), Party (..), Token (..))
import Marlowe.Plutus.Semantics.Types.Address (deserialiseAddressBech32)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Slot (Slot (..))
import PlutusLedgerApi.V1 (BuiltinByteString, CurrencySymbol (..), POSIXTime (..), TokenName (..), toBuiltin)
import Servant.Client (BaseUrl, parseBaseUrl)
import Text.Read (readEither)
import Text.Regex.Posix ((=~))

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Control.Category ((>>>))
import Data.ByteString.Base16 qualified as Base16 (decode)
import Data.ByteString.Char8 qualified as BS8 (pack)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T (pack)
import Data.Time.Units (Second)
import Language.Marlowe.CLI.Scripts (
  MarloweScripts,
  MarloweScriptsPaths (..),
  loadMarloweScriptsPaths,
  readMarloweScriptSuite,
  resolveMarloweScriptsPaths,
 )
import Language.Marlowe.CLI.Types (CliError)
import Marlowe.Plutus.Semantics.Types qualified as M
import Options.Applicative qualified as O
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.Common.Versions (alonzoPV, vasilPV, changPV)
import GHC.IsList (IsList(fromList))

-- | Parser for network ID.
parseNetworkId :: O.Mod O.OptionFields NetworkId -> O.Parser NetworkId
parseNetworkId network =
  parseMainnet <|> parseTestnet
  where
    parseMainnet = O.flag' Mainnet (O.long "mainnet" <> O.help "Execute on mainnet.")
    parseTestnet =
      O.option
        (Testnet . NetworkMagic . toEnum <$> O.auto)
        ( O.long "testnet-magic"
            <> O.metavar "INTEGER"
            <> network
            <> O.help "Network magic. Defaults to the CARDANO_NODE_NETWORK_ID environment variable's value."
        )

-- | Parser for stake address reference.
parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader $
    \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue . fromShelleyStakeCredential $ credential
        Nothing -> Left "Invalid Bech32 stake address."

-- | Parser for slot number.
parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto

-- | Parser for slot number.
parseSlot :: O.ReadM Slot
parseSlot = Slot <$> O.auto

-- | Parser for POSIXTime.
parsePOSIXTime :: O.ReadM POSIXTime
parsePOSIXTime = POSIXTime <$> O.auto

-- | Parser for Timeout.
parseTimeout :: O.ReadM SomeTimeout
parseTimeout = O.eitherReader $ \s -> case s =~ ("^([1-9][[:digit:]]*|0)([s|m|h|d|w]?)$" :: String) of
  [[_, instant, ""]] -> do
    timeout <- readEither instant
    pure $ AbsoluteTimeout (fromInteger timeout)
  [[_, instant, unit]] -> do
    duration <- readEither instant
    pure $ RelativeTimeout $ unitMultiplier unit * fromInteger duration
  result -> Left $ "Invalid timeout value. " <> show result <> timeoutHelpMsg
  where
    unitMultiplier "m" = 60
    unitMultiplier "h" = 60 * 60
    unitMultiplier "d" = 60 * 60 * 24
    unitMultiplier "w" = 60 * 60 * 24 * 7
    unitMultiplier _ = 1

timeoutHelpMsg :: String
timeoutHelpMsg = "POSIX milliseconds or duration: `INTEGER[s|m|d|w|h]`."

-- | Parser for currency symbol.
parseCurrencySymbol :: O.ReadM CurrencySymbol
parseCurrencySymbol = CurrencySymbol <$> parseByteString

-- | Parser for `TxIn`.
parseTxIn :: O.ReadM TxIn
parseTxIn =
  O.eitherReader $
    \s ->
      case s =~ ("^([[:xdigit:]]{64})#([1-9][[:digit:]]*|0)$" :: String) of
        [[_, txId, txIx]] -> do
          txId' <- readTxIdEither txId
          txIx' <- TxIx <$> readEither txIx
          pure $ TxIn txId' txIx'
        _ -> Left "Invalid transaction input."

-- | Parser for `TxId`.
parseTxId :: O.ReadM TxId
parseTxId = O.eitherReader readTxIdEither

-- | Reader for `TxId`.
readTxIdEither
  :: String
  -- ^ The string to be read.
  -> Either String TxId
  -- ^ Either the transaction ID or an error message.
readTxIdEither s =
  case deserialiseFromRawBytesHex (BS8.pack s) of
    Left msg -> Left ("Invalid transaction ID: " <> show msg)
    Right txId -> Right txId

-- | Parser for `TxIx`.
parseTxIx :: O.ReadM TxIx
parseTxIx = TxIx <$> O.auto

-- | Parser for `TxOut` information.
parseTxOut :: C.BabbageEraOnwards era -> O.ReadM (AddressInEra era, Value)
parseTxOut era =
  O.eitherReader $
    \s ->
      case splitOn "+" s of
        address : lovelace' : tokens -> do
          address' <- readAddressEither era address
          lovelace'' <- readLovelaceEither lovelace'
          tokens' <- mapM readAssetValueEither tokens
          pure (address', lovelace'' <> mconcat tokens')
        _ -> Left "Invalid transaction output."

-- | Parser for `Value`.
parseValue :: O.ReadM Value
parseValue =
  O.eitherReader $
    \s ->
      case splitOn "+" s of
        lovelace' : tokens -> do
          lovelace'' <- readLovelaceEither lovelace'
          tokens' <- mapM readAssetValueEither tokens
          pure $ lovelace'' <> mconcat tokens'
        _ -> Left "Invalid transaction output."

-- | Parser for lovelace `Value`.
parseLovelaceValue :: O.ReadM Value
parseLovelaceValue = O.eitherReader readLovelaceEither

-- | Reader for lovelace `Value`.
readLovelaceEither
  :: String
  -- ^ The string to be read.
  -> Either String Value
  -- ^ Either the lovelace value or an error message.
readLovelaceEither =
  fmap (lovelaceToValue . quantityToLovelace . Quantity)
    . readEither

-- | Reader for an asset and its value.
readAssetValueEither
  :: String
  -- ^ The string to be read.
  -> Either String Value
  -- ^ Either the value or an error message.
readAssetValueEither s =
  case break (== ' ') s of
    (amount, ' ' : token) -> do
      token' <- readAssetIdEither $ dropWhile (== ' ') token
      amount' <- Quantity <$> readEither amount
      pure $ fromList [(token', amount')]
    _ -> Left "Invalid asset value."

-- | Parser for `AssetId`.
parseAssetId :: O.ReadM AssetId
parseAssetId = O.eitherReader readAssetIdEither

-- | Reader for `AssetId`.
readAssetIdEither
  :: String
  -- ^ The string to be read.
  -> Either String AssetId
  -- ^ Either the asset ID or an error message.
readAssetIdEither s =
  case s =~ ("^([[:xdigit:]]{56})\\.([^+]+)$" :: String) of
    [[_, symbol, name]] -> case deserialiseFromRawBytesHex $ BS8.pack symbol of
      Right symbol' ->
        Right $
          AssetId
            symbol'
            (C.UnsafeAssetName . BS8.pack $ name)
      Left msg -> Left ("Invalid policy ID: " <> show msg)
    _ -> Left "Invalid token."

-- | Parser for `AddressInEra era`.
parseAddress :: C.BabbageEraOnwards era -> O.ReadM (AddressInEra era)
parseAddress = O.eitherReader . readAddressEither

-- | Parser for `AddressInEra era`.
readAddressEither
  :: forall era
   . C.BabbageEraOnwards era
  -> String
  -- ^ The string to be read.
  -> Either String (AddressInEra era)
  -- ^ Either the address or an error message.
readAddressEither era s = do
  let result = case era of
        C.BabbageEraOnwardsBabbage -> deserialiseAddress (AsAddressInEra AsBabbageEra) $ T.pack s
        C.BabbageEraOnwardsConway -> deserialiseAddress (AsAddressInEra AsConwayEra) $ T.pack s
  case result of
    Nothing -> Left "Invalid address."
    Just address -> Right address

readPartyEither :: String -> Either String Party
readPartyEither str = case readPartyAddressEither str of
  Right party -> Right party
  Left _ -> readPartyRoleEither str

-- | Parser for `Party`.
parseParty :: O.ReadM Party
parseParty = O.eitherReader readPartyEither <|> O.readerError "Invalid party."

-- | Reader for `Party` `Address`.
readPartyAddressEither
  :: String
  -- ^ The string to be read.
  -> Either String Party
  -- ^ Either the address party or an error message.
readPartyAddressEither s =
  case deserialiseAddressBech32 $ T.pack s of
    Just (network, address) -> Right $ Address network address
    _ -> Left "Invalid Bech32 address for party."

-- | Reader for `Party` `Role`.
readPartyRoleEither
  :: String
  -- ^ The string to be read.
  -> Either String Party
  -- ^ Either the role party or an error message.
readPartyRoleEither s =
  if length s <= 32
    then Right . Role . TokenName . toBuiltin $ BS8.pack s
    else Left "Invalid role name for party."

-- | Parser for `Token`.
parseToken :: O.ReadM Token
parseToken = O.eitherReader readTokenEither

-- | Reader for `Token`.
readTokenEither
  :: String
  -- ^ The string to be read.
  -> Either String Token
  -- ^ Either the token or an error message.
readTokenEither s =
  case s =~ ("^([[:xdigit:]]{56})\\.([^+]+)$" :: String) of
    [[_, symbol, name]] -> case Base16.decode $ BS8.pack symbol of
      Right symbol' ->
        Right $
          Token
            (CurrencySymbol . toBuiltin $ symbol')
            (TokenName . toBuiltin . BS8.pack $ name)
      Left message -> Left message
    _ -> Left "Invalid token."

-- | Parser for `TokenName`.
parseTokenName :: O.ReadM TokenName
parseTokenName = O.eitherReader $ Right . readTokenName

-- | Reader for `TokenName`.
readTokenName
  :: String
  -> TokenName
readTokenName = TokenName . toBuiltin . BS8.pack

readByteStringEither :: String -> Either String BuiltinByteString
readByteStringEither s =
  case Base16.decode $ BS8.pack s of
    Left message -> Left message
    Right currency -> Right . toBuiltin $ currency

-- | Parser for `BuiltinByteString`.
parseByteString :: O.ReadM BuiltinByteString
parseByteString =
  O.eitherReader readByteStringEither

-- | Parse input to a contract.
parseInput :: O.Parser Input
parseInput = NormalInput <$> parseInputContent

-- | Parse input to a contract.
parseInputContent :: O.Parser InputContent
parseInputContent =
  parseDeposit <|> parseChoice <|> parseNotify
  where
    parseDeposit =
      IDeposit
        <$> O.option
          parseParty
          ( O.long "deposit-account"
              <> O.metavar "PARTY"
              <> O.help "The account for the deposit."
          )
        <*> O.option
          parseParty
          ( O.long "deposit-party"
              <> O.metavar "PARTY"
              <> O.help "The party making the deposit."
          )
        <*> O.option
          parseToken
          ( O.long "deposit-token"
              <> O.metavar "TOKEN"
              <> O.value (Token adaSymbol adaToken)
              <> O.help "The token being deposited, if not Ada."
          )
        <*> O.option
          O.auto
          ( O.long "deposit-amount"
              <> O.metavar "INTEGER"
              <> O.help "The amount of token being deposited."
          )
    parseChoice =
      IChoice
        <$> ( ChoiceId
                <$> O.strOption (O.long "choice-name" <> O.metavar "NAME" <> O.help "The name of the choice made.")
                <*> O.option parseParty (O.long "choice-party" <> O.metavar "PARTY" <> O.help "The party making the choice.")
            )
        <*> O.option O.auto (O.long "choice-number" <> O.metavar "INTEGER" <> O.help "The number chosen.")
    parseNotify =
      INotify
        <$ O.flag' () (O.long "notify" <> O.help "Notify the contract.")

-- | Parse a URL.
parseUrl :: O.ReadM BaseUrl
parseUrl =
  O.eitherReader $
    either (Left . show) Right
      . parseBaseUrl

-- | Parse a role.
parseRole :: C.BabbageEraOnwards era -> O.ReadM (TokenName, AddressInEra era)
parseRole era =
  O.eitherReader $
    \s ->
      case splitOn "=" s of
        [name, address] -> do
          address' <- readAddressEither era address
          pure (readTokenName name, address')
        _ -> Left "Invalid role assignment."

-- | Parse an address query.
parseOutputQuery :: O.Parser (Maybe (OutputQuery era (OutputQueryResult era)))
parseOutputQuery =
  Just <$> parseLovelaceOnly <|> Just <$> parseAssetOnly <|> parseAllOutput
  where
    parseAllOutput =
      Nothing
        <$ O.flag' () (O.long "all" <> O.help "Report all output.")
    parseLovelaceOnly =
      LovelaceOnly . (<=) . Ledger.Coin
        <$> O.option
          O.auto
          ( O.long "lovelace-only"
              <> O.metavar "LOVELACE"
              <> O.help "The minimum Lovelace that must be the sole asset in the output value."
          )
    parseAssetOnly =
      AssetOnly
        <$> O.option
          parseAssetId
          ( O.long "asset-only"
              <> O.metavar "CURRENCY_SYMBOL.TOKEN_NAME"
              <> O.help "The current symbol and token name for the sole native asset in the value."
          )

parseProtocolVersion :: O.ReadM MajorProtocolVersion
parseProtocolVersion = O.eitherReader \case
  "alonzo" -> pure alonzoPV
  "vasil" -> pure vasilPV
  "chang" -> pure changPV
  s -> Left $ "Invalid protocol version: " <> s <> ". Expecting [alonzo|vasil]."

protocolVersionOpt :: O.Parser MajorProtocolVersion
protocolVersionOpt =
  fromMaybe changPV
    <$> ( O.optional $
            O.option
              parseProtocolVersion
              (O.long "protocol-version" <> O.metavar "PROTOCOL_VERSION" <> O.help "Protocol version: [alonzo|vasil]")
        )

requiredSignerOpt :: O.Parser SigningKeyFile
requiredSignerOpt =
  SigningKeyFile
    <$> O.strOption (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "File containing a required signing key.")

requiredSignersOpt :: O.Parser [SigningKeyFile]
requiredSignersOpt =
  map SigningKeyFile
    <$> (O.some . O.strOption)
      (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "File containing a required signing key.")

parseWallet :: C.BabbageEraOnwards era -> O.ReadM (AddressInEra era, SigningKeyFile)
parseWallet era =
  O.eitherReader $
    splitOn ":" >>> \case
      [address, signingKeyFile] -> do
        address' <- readAddressEither era address
        pure (address', SigningKeyFile signingKeyFile)
      _ -> Left "Expecting address and signing key file path: ADDRESS:SIGNING_FILE"

walletOpt
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields (AddressInEra era, SigningKeyFile)
  -> O.Parser (AddressInEra era, SigningKeyFile)
walletOpt = O.option . parseWallet

txBodyFileOpt :: O.Parser TxBodyFile
txBodyFileOpt =
  TxBodyFile <$> O.strOption (O.long "out-file" <> O.metavar "FILE" <> O.help "Output file for transaction body.")

outTxFileOpt :: O.Parser TxFile
outTxFileOpt =
  TxFile <$> O.strOption (O.long "out-tx-file" <> O.metavar "FILE" <> O.help "The output file containing a transaction.")

publishingStrategyOpt :: forall era. C.BabbageEraOnwards era -> O.Parser (PublishingStrategy era)
publishingStrategyOpt era =
  PublishAtAddress
    <$> O.option
      (parseAddress era)
      ( O.long "at-address"
          <> O.metavar "ADDRESS"
          <> O.help "Publish script at a given address. This is a default strategy which uses change address as a destination."
      )
    <|> PublishPermanently
      <$> O.option
        parseStakeAddressReference
        ( O.long "permanently"
            <> O.metavar "STAKING_ADDRESS"
            <> O.help "Publish permanently at unspendable script address staking the min. ADA value."
        )
    <|> O.flag'
      (PublishPermanently C.NoStakeAddress)
      ( O.long "permanently-without-staking"
          <> O.help "Publish permanently at unspendable script address without min. ADA staking."
      )

parseSecond :: O.ReadM Second
parseSecond = O.auto <|> (fromInteger <$> O.auto)

parseMarloweValuePair :: O.ReadM (Token, Integer)
parseMarloweValuePair = O.eitherReader $ \s ->
  case splitOn "," s of
    [currencySymbol, token, amount] -> do
      (amount' :: Integer) <- readEither amount
      currencySymbol' <- readByteStringEither currencySymbol
      pure (M.Token (CurrencySymbol currencySymbol') (readTokenName token), amount')
    _ -> Left "Invalid deposit format. Expecting: CURRENCY_SYMBOL,TOKEN,AMOUNT"

-- | Options for locating the Marlowe script bundle. Each individual
-- @--*-script-file@ option, when present, overrides the corresponding
-- entry of the @--script-suite-file@ suite.
data ScriptFilesOptions = ScriptFilesOptions
  { scriptSuiteFile :: Maybe FilePath
  -- ^ Path to a 'ScriptSuite' JSON/YAML document (from
  -- 'marlowe-binaries compile suite'). When set, the suite is loaded and
  -- provides default paths to the underlying script files.
  , marloweScriptFile :: Maybe FilePath
  -- ^ Path to the marlowe semantics script. May point to a plutus binary
  -- text envelope or to a JSON document describing one. Overrides the
  -- @script-suite-file@ entry for the marlowe script.
  , payoutScriptFile :: Maybe FilePath
  -- ^ Path to the marlowe role payout validator.
  -- Overrides the @script-suite-file@ entry for the payout script.
  , openRolesScriptFile :: Maybe FilePath
  -- ^ Path to the open roles validator. Overrides the
  -- @script-suite-file@ entry for the open roles script.
  }

-- | Parser that captures the @--script-suite-file@ /
-- @--*-script-file@ option family. All fields are optional; commands
-- that require a specific script should validate the resolved
-- 'ScriptFilesOptions' before proceeding.
scriptsFilesOptions :: O.Parser ScriptFilesOptions
scriptsFilesOptions =
  ScriptFilesOptions
    <$> O.optional
      ( O.strOption
          ( O.long "script-suite-file"
              <> O.metavar "FILE"
              <> O.help
                "Path to a ScriptSuite JSON/YAML document describing the bundle \
                \(marlowe semantics, role payout, open roles) produced by \
                \'marlowe-binaries compile suite\'."
          )
      )
    <*> O.optional
      ( O.strOption
          ( O.long "marlowe-script-file"
              <> O.metavar "FILE"
              <> O.help
                "Path to the marlowe semantics script (plutus text envelope or JSON). \
                \Overrides --script-suite-file."
          )
      )
    <*> O.optional
      ( O.strOption
          ( O.long "payout-script-file"
              <> O.metavar "FILE"
              <> O.help
                "Path to the marlowe role payout script (plutus text envelope or JSON). \
                \Overrides --script-suite-file."
          )
      )
    <*> O.optional
      ( O.strOption
          ( O.long "open-roles-script-file"
              <> O.metavar "FILE"
              <> O.help
                "Path to the open roles validator (plutus text envelope or JSON). \
                \Overrides --script-suite-file."
          )
      )

-- | Load the bundle of Marlowe Plutus scripts using the overrides in
-- 'ScriptFilesOptions'. The @--*-script-file@ overrides win over the
-- entries in the @--script-suite-file@ bundle.
loadMarloweScripts :: ScriptFilesOptions -> IO (Either CliError MarloweScripts)
loadMarloweScripts ScriptFilesOptions{..} = do
  suiteResult <- traverse readMarloweScriptSuite scriptSuiteFile
  case suiteResult of
    Just (Left err) -> pure (Left err)
    Just (Right suite) ->
      loadMarloweScriptsPaths
        . resolveMarloweScriptsPaths (Just suite)
        $ MarloweScriptsPaths
          { marloweScriptPath = marloweScriptFile
          , payoutScriptPath = payoutScriptFile
          , openRolesScriptPath = openRolesScriptFile
          , roleTokensScriptPath = Nothing
          }
    Nothing ->
      loadMarloweScriptsPaths
        MarloweScriptsPaths
          { marloweScriptPath = marloweScriptFile
          , payoutScriptPath = payoutScriptFile
          , openRolesScriptPath = openRolesScriptFile
          , roleTokensScriptPath = Nothing
          }
