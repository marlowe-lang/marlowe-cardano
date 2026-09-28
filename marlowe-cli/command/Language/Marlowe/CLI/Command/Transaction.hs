-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Transaction-related commands in the Marlowe CLI tool.
module Language.Marlowe.CLI.Command.Transaction (
  -- * Marlowe CLI Commands
  TransactionCommand (..),
  findPublishedCommand,
  parseTransactionCommand,
  publishCommand,
  runTransactionCommand,
) where

import Cardano.Api qualified as C
import qualified Data.Text as T
import Cardano.Api (
  AddressInEra,
  BabbageEraOnwards,
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  SlotNo,
  TxId,
  TxIn,
  TxOutDatum (TxOutDatumNone),
  babbageEraOnwardsToShelleyBasedEra,
  shelleyBasedEraConstraints,
 )
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (
  ScriptFilesOptions,
  loadMarloweScripts,
  parseAddress,
  parseNetworkId,
  parseSecond,
  parseSlotNo,
  parseTxIn,
  parseTxOut,
  parseValue,
  publishingStrategyOpt,
  requiredSignerOpt,
  requiredSignersOpt,
  scriptsFilesOptions,
  outTxFileOpt,
 )
import Language.Marlowe.CLI.Transaction (
  buildContinuing,
  buildIncoming,
  buildOutgoing,
  buildSimple,
  submit, buildPublishing, findPublished,
 )
-- (Previously imported: openRolesValidator - removed when embedded scripts were dropped)
import Language.Marlowe.CLI.Types (
  CliEnv,
  CliError (..),
  PublishingStrategy,
  SigningKeyFile,
  TxFile,
  TxBodyFile (TxBodyFile),
  mkNodeTxBuildup, QueryExecutionContext (QueryNode),
 )
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), messageFormatFromText)
import Language.Marlowe.Runtime.Core.ScriptRegistry (ScriptsSuiteName (..))

import Cardano.Api qualified as Api (Value)
import Control.Monad.Reader.Class (MonadReader)
import Data.Time.Units (Second)
import Options.Applicative qualified as O

-- | Marlowe CLI commands and options.
data TransactionCommand era
  = -- | Build a non-Marlowe transaction.
    BuildTransact
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
      , inputs :: [TxIn]
      -- ^ The transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The transaction outputs.
      , change :: AddressInEra era
      -- ^ The change address.
      , metadataFile :: Maybe FilePath
      -- ^ The file containing JSON metadata, if any.
      , txFile :: TxFile
      -- ^ The output file for the transaction body.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract and transaction.
      , invalid :: Bool
      -- ^ Assertion that the transaction is invalid.
      }
  | -- | Build a transaction paying into a Marlowe contract.
    BuildCreate
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , scriptAddress :: AddressInEra era
      -- ^ The script address.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
      , outputDatumFile :: FilePath
      -- ^ The file containing the datum for the payment to the script.
      , outputValue :: Api.Value
      -- ^ The value to be paid to the script.
      , inputs :: [TxIn]
      -- ^ The transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The transaction outputs.
      , change :: AddressInEra era
      -- ^ The change address.
      , metadataFile :: Maybe FilePath
      -- ^ The file containing JSON metadata, if any.
      , txFile :: TxFile
      -- ^ The output file for the transaction body.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract and transaction.
      , invalid :: Bool
      -- ^ Assertion that the transaction is invalid.
      }
  | -- | Build a transaction that spends from and pays to a Marlowe contract.
    BuildAdvance
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , scriptAddress :: AddressInEra era
      -- ^ The script address.
      , validatorFile :: FilePath
      -- ^ The file containing the script validator.
      , redeemerFile :: FilePath
      -- ^ The file containing the redeemer.
      , inputDatumFile :: FilePath
      -- ^ The file containing the datum for spending from the script.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
      , inputTxIn :: TxIn
      -- ^ The script eUTxO to be spent.
      , outputDatumFile :: FilePath
      -- ^ The file containing the datum for the payment to the script.
      , outputValue :: Api.Value
      -- ^ The value to be paid to the script.
      , inputs :: [TxIn]
      -- ^ The transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The transaction outputs.
      , collateral :: TxIn
      -- ^ The collateral.
      , change :: AddressInEra era
      -- ^ The change address.
      , minimumSlot :: SlotNo
      -- ^ The first valid slot for the transaction.
      , maximumSlot :: SlotNo
      -- ^ The last valid slot for the transaction.
      , metadataFile :: Maybe FilePath
      -- ^ The file containing JSON metadata, if any.
      , txFile :: TxFile
      -- ^ The output file for the transaction body.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract and transaction.
      , invalid :: Bool
      -- ^ Assertion that the transaction is invalid.
      }
  | -- | Build a transaction spending from a Marlowe contract.
    BuildClose
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , validatorFile :: FilePath
      -- ^ The file containing the script validator.
      , redeemerFile :: FilePath
      -- ^ The file containing the redeemer.
      , inputDatumFile :: FilePath
      -- ^ The file containing the datum for spending from the script.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
      , inputTxIn :: TxIn
      -- ^ The script eUTxO to be spent.
      , inputs :: [TxIn]
      -- ^ The transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The transaction outputs.
      , collateral :: TxIn
      -- ^ The collateral.
      , change :: AddressInEra era
      -- ^ The change address.
      , minimumSlot :: SlotNo
      -- ^ The first valid slot for the transaction.
      , maximumSlot :: SlotNo
      -- ^ The last valid slot for the transaction.
      , metadataFile :: Maybe FilePath
      -- ^ The file containing JSON metadata, if any.
      , txFile :: TxFile
      -- ^ The output file for the transaction body.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract and transaction.
      , invalid :: Bool
      -- ^ Assertion that the transaction is invalid.
      }
  | -- | Submit a transaction.
    Submit
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , txBodyFile :: TxBodyFile
      -- ^ The JSON file containing the transaction body.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The signing key files.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      }
  | Publish
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , signingKeyFile :: SigningKeyFile
      -- ^ The files containing the required signing keys.
      , change :: AddressInEra era
      -- ^ The change address.
      , strategy :: Maybe (PublishingStrategy era)
      , txFile :: TxFile
      -- ^ The output file for the transaction body.
      , submitTimeout :: Maybe Second
      -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
      , expires :: Maybe SlotNo
      , messageFormat :: MessageFormat
      -- ^ The format for messages printed by this command to the stdout.
      , scriptFiles :: ScriptFilesOptions
      -- ^ Paths to Marlowe Plutus scripts.
      , releaseName :: ScriptsSuiteName
      -- ^ The release name under which the registry should record the bundle.
      , description :: Maybe T.Text
      -- ^ Optional human-readable description of the release.
      }
  | FindPublished
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , strategy :: Maybe (PublishingStrategy era)
      , scriptFiles :: ScriptFilesOptions
      -- ^ Paths to Marlowe Plutus scripts.
      }

-- | Run a transaction-related command.
runTransactionCommand
  :: forall era m
   . (MonadError CliError m)
  => (MonadIO m)
  => (MonadReader (CliEnv era) m)
  => BabbageEraOnwards era
  -> TransactionCommand era
  -- ^ The command.
  -> m ()
  -- ^ Action for running the command.
runTransactionCommand era command =
  do
    let network' = network command
        connection =
          LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
            , localNodeNetworkId = network'
            , localNodeSocketPath = File $ socketPath command
            }
        printTxId :: TxId -> m ()
        printTxId = liftIO . putStrLn . ("TxId " <>) . show
        padTxOut (address, value) = (address, TxOutDatumNone, value)
        outputs' :: [(AddressInEra era, C.TxOutDatum C.CtxTx era, Api.Value)]
        outputs' = padTxOut <$> outputs command
    shelleyBasedEraConstraints (babbageEraOnwardsToShelleyBasedEra era) case command of
      BuildTransact{..} ->
        buildSimple
          (mkNodeTxBuildup connection submitTimeout)
          signingKeyFiles
          inputs
          outputs'
          change
          metadataFile
          txFile
          printStats
          invalid
          >>= printTxId
      BuildCreate{..} ->
        buildIncoming
          connection
          scriptAddress
          signingKeyFiles
          outputDatumFile
          outputValue
          inputs
          outputs'
          change
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      BuildAdvance{..} ->
        buildContinuing
          connection
          scriptAddress
          validatorFile
          redeemerFile
          inputDatumFile
          signingKeyFiles
          inputTxIn
          outputDatumFile
          outputValue
          inputs
          outputs'
          collateral
          change
          minimumSlot
          maximumSlot
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      BuildClose{..} ->
        buildOutgoing
          connection
          validatorFile
          redeemerFile
          inputDatumFile
          signingKeyFiles
          inputTxIn
          inputs
          outputs'
          collateral
          change
          minimumSlot
          maximumSlot
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      Submit{..} ->
        submit
          connection
          txBodyFile
          signingKeyFiles
          (fromMaybe 0 submitTimeout)
          >>= printTxId
      Publish{..} -> do
        loadedScripts <- liftIO $ loadMarloweScripts scriptFiles
        case loadedScripts of
          Left err -> throwError err
          Right (marloweV, payoutV, openRolesV, _) ->
            void
              $ buildPublishing @_
                marloweV
                payoutV
                openRolesV
                releaseName
                description
                connection
                signingKeyFile
                expires
                change
                strategy
                txFile
                submitTimeout
                messageFormat
      FindPublished{..} -> do
        loadedScripts <- liftIO $ loadMarloweScripts scriptFiles
        case loadedScripts of
          Left err -> throwError err
          Right (marloweV, payoutV, openRolesV, _) ->
            findPublished @_
              marloweV
              payoutV
              openRolesV
              (QueryNode connection)
              strategy

messageFormatParser :: O.Parser MessageFormat
messageFormatParser =
  O.option (O.eitherReader (Right . messageFormatFromText))
    ( O.long "message-format"
        <> O.metavar "text|json|yaml"
        <> O.value MessageFormatText
        <> O.showDefault
        <> O.help "Format of command output."
    )

-- | Parser for the @--release-name@ flag used by the @publish@ command.
releaseNameOpt :: O.Parser ScriptsSuiteName
releaseNameOpt =
  O.option
    (O.eitherReader (Right . ScriptsSuiteName . T.pack))
    ( O.long "release-name"
        <> O.metavar "NAME"
        <> O.help
          "Release name under which the registry should record the published \
          \Marlowe script bundle. Used as the 'currentRelease' entry in the\
          \ emitted 'ScriptRegistry' document."
    )

-- | Parser for the @--description@ flag. The free-form string is stored
-- in the 'MarloweScripts.description' field of the emitted registry.
descriptionOpt :: O.Parser (Maybe T.Text)
descriptionOpt =
  O.optional $
    T.pack
      <$> O.strOption
        ( O.long "description"
            <> O.metavar "TEXT"
            <> O.help
              "Optional human-readable description for the released scripts."
        )

-- | Parser for transaction-related commands.
parseTransactionCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
parseTransactionCommand era network socket =
  O.hsubparser $
    O.commandGroup "Low-level commands for creating and submitting transactions:"
      <> buildContinuingCommand era network socket
      <> buildOutgoingCommand era network socket
      <> buildIncomingCommand era network socket
      <> buildSimpleCommand era network socket
      <> findPublishedCommand era network socket
      <> publishCommand era network socket
      <> submitCommand network socket

-- | Parser for the "simple" command.
buildSimpleCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
buildSimpleCommand era network socket =
  O.command "simple" $
    O.info (buildSimpleOptions era network socket) $
      O.progDesc "Build a non-Marlowe transaction."

-- | Parser for the "simple" options.
buildSimpleOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
buildSimpleOptions era network socket =
  BuildTransact
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> requiredSignersOpt
    <*> (O.some . O.option parseTxIn)
      ( O.long "tx-in" <> O.metavar "TXID#TXIX" <> O.help "Transaction input in TxId#TxIx format."
      )
    <*> (O.many . O.option (parseTxOut era))
      ( O.long "tx-out" <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address" <> O.metavar "ADDRESS" <> O.help "Address to receive ADA in excess of fee."
      )
    <*> (O.optional . O.strOption)
      ( O.long "metadata-file" <> O.metavar "METADATA_FILE" <> O.help "JSON file containing metadata."
      )
    <*> outTxFileOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "submit"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> O.switch
      ( O.long "script-invalid" <> O.help "Assert that the transaction is invalid."
      )

-- | Parser for the "create" command.
buildIncomingCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
buildIncomingCommand era network socket =
  O.command "create" $
    O.info (buildIncomingOptions era network socket) $
      O.progDesc "Build a transaction that pays to a Marlowe script."

-- | Parser for the "create" options.
buildIncomingOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
buildIncomingOptions era network socket =
  BuildCreate
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "script-address" <> O.metavar "ADDRESS" <> O.help "Address of the Marlowe contract."
      )
    <*> requiredSignersOpt
    <*> O.strOption
      ( O.long "tx-out-datum-file"
          <> O.metavar "DATUM_FILE"
          <> O.help "Datum JSON file datum paid to Marlowe contract."
      )
    <*> O.option
      parseValue
      ( O.long "tx-out-marlowe" <> O.metavar "VALUE" <> O.help "Value paid to Marlowe contract."
      )
    <*> (O.some . O.option parseTxIn)
      ( O.long "tx-in" <> O.metavar "TXID#TXIX" <> O.help "Transaction input in TxId#TxIx format."
      )
    <*> (O.many . O.option (parseTxOut era))
      ( O.long "tx-out"
          <> O.metavar "ADDRESS+VALUE"
          <> O.help "Transaction output in ADDRESS+VALUE format."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address" <> O.metavar "ADDRESS" <> O.help "Address to receive ADA in excess of fee."
      )
    <*> (O.optional . O.strOption)
      ( O.long "metadata-file" <> O.metavar "METADATA_FILE" <> O.help "JSON file containing metadata."
      )
    <*> outTxFileOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "submit"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> O.switch
      ( O.long "script-invalid" <> O.help "Assert that the transaction is invalid."
      )

-- | Parser for the "advance" command.
buildContinuingCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
buildContinuingCommand era network socket =
  O.command "advance" $
    O.info (buildContinuingOptions era network socket) $
      O.progDesc "Build a transaction that both spends from and pays to a Marlowe script."

-- | Parser for the "advance" options.
buildContinuingOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
buildContinuingOptions era network socket =
  BuildAdvance
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "script-address" <> O.metavar "ADDRESS" <> O.help "Address of the Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-in-script-file" <> O.metavar "PLUTUS_FILE" <> O.help "Plutus file for Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-in-redeemer-file"
          <> O.metavar "REDEEMER_FILE"
          <> O.help "Redeemer JSON file spent from Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-in-datum-file"
          <> O.metavar "DATUM_FILE"
          <> O.help "Datum JSON file spent from Marlowe contract."
      )
    <*> requiredSignersOpt
    <*> O.option
      parseTxIn
      ( O.long "tx-in-marlowe" <> O.metavar "TXID#TXIX" <> O.help "UTxO spent from Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-out-datum-file"
          <> O.metavar "DATUM_FILE"
          <> O.help "Datum JSON file datum paid to Marlowe contract."
      )
    <*> O.option
      parseValue
      ( O.long "tx-out-marlowe" <> O.metavar "VALUE" <> O.help "Value paid to Marlowe contract."
      )
    <*> (O.some . O.option parseTxIn)
      ( O.long "tx-in" <> O.metavar "TXID#TXIX" <> O.help "Transaction input in TxId#TxIx format."
      )
    <*> (O.many . O.option (parseTxOut era))
      ( O.long "tx-out"
          <> O.metavar "ADDRESS+VALUE"
          <> O.help "Transaction output in ADDRESS+VALUE format."
      )
    <*> O.option
      parseTxIn
      ( O.long "tx-in-collateral" <> O.metavar "TXID#TXIX" <> O.help "Collateral for transaction."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address"
          <> O.metavar "ADDRESS"
          <> O.help "Address to receive ADA in excess of fee."
      )
    <*> O.option
      parseSlotNo
      ( O.long "invalid-before" <> O.metavar "SLOT" <> O.help "Minimum slot for the redemption."
      )
    <*> O.option
      parseSlotNo
      ( O.long "invalid-hereafter" <> O.metavar "SLOT" <> O.help "Maximum slot for the redemption."
      )
    <*> (O.optional . O.strOption)
      ( O.long "metadata-file" <> O.metavar "METADATA_FILE" <> O.help "JSON file containing metadata."
      )
    <*> outTxFileOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "submit"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> O.switch
      ( O.long "script-invalid" <> O.help "Assert that the transaction is invalid."
      )

-- | Parser for the "close" command.
buildOutgoingCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
buildOutgoingCommand era network socket =
  O.command "close" $
    O.info (buildOutgoingOptions era network socket) $
      O.progDesc "Build a transaction that spends from a Marlowe script."

-- | Parser for the "close" options.
buildOutgoingOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
buildOutgoingOptions era network socket =
  BuildClose
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.strOption
      ( O.long "tx-in-script-file" <> O.metavar "PLUTUS_FILE" <> O.help "Plutus file for Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-in-redeemer-file"
          <> O.metavar "REDEEMER_FILE"
          <> O.help "Redeemer JSON file spent from Marlowe contract."
      )
    <*> O.strOption
      ( O.long "tx-in-datum-file"
          <> O.metavar "DATUM_FILE"
          <> O.help "Datum JSON file spent from Marlowe contract."
      )
    <*> requiredSignersOpt
    <*> O.option
      parseTxIn
      ( O.long "tx-in-marlowe" <> O.metavar "TXID#TXIX" <> O.help "UTxO spent from Marlowe contract."
      )
    <*> (O.some . O.option parseTxIn)
      ( O.long "tx-in" <> O.metavar "TXID#TXIX" <> O.help "Transaction input in TxId#TxIx format."
      )
    <*> (O.many . O.option (parseTxOut era))
      ( O.long "tx-out"
          <> O.metavar "ADDRESS+VALUE"
          <> O.help "Transaction output in ADDRESS+VALUE format."
      )
    <*> O.option
      parseTxIn
      ( O.long "tx-in-collateral" <> O.metavar "TXID#TXIX" <> O.help "Collateral for transaction."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address"
          <> O.metavar "ADDRESS"
          <> O.help "Address to receive ADA in excess of fee."
      )
    <*> O.option
      parseSlotNo
      ( O.long "invalid-before" <> O.metavar "SLOT" <> O.help "Minimum slot for the redemption."
      )
    <*> O.option
      parseSlotNo
      ( O.long "invalid-hereafter" <> O.metavar "SLOT" <> O.help "Maximum slot for the redemption."
      )
    <*> (O.optional . O.strOption)
      ( O.long "metadata-file" <> O.metavar "METADATA_FILE" <> O.help "JSON file containing metadata."
      )
    <*> outTxFileOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "submit"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> O.switch
      ( O.long "script-invalid" <> O.help "Assert that the transaction is invalid."
      )

-- | Parser for the "submit" command.
submitCommand
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
submitCommand network socket =
  O.command "submit" $
    O.info (submitOptions network socket) $
      O.progDesc "Submit a transaction body."

-- | Parser for the "submit" options.
submitOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
submitOptions network socket =
  Submit
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> ( TxBodyFile
            <$> O.strOption
              ( O.long "tx-body-file" <> O.metavar "BODY_FILE" <> O.help "File containing the transaction body."
              )
        )
    <*> requiredSignersOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "timeout"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )

-- | Parser for the "publish" command.
publishCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
publishCommand era network socket =
  O.command "publish" $
    O.info (publishOptions era network socket) $
      O.progDesc "Publish Marlowe validator and role validator on the chain."

-- | Parser for the "publish" options.
publishOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
publishOptions era network socket =
  Publish
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> socket
          <> O.metavar "SOCKET_FILE"
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> requiredSignerOpt
    <*> O.option
      (parseAddress era)
      ( O.long "change-address"
          <> O.metavar "ADDRESS"
          <> O.help "Address to receive ADA in excess of fee."
      )
    <*> O.optional (publishingStrategyOpt era)
    <*> outTxFileOpt
    <*> (O.optional . O.option parseSecond)
      ( O.long "submit"
          <> O.metavar "SECONDS"
          <> O.help "Also submit the transaction, and wait for confirmation."
      )
    <*> (O.optional . O.option parseSlotNo)
      ( O.long "expires"
          <> O.metavar "SLOT_NO"
          <> O.help "The slot number after which minting is no longer possible."
      )
    <*> messageFormatParser
    <*> scriptsFilesOptions
    <*> releaseNameOpt
    <*> descriptionOpt

-- | Parser for the "find-publish" command.
findPublishedCommand
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (TransactionCommand era)
findPublishedCommand era network socket =
  O.command "find-published" $
    O.info (findPublishedOptions era network socket) $
      O.progDesc "Publish Marlowe validator and role validator on the chain."

-- | Parser for the "find-publish" options.
findPublishedOptions
  :: BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (TransactionCommand era)
findPublishedOptions era network socket =
  FindPublished
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> socket
          <> O.metavar "SOCKET_FILE"
           <> O.help
             "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
       )
     <*> O.optional (publishingStrategyOpt era)
     <*> scriptsFilesOptions

