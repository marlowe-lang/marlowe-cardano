-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Contract-related commands in the Marlowe CLI tool.
module Language.Marlowe.CLI.Command.Run (
  -- * Marlowe CLI Commands
  RunCommand (..),
  parseRunCommand,
  runRunCommand,
) where

import Cardano.Api (
  AddressInEra,
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (..),
  File (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  StakeAddressReference (..),
  TxIn,
 )
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Analyze (analyze)
import Language.Marlowe.CLI.Command.Parse (
  ScriptFilesOptions,
  loadMarloweScripts,
  parseAddress,
  parseCurrencySymbol,
  parseInput,
  parseNetworkId,
  parsePOSIXTime,
  parseSecond,
  parseStakeAddressReference,
  parseTokenName,
  parseTxIn,
  parseTxOut,
  publishingStrategyOpt,
  requiredSignersOpt,
  scriptsFilesOptions,
  outTxFileOpt,
 )
import Language.Marlowe.CLI.Run (
  autoRunTransaction,
  autoWithdrawFunds,
  initializeTransaction,
  prepareTransaction,
  runTransaction,
  withdrawFunds,
 )
import Language.Marlowe.CLI.Scripts (mkMarloweScriptsInfo)
import Language.Marlowe.CLI.Transaction (querySlotConfig)
import Language.Marlowe.CLI.Types (
  CliEnv,
  CliError (..),
  PrintStats (PrintStats),
  PublishingStrategy,
  SigningKeyFile,
  TxFile,
 )
import Marlowe.Plutus.Semantics (MarloweParams (MarloweParams))
import Marlowe.Plutus.Semantics.Types (Input)
import PlutusLedgerApi.V1 (CurrencySymbol, POSIXTime (..), TokenName, adaSymbol)
import Cardano.Api qualified as Api (Value)
import Cardano.Api qualified as C
import Control.Monad.Reader (MonadReader)
import Data.Time.Units (Second)
import Options.Applicative qualified as O

defaultMarloweParams :: MarloweParams
defaultMarloweParams = MarloweParams adaSymbol

marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rc = MarloweParams rc

-- | Marlowe CLI commands and options for running contracts.
data RunCommand era
  = -- | Initialize a Marlowe transaction.
    Initialize
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , stake :: Maybe StakeAddressReference
      -- ^ The stake address, if any.
      , rolesCurrency :: Maybe CurrencySymbol
      -- ^ The role currency symbols, if any.
      , contractFile :: FilePath
      -- ^ The JSON file containing the contract.
      , stateFile :: FilePath
      -- ^ The JSON file containing the contract's state.
      , strategy :: Maybe (PublishingStrategy era)
      , outputFile :: Maybe FilePath
      -- ^ The output JSON file for the validator information.
      , merkleize :: Bool
      -- ^ Whether to deeply merkleize the contract.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract.
      , scriptFiles :: ScriptFilesOptions
      -- ^ Paths to Marlowe Plutus scripts.
      }
  | -- | Prepare a Marlowe transaction for execution.
    Prepare
      { marloweInFile :: FilePath
      -- ^ The JSON file with Marlowe initial state and initial contract.
      , inputs' :: [Input]
      -- ^ The contract's inputs.
      , minimumTime :: POSIXTime
      -- ^ The first valid time for the transaction.
      , maximumTime :: POSIXTime
      -- ^ The last valid time for the transaction.
      , outputFile :: Maybe FilePath
      -- ^ The output JSON file with the results of the computation.
      , printStats :: Bool
      -- ^ Whether to print statistics about the redeemer.
      }
  | -- | Run a Marlowe transaction.
    Run
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , marloweIn :: Maybe (FilePath, TxIn, TxIn)
      -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spend and the collateral, unless the transaction opens the contract.
      , marloweOut :: FilePath
      -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
      , inputs :: [TxIn]
      -- ^ The ordinary transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The ordinary transaction outputs.
      , change :: AddressInEra era
      -- ^ The change address.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
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
  | -- | Withdraw funds from a role address.
    Withdraw
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , marloweOut :: FilePath
      -- ^ The JSON file with Marlowe state and contract.
      , roleName :: TokenName
      -- ^ The role name for the redemption.
      , collateral :: TxIn
      -- ^ The collateral.
      , inputs :: [TxIn]
      -- ^ The ordinary transaction inputs.
      , outputs :: [(AddressInEra era, Api.Value)]
      -- ^ The ordinary transaction outputs.
      , change :: AddressInEra era
      -- ^ The change address.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
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
  | -- | Run a Marlowe transaction, without specifying inputs and outputs.
    AutoRun
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , marloweIn' :: Maybe (FilePath, TxIn)
      -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spend and the collateral, unless the transaction opens the contract.
      , marloweOut :: FilePath
      -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
      , change :: AddressInEra era
      -- ^ The change address.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
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
  | -- | Withdraw funds from a role address, without specifying inputs and outputs.
    AutoWithdraw
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , marloweOut :: FilePath
      -- ^ The JSON file with Marlowe state and contract.
      , roleName :: TokenName
      -- ^ The role name for the redemption.
      , change :: AddressInEra era
      -- ^ The change address.
      , signingKeyFiles :: [SigningKeyFile]
      -- ^ The files containing the required signing keys.
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
  | -- | Analyze a contract.
    Analyze
      { network :: NetworkId
      -- ^ The network ID, if any.
      , socketPath :: FilePath
      -- ^ The path to the node socket.
      , marloweOut :: FilePath
      -- ^ The JSON file with Marlowe state and contract.
      , preconditions :: Bool
      -- ^ Whether to check preconditions for Marlowe state.
      , roles :: Bool
      -- ^ Whether to check lengths of role names.
      , tokens :: Bool
      -- ^ Whether to check lengths of token names.
      , maximumValue :: Bool
      -- ^ Whether to check the `maxValueSize` protocol limit.
      , minimumUtxo :: Bool
      -- ^ Whether to check the `utxoCostPerWord` protocol limit.
      , executionCost :: Bool
      -- ^ Whether to check the `maxTxExecutionUnits` protocol limits.
      , transactionSize :: Bool
      -- ^ Whether to check the `maxTxSize` protocol limits.
      , best :: Bool
      -- ^ Whether to compute tight estimates of worst-case bounds.
      , verbose :: Bool
      -- ^ Whether to include worst-case example in output.
      }

-- | Run a contract-related command.
runRunCommand
  :: (MonadError CliError m, MonadReader (CliEnv era) m)
  => (MonadIO m)
  => (C.IsShelleyBasedEra era)
  => RunCommand era
  -- ^ The command.
  -> m ()
  -- ^ Action for running the command.
runRunCommand command =
  do
    let network' = network command
        marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
        stake' = fromMaybe NoStakeAddress $ stake command
        printTxId = liftIO . putStrLn . ("TxId " <>) . show
        padTxOut (address, value) = (address, C.TxOutDatumNone, value)
        outputs' = padTxOut <$> outputs command
    case command of
      Initialize{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        slotConfig <- querySlotConfig connection
        loadedScripts <- liftIO $ loadMarloweScripts scriptFiles
        case loadedScripts of
          Left err -> throwError err
          Right scripts ->
            initializeTransaction
              (mkMarloweScriptsInfo network' stake' scripts)
              marloweParams'
              slotConfig
              stake'
              contractFile
              stateFile
              outputFile
              merkleize
              printStats
      Prepare{..} ->
        prepareTransaction
          marloweInFile
          inputs'
          minimumTime
          maximumTime
          outputFile
          printStats
      Run{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        runTransaction
          connection
          marloweIn
          marloweOut
          inputs
          outputs'
          change
          signingKeyFiles
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      Withdraw{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        withdrawFunds
          connection
          marloweOut
          roleName
          collateral
          inputs
          outputs'
          change
          signingKeyFiles
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      AutoRun{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        autoRunTransaction
          connection
          marloweIn'
          marloweOut
          change
          signingKeyFiles
          metadataFile
          txFile
          submitTimeout
          printStats
          invalid
          >>= printTxId
      AutoWithdraw{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        autoWithdrawFunds
          connection
          marloweOut
          roleName
          change
          signingKeyFiles
          metadataFile
          txFile
          submitTimeout
          (PrintStats printStats)
          invalid
          >>= printTxId
      Analyze{..} -> do
        let connection =
              LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                , localNodeNetworkId = network'
                , localNodeSocketPath = File socketPath
                }
        analyze
          connection
          marloweOut
          preconditions
          roles
          tokens
          maximumValue
          minimumUtxo
          executionCost
          transactionSize
          best
          verbose

-- | Parser for contract commands.
parseRunCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
parseRunCommand era network socket =
  asum
    [ O.hsubparser $
        O.commandGroup "Commands for running contracts:"
          <> runCommand era network socket
          <> initializeCommand era network socket
          <> prepareCommand
          <> withdrawCommand era network socket
          <> analyzeCommand network socket
    , O.hsubparser $
        O.commandGroup "Experimental commands for running contracts, with automatic balancing."
          <> autoRunCommand era network socket
          <> autoWithdrawCommand era network socket
    ]

-- | Parser for the "initialize" command.
initializeCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
initializeCommand era network socket =
  O.command "initialize"
    . O.info (initializeOptions era network socket)
    $ O.progDesc "Initialize the first transaction of a Marlowe contract and write output to a JSON file."

-- | Parser for the "initialize" options.
initializeOptions
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
initializeOptions era network socket =
  Initialize
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> (O.optional . O.option parseStakeAddressReference)
      ( O.long "stake-address" <> O.metavar "ADDRESS" <> O.help "Stake address, if any."
      )
    <*> (O.optional . O.option parseCurrencySymbol)
      ( O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."
      )
    <*> O.strOption
      ( O.long "contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."
      )
    <*> O.strOption
      ( O.long "state-file" <> O.metavar "STATE_FILE" <> O.help "JSON input file for the contract state."
      )
    <*> O.optional (publishingStrategyOpt era)
    <*> (O.optional . O.strOption)
      ( O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for initialize."
      )
    <*> O.switch
      ( O.long "merkleize" <> O.help "Whether to deeply merkleize the contract."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> scriptsFilesOptions

-- | Parser for the "prepare" command.
prepareCommand :: O.Mod O.CommandFields (RunCommand era)
prepareCommand =
  O.command "prepare" $
    O.info prepareOptions $
      O.progDesc "Prepare the next step of a Marlowe contract and write the output to a JSON file."

-- | Parser for the "prepare" options.
prepareOptions :: O.Parser (RunCommand era)
prepareOptions =
  Prepare
    <$> O.strOption
      ( O.long "marlowe-file" <> O.metavar "MARLOWE_FILE" <> O.help "JSON input file for the Marlowe state and contract."
      )
    <*> O.many parseInput
    <*> O.option
      parsePOSIXTime
      ( O.long "invalid-before" <> O.metavar "POSIX_TIME" <> O.help "Minimum time for the input, in POSIX milliseconds."
      )
    <*> O.option
      parsePOSIXTime
      ( O.long "invalid-hereafter" <> O.metavar "POSIX_TIME" <> O.help "Maximum time for the input, in POSIX milliseconds."
      )
    <*> (O.optional . O.strOption)
      ( O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )

-- | Parser for the "execute" command.
runCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
runCommand era network socket =
  O.command "execute" $
    O.info (runOptions era network socket) $
      O.progDesc "Run a Marlowe transaction."

-- | Parser for the "execute" options.
runOptions
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
runOptions era network socket =
  Run
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.optional parseMarloweIn
    <*> O.strOption
      ( O.long "marlowe-out-file"
          <> O.metavar "MARLOWE_FILE"
          <> O.help "JSON file with the Marlowe inputs, final state, and final contract."
      )
    <*> (O.many . O.option parseTxIn)
      ( O.long "tx-in" <> O.metavar "TXID#TXIX" <> O.help "Transaction input in TxId#TxIx format."
      )
    <*> (O.many . O.option (parseTxOut era))
      ( O.long "tx-out" <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address" <> O.metavar "ADDRESS" <> O.help "Address to receive ADA in excess of fee."
      )
    <*> requiredSignersOpt
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
  where
    parseMarloweIn :: O.Parser (FilePath, TxIn, TxIn)
    parseMarloweIn =
      (,,)
        <$> O.strOption
          ( O.long "marlowe-in-file"
              <> O.metavar "MARLOWE_FILE"
              <> O.help "JSON file with the Marlowe initial state and initial contract, if any."
          )
        <*> O.option
          parseTxIn
          ( O.long "tx-in-marlowe" <> O.metavar "TXID#TXIX" <> O.help "UTxO spent from Marlowe contract, if any."
          )
        <*> O.option
          parseTxIn
          ( O.long "tx-in-collateral" <> O.metavar "TXID#TXIX" <> O.help "Collateral for transaction, if any."
          )

-- | Parser for the "withdraw" command.
withdrawCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
withdrawCommand era network socket =
  O.command "withdraw" $
    O.info (withdrawOptions era network socket) $
      O.progDesc "Withdraw funds from the Marlowe role address."

-- | Parser for the "withdraw" options.
withdrawOptions
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
withdrawOptions era network socket =
  Withdraw
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.strOption
      ( O.long "marlowe-file"
          <> O.metavar "MARLOWE_FILE"
          <> O.help "JSON file with the Marlowe inputs, final state, and final contract."
      )
    <*> O.option
      parseTokenName
      ( O.long "role-name" <> O.metavar "TOKEN_NAME" <> O.help "The role name for the withdrawal."
      )
    <*> O.option
      parseTxIn
      ( O.long "tx-in-collateral" <> O.metavar "TXID#TXIX" <> O.help "Collateral for transaction."
      )
    <*> (O.many . O.option parseTxIn)
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
    <*> requiredSignersOpt
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

-- | Parser for the "auto-execute" command.
autoRunCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
autoRunCommand era network socket =
  O.command "auto-execute" $
    O.info (autoRunOptions era network socket) $
      O.progDesc "[EXPERIMENTAL] Run a Marlowe transaction, selecting transaction inputs and outputs automatically."

-- | Parser for the "auto-execute" options.
autoRunOptions
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
autoRunOptions era network socket =
  AutoRun
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.optional parseMarloweIn
    <*> O.strOption
      ( O.long "marlowe-out-file"
          <> O.metavar "MARLOWE_FILE"
          <> O.help "JSON file with the Marlowe inputs, final state, and final contract."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address" <> O.metavar "ADDRESS" <> O.help "Address to receive ADA in excess of fee."
      )
    <*> requiredSignersOpt
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
  where
    parseMarloweIn :: O.Parser (FilePath, TxIn)
    parseMarloweIn =
      (,)
        <$> O.strOption
          ( O.long "marlowe-in-file"
              <> O.metavar "MARLOWE_FILE"
              <> O.help "JSON file with the Marlowe initial state and initial contract, if any."
          )
        <*> O.option
          parseTxIn
          ( O.long "tx-in-marlowe" <> O.metavar "TXID#TXIX" <> O.help "UTxO spent from Marlowe contract, if any."
          )

-- | Parser for the "auto-withdraw" command.
autoWithdrawCommand
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
autoWithdrawCommand era network socket =
  O.command "auto-withdraw" $
    O.info (autoWithdrawOptions era network socket) $
      O.progDesc
        "[EXPERIMENTAL] Withdraw funds from the Marlowe role address, selecting transaction inputs and outputs automatically."

-- | Parser for the "auto-withdraw" options.
autoWithdrawOptions
  :: C.BabbageEraOnwards era
  -> O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
autoWithdrawOptions era network socket =
  AutoWithdraw
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.strOption
      ( O.long "marlowe-file"
          <> O.metavar "MARLOWE_FILE"
          <> O.help "JSON file with the Marlowe inputs, final state, and final contract."
      )
    <*> O.option
      parseTokenName
      ( O.long "role-name" <> O.metavar "TOKEN_NAME" <> O.help "The role name for the withdrawal."
      )
    <*> O.option
      (parseAddress era)
      ( O.long "change-address" <> O.metavar "ADDRESS" <> O.help "Address to receive ADA in excess of fee."
      )
    <*> requiredSignersOpt
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

-- | Parser for the "analyze" command.
analyzeCommand
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.CommandFields (RunCommand era)
analyzeCommand network socket =
  O.command "analyze" $
    O.info (analyzeOptions network socket) $
      O.progDesc "[EXPERIMENTAL] Analyze a Marlowe contract."

-- | Parser for the "analyze" options.
analyzeOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Parser (RunCommand era)
analyzeOptions network socket =
  Analyze
    <$> parseNetworkId network
    <*> O.strOption
      ( O.long "socket-path"
          <> O.metavar "SOCKET_FILE"
          <> socket
          <> O.help
            "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
      )
    <*> O.strOption
      ( O.long "marlowe-file" <> O.metavar "MARLOWE_FILE" <> O.help "JSON file with the state and contract."
      )
    <*> O.switch
      ( O.long "preconditions"
          <> O.help "Whether to check preconditions for valid Marlowe state."
      )
    <*> O.switch
      ( O.long "roles" <> O.help "Whether to check lengths of role names."
      )
    <*> O.switch
      ( O.long "tokens" <> O.help "Whether to check lengths of token names."
      )
    <*> O.switch
      ( O.long "maximum-value"
          <> O.help "Whether to check the `maxValueSize` protocol limit."
      )
    <*> O.switch
      ( O.long "minimum-utxo"
          <> O.help "Whether to check the `utxoCostPerWord` protocol limit."
      )
    <*> O.switch
      ( O.long "execution-cost"
          <> O.help "Whether to check the `maxTxExecutionUnits` protocol limit."
      )
    <*> O.switch
      ( O.long "transaction-size"
          <> O.help "Whether to check the `maxTxSize` protocol limit."
      )
    <*> O.switch
      ( O.long "best"
          <> O.help "Whether to compute tight estimates of worst-case bounds, instead of generous estimates of those bounds."
      )
    <*> O.switch
      ( O.long "verbose"
          <> O.help "Whether to include worst-case example in output."
      )
