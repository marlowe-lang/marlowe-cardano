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
{-# LANGUAGE TypeApplications #-}

-- | Role-related commands of the Marlowe CLI tool.
module Language.Marlowe.CLI.Command.Role (
  -- * Marlowe CLI Commands
  RoleCommand (..),
  parseRoleCommand,
  runRoleCommand,
) where

import Cardano.Api (NetworkId (..), StakeAddressReference (..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (
  ScriptFilesOptions,
  loadMarloweScripts,
  parseCurrencySymbol,
  parseNetworkId,
  parseStakeAddressReference,
  parseTokenName,
  protocolVersionOpt,
  scriptsFilesOptions,
 )
import Language.Marlowe.CLI.Export (exportRoleAddress, exportRoleDatum, exportRoleRedeemer, exportRoleValidator)
import Language.Marlowe.CLI.IO (getDefaultCostModel)
import Language.Marlowe.CLI.Types (CliEnv, CliError (..))
import Marlowe.Plutus.Semantics.Types (Token (Token))
import Options.Applicative qualified as O
import PlutusLedgerApi.Common (MajorProtocolVersion)
import PlutusLedgerApi.V1 (CurrencySymbol, TokenName)

-- | Marlowe CLI commands and options for exporting role information.
data RoleCommand
  = -- | Export the role address for a Marlowe contract.
    ExportAddress
      { network :: NetworkId
      -- ^ The network ID, if any.
      , stake :: Maybe StakeAddressReference
      -- ^ The stake address, if any.
      , scriptFiles :: ScriptFilesOptions
      -- ^ Paths to Marlowe Plutus scripts.
      }
  | -- | Export the role validator for a Marlowe contract.
    ExportValidator
      { network :: NetworkId
      -- ^ The network ID, if any.
      , stake :: Maybe StakeAddressReference
      -- ^ The stake address, if any.
      , protocolVersion :: MajorProtocolVersion
      -- ^ Protocol version
      , outputFile :: Maybe FilePath
      -- ^ The output JSON file for the validator information.
      , printHash :: Bool
      -- ^ Whether to print the validator hash.
      , printStats :: Bool
      -- ^ Whether to print statistics about the contract.
      , scriptFiles :: ScriptFilesOptions
      -- ^ Paths to Marlowe Plutus scripts.
      }
  | -- | Export the role datum for a Marlowe contract transaction.
    ExportDatum
      { rolesCurrency' :: CurrencySymbol
      -- ^ The role currency symbols, if any.
      , roleName :: TokenName
      -- ^ The role name.
      , outputFile :: Maybe FilePath
      -- ^ The output JSON file for the datum.
      , printStats :: Bool
      -- ^ Whether to print statistics about the datum.
      }
  | -- | Export the role redeemer for a Marlowe contract transaction.
    ExportRedeemer
      { outputFile :: Maybe FilePath
      -- ^ The output JSON file for the redeemer.
      , printStats :: Bool
      -- ^ Whether to print statistics about the redeemer.
      }

-- | Run a related command.
runRoleCommand
  :: (MonadError CliError m)
  => (MonadReader (CliEnv era) m)
  => (MonadIO m)
  => RoleCommand
  -- ^ The command.
  -> m ()
-- ^ Action for running the command.
runRoleCommand command =
  do
    -- FIXME: we should use `getPV2CostModel` here - add node socket path to the command.
    costModel <- getDefaultCostModel
    let network' = network command
        stake' = fromMaybe NoStakeAddress $ stake command
    case command of
      ExportAddress{..} -> do
        loadedScripts <- liftIO $ loadMarloweScripts scriptFiles
        case loadedScripts of
          Left err -> throwError err
          Right (_, _, _, mrt) -> do
            case mrt of
              Nothing ->
                throwError
                  $ CliError
                  $ "Role address requires a role-token minting policy; pass --role-tokens-script-file or include roleTokens in --scripts-suite-file."
              Just roleValidator ->
                exportRoleAddress @_ roleValidator network' stake'
      ExportValidator{..} -> do
        loadedScripts <- liftIO $ loadMarloweScripts scriptFiles
        case loadedScripts of
          Left err -> throwError err
          Right (_, payoutValidator, _, _) ->
            exportRoleValidator @_
              payoutValidator
              protocolVersion
              (Map.elems $ fromIntegral <$> costModel)
              network'
              stake'
              outputFile
              printHash
              printStats
      ExportDatum{..} ->
        exportRoleDatum
          (Token rolesCurrency' roleName)
          outputFile
          printStats
      ExportRedeemer{..} ->
        exportRoleRedeemer
          outputFile
          printStats

-- | Parser for related commands.
parseRoleCommand
  :: O.Mod O.OptionFields NetworkId
  -> O.Parser RoleCommand
parseRoleCommand network =
  O.hsubparser $
    O.commandGroup "Low-level commands for exporting Marlowe role information:"
      <> exportAddressCommand network
      <> exportDatumCommand
      <> exportRedeemerCommand
      <> exportValidatorCommand network

-- | Parser for the "address" command.
exportAddressCommand
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.CommandFields RoleCommand
exportAddressCommand network =
  O.command "address"
    . O.info (exportAddressOptions network)
    $ O.progDesc "Print a role validator address."

-- | Parser for the "address" options.
exportAddressOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Parser RoleCommand
exportAddressOptions network =
  ExportAddress
    <$> parseNetworkId network
    <*> (O.optional . O.option parseStakeAddressReference)
      ( O.long "stake-address" <> O.metavar "ADDRESS" <> O.help "Stake address, if any."
      )
    <*> scriptsFilesOptions

-- | Parser for the "validator" command.
exportValidatorCommand
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.CommandFields RoleCommand
exportValidatorCommand network =
  O.command "validator"
    . O.info (exportValidatorOptions network)
    $ O.progDesc "Export a role validator to a JSON file."

-- | Parser for the "validator" options.
exportValidatorOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Parser RoleCommand
exportValidatorOptions network =
  ExportValidator
    <$> parseNetworkId network
    <*> (O.optional . O.option parseStakeAddressReference)
      ( O.long "stake-address" <> O.metavar "ADDRESS" <> O.help "Stake address, if any."
      )
    <*> protocolVersionOpt
    <*> (O.optional . O.strOption)
      ( O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for validator."
      )
    <*> O.switch
      ( O.long "print-hash" <> O.help "Print validator hash."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )
    <*> scriptsFilesOptions

-- | Parser for the "datum" command.
exportDatumCommand :: O.Mod O.CommandFields RoleCommand
exportDatumCommand =
  O.command "datum"
    . O.info exportDatumOptions
    $ O.progDesc "Export a role datum to a JSON file."

-- | Parser for the "datum" options.
exportDatumOptions :: O.Parser RoleCommand
exportDatumOptions =
  ExportDatum
    <$> O.option
      parseCurrencySymbol
      (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles.")
    <*> O.option
      parseTokenName
      ( O.long "role-name"
          <> O.metavar "TOKEN_NAME"
          <> O.help "The role name for the datum."
      )
    <*> (O.optional . O.strOption)
      ( O.long "out-file" <> O.metavar "DATUM_FILE" <> O.help "JSON output file for datum."
      )
    <*> O.switch
      ( O.long "print-stats" <> O.help "Print statistics."
      )

-- | Parser for the "redeemer" command.
exportRedeemerCommand :: O.Mod O.CommandFields RoleCommand
exportRedeemerCommand =
  O.command "redeemer"
    . O.info exportRedeemerOptions
    $ O.progDesc "Export a role redeemer to a JSON file."

-- | Parser for the "redeemer" options.
exportRedeemerOptions :: O.Parser RoleCommand
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer.")
    <*> O.switch (O.long "print-stats" <> O.help "Print statistics.")
