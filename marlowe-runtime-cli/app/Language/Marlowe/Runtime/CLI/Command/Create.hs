{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.CLI.Command.Create where

import qualified Cardano.Api as C
import Cardano.Api.Shelley (BabbageEraOnwards (..), File (..))
import qualified Cardano.Api.Shelley as C
import Control.Error.Util (hoistMaybe, noteT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import qualified Data.Aeson as A
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Char8 as BS8
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (pack)
import qualified Data.Yaml as Yaml
import Data.Yaml.Aeson (decodeFileEither)
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime (POSIXTime))
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Runtime.CLI.Command.Tx (SigningMethod (Manual), TxCommand (..), txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLIExceptT)
import Language.Marlowe.Runtime.CLI.Option (keyValueOption, marloweVersionParser, parseAddress)
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  DatumHash,
  Lovelace (Lovelace),
  PolicyId,
  Quantity (..),
  StakeCredential,
  TokenName (..),
  TransactionMetadata,
  fromCardanoStakeCredential,
  fromJSONEncodedMetadata,
  fromJSONEncodedTransactionMetadata,
  renderTxOutRef,
 )
import Language.Marlowe.Runtime.Client (createContract)
import Language.Marlowe.Runtime.Core.Api (
  ContractId (ContractId),
  IsMarloweVersion (Contract),
  MarloweMetadata (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (MarloweV1),
  SomeMarloweVersion (SomeMarloweVersion),
 )
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateError,
  Destination (ToAddress),
  RoleTokenMetadata,
  RoleTokensConfig (..),
  mkMint,
 )
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data CreateCommand = CreateCommand
  { marloweVersion :: SomeMarloweVersion
  , stakeCredential :: Maybe StakeCredential
  , roles :: Maybe RolesConfig
  , contractFiles :: ContractFiles
  , minUTxO :: Maybe Lovelace
  }

data RoleConfig = RoleConfig
  { address :: Address
  , metadata :: RoleTokenMetadata
  }
  deriving (Generic, A.FromJSON)

data RolesConfig
  = MintSimple (NonEmpty (TokenName, Address))
  | MintConfig FilePath
  | UseExistingPolicyId PolicyId
  deriving (Show)

data ContractFiles
  = CoreFile FilePath
  | ExtendedFiles FilePath ContractArgs
  | ContractHash DatumHash

data ContractArgs
  = ContractArgsByFile FilePath
  | ContractArgsByValue ContractArgsValue

data ContractArgsValue = ContractArgsValue
  { timeoutArguments :: Map String POSIXTime
  , valueArguments :: Map String Integer
  }

data CreateCommandError
  = CreateFailed CreateError
  | ContractFileDecodingError Yaml.ParseException
  | TransactionFileWriteFailed (C.FileError ())
  | RolesConfigFileDecodingError String
  | MetadataDecodingFailed (Maybe Yaml.ParseException)
  | TagsDecodingFailed (Maybe Yaml.ParseException)
  | ExtendedContractsAreNotSupportedYet

deriving instance Show CreateCommandError

createCommandParser :: ParserInfo (TxCommand CreateCommand)
createCommandParser = info (txCommandParser True parser) $ progDesc "Create a new Marlowe Contract"
  where
    parser =
      CreateCommand
        <$> marloweVersionParser
        <*> stakeCredentialParser
        <*> rolesParser
        <*> contractFilesParser
        <*> minUTxOParser
    rolesParser = optional (mintSimpleParser <|> mintConfigParser <|> policyIdParser)
    stakeCredentialParser =
      let stakeCredentialReader =
            eitherReader
              \s ->
                case C.deserialiseAddress C.AsStakeAddress $ pack s of
                  Just (C.StakeAddress _ credential) -> Right . fromCardanoStakeCredential $ C.fromShelleyStakeCredential credential
                  Nothing -> Left ("Invalid Bech32 stake address." :: String)
       in optional $
            option stakeCredentialReader $
              mconcat
                [ long "stake-address"
                , help $
                    unwords
                      [ "The Bech32-encoded stake address to be used for the staking credential"
                      , "in the Marlowe validator address."
                      ]
                , metavar "STAKE_ADDRESS"
                ]
    mintSimpleParser = MintSimple <$> some1 roleParser
    mintConfigParser =
      fmap MintConfig $
        strOption $
          mconcat
            [ long "roles-config-file"
            , help $
                unwords
                  [ "A JSON file containing a map of role token names to a roles configuration object."
                  , "The roles configuration object has two keys, \"address\" and \"metadata\","
                  , "where \"address\" is the address to send the newly minted role token and \"metadata\""
                  , "is the CIP-25 metadata object to associate with the token."
                  ]
            , metavar "FILE_PATH"
            ]
    policyIdParser =
      fmap UseExistingPolicyId $
        strOption $
          mconcat
            [ long "role-token-policy-id"
            , metavar "POLICY_ID"
            , help
                "The hexadecimal-encoded policy ID of the role tokens for this contract. This option is used to support role tokens minted in a separate transaction."
            ]
    roleParser =
      keyValueOption (Right . TokenName . fromString) parseAddress $
        mconcat
          [ long "role"
          , short 'r'
          , help "The name of a role in the contract with the address to send the token to"
          , metavar "ROLE=ADDRESS"
          ]
    contractFilesParser = CoreFile <$> coreParser <|> extendedParser <|> ContractHash <$> contractHashParser
    coreParser =
      strOption $
        mconcat
          [ long "core-file"
          , help "A file containing the Core Marlowe JSON definition of the contract to create."
          , metavar "FILE_PATH"
          ]
    contractHashParser =
      strOption $
        mconcat
          [ long "contract-hash"
          , help "The hash of a contract in the contract store to create."
          , metavar "BASE_16"
          ]
    extendedParser = ExtendedFiles <$> extendedFileParser <*> contractArgsParser
    extendedFileParser =
      strOption $
        mconcat
          [ long "contract-file"
          , help "A file containing the Extended Marlowe JSON definition of the contract to create."
          , metavar "FILE_PATH"
          ]
    contractArgsParser =
      ContractArgsByFile <$> argsFileParser <|> ContractArgsByValue <$> contractArgsValueParser
    argsFileParser =
      strOption $
        mconcat
          [ long "args-file"
          , help "A file containing the Extended Marlowe arguments to apply to the contract."
          , metavar "FILE_PATH"
          ]
    contractArgsValueParser =
      ContractArgsValue
        <$> timeoutArgumentsParser
        <*> valueArgumentsParser
    timeoutArgumentsParser = Map.fromList <$> many timeoutArgumentParser
    valueArgumentsParser = Map.fromList <$> many valueArgumentParser
    timeoutArgumentParser =
      keyValueOption Right (fmap POSIXTime . integerParser) $
        mconcat
          [ long "timeout-arg"
          , metavar "NAME=POSIX_TIMESTAMP"
          , help "The name of a timeout parameter in the contract and a value to assign to it (in POSIX milliseconds)."
          ]
    valueArgumentParser =
      keyValueOption Right integerParser $
        mconcat
          [ long "value-arg"
          , metavar "NAME=INTEGER"
          , help "The name of a numeric parameter in the contract and a value to assign to it."
          ]
    integerParser = maybe (Left "Invalid Integer value") Right . readMaybe
    minUTxOParser =
      optional $
        option (Lovelace <$> auto) $
          mconcat
            [ long "min-utxo"
            , help "The amount of ADA to deposit into the contract to ensure the contract output satisfies min ADA requirements."
            , metavar "LOVELACE"
            ]

runCreateCommand :: TxCommand CreateCommand -> CLI ()
runCreateCommand TxCommand{walletAddresses, signingMethod, tagsFile, metadataFile, subCommand = CreateCommand{..}} = case marloweVersion of
  SomeMarloweVersion MarloweV1 -> runCLIExceptT do
    minting' <- case roles of
      Nothing -> pure RoleTokensNone
      Just (MintSimple tokens) -> do
        let toNFT (token, addr) = (token, Nothing, ToAddress addr, Quantity 1)
        pure $ RoleTokensMint $ mkMint $ toNFT <$> tokens
      Just (UseExistingPolicyId policyId) -> pure $ RoleTokensUsePolicy policyId mempty
      Just (MintConfig roleTokensConfigFilePath) -> do
        configMap <- ExceptT $ liftIO $ first RolesConfigFileDecodingError <$> A.eitherDecodeFileStrict roleTokensConfigFilePath
        case Map.toList configMap of
          [] -> throwE $ RolesConfigFileDecodingError "Empty role token config"
          (x : xs) ->
            pure $
              RoleTokensMint $
                mkMint $
                  (\(token, RoleConfig{..}) -> (token, Just metadata, ToAddress address, Quantity 1)) <$> x :| xs
    (ContractId contractId, safetyErrors) <- run MarloweV1 minting'
    liftIO $
      if null safetyErrors
        then hPutStrLn stderr "Safety analysis found no errors in the contract."
        else do
          hPutStrLn stderr "Safety analysis found the following errors in the contract:"
          BS8.hPutStrLn stderr $ Yaml.encode safetyErrors
    liftIO . print $ A.encode (A.object [("contractId", A.toJSON . renderTxOutRef $ contractId)])
  where
    readContract :: MarloweVersion v -> ExceptT CreateCommandError CLI (Either (Contract v) DatumHash)
    readContract = \case
      MarloweV1 -> case contractFiles of
        CoreFile filePath -> ExceptT $ liftIO $ bimap ContractFileDecodingError Left <$> decodeFileEither filePath
        ExtendedFiles _ _ -> do
          -- extendedContract <- ExceptT $ liftIO $ first (ContractFileDecodingError . Just) <$> decodeFileEither filePath
          throwE ExtendedContractsAreNotSupportedYet
        ContractHash hash -> pure $ Right hash

    readMetadata :: ExceptT CreateCommandError CLI TransactionMetadata
    readMetadata = case metadataFile of
      Just filePath -> do
        metadataJSON <- ExceptT $ liftIO $ first (MetadataDecodingFailed . Just) <$> decodeFileEither filePath
        noteT (MetadataDecodingFailed Nothing) $ hoistMaybe (fromJSONEncodedTransactionMetadata metadataJSON)
      Nothing -> pure mempty

    readTags :: ExceptT CreateCommandError CLI (Maybe MarloweMetadata)
    readTags = case tagsFile of
      Just filePath ->
        Just <$> do
          tagsJSON <- ExceptT $ liftIO $ first (TagsDecodingFailed . Just) <$> decodeFileEither filePath
          tags <- noteT (TagsDecodingFailed Nothing) $ hoistMaybe (traverse (traverse fromJSONEncodedMetadata) tagsJSON)
          pure $ MarloweMetadata tags Nothing
      Nothing -> pure Nothing

    run :: MarloweVersion v -> RoleTokensConfig -> ExceptT CreateCommandError CLI (ContractId, [SafetyError])
    run version rolesDistribution = do
      contract <- readContract version
      metadata <- MarloweTransactionMetadata <$> readTags <*> readMetadata
      ContractCreated era ContractCreatedInEra{contractId, txBody, safetyErrors} <-
        ExceptT $
          first CreateFailed
            <$> createContract stakeCredential version walletAddresses Nothing rolesDistribution metadata minUTxO mempty contract
      case signingMethod of
        Manual outputFile -> do
          ExceptT @_ @_ @() $
            liftIO $
              first TransactionFileWriteFailed <$> case era of
                BabbageEraOnwardsBabbage -> C.writeFileTextEnvelope (File outputFile) Nothing txBody
                BabbageEraOnwardsConway -> C.writeFileTextEnvelope (File outputFile) Nothing txBody
          pure (contractId, safetyErrors)
