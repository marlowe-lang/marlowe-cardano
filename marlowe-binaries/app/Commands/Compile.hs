module Commands.Compile where

import Cardano.Api (File (File), PlutusScriptVersion (PlutusScriptV3), Script (PlutusScript), writeFileTextEnvelope)
import Cardano.Api.Plutus (PlutusScript (PlutusScriptSerialised))
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString (ByteString, pack)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.Yaml qualified as Y
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), messageFormatParser)
import Marlowe.Plutus.Binaries.Devel qualified as Devel
import Marlowe.Plutus.Binaries.Production qualified as Production
import Options.Applicative (
   Parser,
   ParserInfo,
   ReadM,
   command,
   eitherReader,
   help,
   hsubparser,
   info,
   long,
   many,
   metavar,
   option,
   progDesc,
   short,
   showDefault,
   strOption,
   switch,
   value,
   (<|>)
 )
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import PlutusTx.Builtins (toBuiltin)
import qualified Data.Text as T
import qualified PlutusLedgerApi.V3 as PV3
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Exit (die)
import System.FilePath ((</>))
import Marlowe.Plutus.RoleTokens (RoleTokens, mkRoleTokens)
import Marlowe.Plutus.Binaries.Api.Compile (ScriptOutput(ScriptOutput, scriptName, scriptHash, scriptFile, hashFile), ScriptSuite(ScriptSuite, suiteVariant, responseOutputDir, marloweSemantics, marloweRolePayout, openRoles, roleTokens), ScriptVariant(DevelScripts, ProductionScripts), scriptNameToText, ScriptName(MarloweSemantics, MarloweRolePayout, OpenRoles, MarloweRoleTokens))

data CompileCommand
  = MarloweCompile MarloweCompileCommand
  | PayoutCompile PayoutCompileCommand
  | OpenRolesCompile OpenRolesCompileCommand
  | RoleTokenMintingCompile RoleTokenMintingCompileCommand
  | SuiteCompile SuiteCompileCommand

data MarloweCompileCommand = MarloweCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , outputAbsolutePaths :: Bool
  }

data PayoutCompileCommand = PayoutCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , outputAbsolutePaths :: Bool
  }

data OpenRolesCompileCommand = OpenRolesCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , outputAbsolutePaths :: Bool
  }

data RoleTokenMintingCompileCommand = RoleTokenMintingCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , roleOptions :: [RoleOption]
  , roleHexOptions :: [RoleOption]
  , txOutRef :: TxOutRef
  , outputAbsolutePaths :: Bool
  }

-- | Optional role-token minting policy parameters. Captures the three CLI
-- flags (@--role@, @--role-hex@, @--tx-out-ref@) so that the suite command
-- can either compile the role-token policy (when all three are present) or
-- skip it (when none are present).
data RoleTokensSpec = RoleTokensSpec
  { roleOptionsSpec :: [RoleOption]
  , roleHexOptionsSpec :: [RoleOption]
  , txOutRefSpec :: TxOutRef
  }

-- | Was at least one of the role-token flags supplied? We use this to
-- decide whether the user intended to compile a role-token policy. The
-- companion validator below rejects the "some but not all" case.
roleTokensSpecProvided :: RoleTokensSpec -> Bool
roleTokensSpecProvided RoleTokensSpec{roleOptionsSpec, roleHexOptionsSpec, txOutRefSpec} =
  not (null roleOptionsSpec)
    || not (null roleHexOptionsSpec)
    || case txOutRefSpec of
        TxOutRef "" 0 -> False
        _ -> True

data SuiteCompileCommand = SuiteCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , outputAbsolutePaths :: Bool
  , roleTokensSpec :: RoleTokensSpec
  -- ^ Inputs needed to compile the role-token minting policy. When all
  -- three components are supplied (see 'roleTokensSpecProvided') the
  -- role-token policy is bundled into the emitted 'ScriptSuite';
  -- otherwise it is emitted as @null@.
  }

data RoleOption = RoleOption
  { roleName :: RoleName
  , roleAmount :: Amount
  } deriving (Eq, Show)

newtype RoleName = RoleName ByteString deriving (Eq, Show)
newtype Amount = Amount Integer deriving (Eq, Show)

data TxOutRef = TxOutRef
  { txId :: ByteString
  , txIx :: Integer
  } deriving (Eq, Show)

parseTxOutRef :: ReadM TxOutRef
parseTxOutRef = eitherReader $ \s -> case break (== '#') s of
  (txid, '#' : ixStr) -> case reads ixStr of
    (ix, "") : _ | ix >= 0 -> Right $ TxOutRef (BS8.pack txid) ix
    _ -> Left $ "Invalid txix: " <> ixStr <> ". Expected decimal number."
  _ -> Left "Invalid tx-out-ref format. Expected TXID#INDEX."

parseRoleOption :: ReadM RoleOption
parseRoleOption = eitherReader $ \s ->
  case lastIndexOf '.' (T.pack s) of
    Nothing -> Left $ "Invalid role option format: " <> s <> ". Expected NAME.AMOUNT"
    Just idx ->
      let nameBS = encodeUtf8 $ T.take idx (T.pack s)
          amountStr = T.unpack $ T.drop (idx + 1) (T.pack s)
       in case reads amountStr of
            (amount, "") : _ | amount >= 0 -> Right $ RoleOption (RoleName nameBS) (Amount amount)
            _ -> Left $ "Invalid amount: " <> amountStr <> ". Expected non-negative integer."

parseRoleOptionHex :: ReadM RoleOption
parseRoleOptionHex = eitherReader $ \s ->
  case lastIndexOf '.' (T.pack s) of
    Nothing -> Left $ "Invalid role hex option format: " <> s <> ". Expected HEX.AMOUNT"
    Just idx ->
      let hexStr = T.unpack $ T.take idx (T.pack s)
          amountStr = T.unpack $ T.drop (idx + 1) (T.pack s)
       in case reads amountStr of
            (amount, "") : _ | amount >= 0 -> case parseHex hexStr of
              Nothing -> Left $ "Invalid hex string: " <> s
              Just bs -> Right $ RoleOption (RoleName bs) (Amount amount)
            _ -> Left $ "Invalid amount: " <> amountStr <> ". Expected non-negative integer."
  where
    parseHex :: String -> Maybe ByteString
    parseHex [] = Just ""
    parseHex (a:b:rest) = do
      c1 <- hexChar a
      c2 <- hexChar b
      rest' <- parseHex rest
      pure $ pack [c1, c2] <> rest'
    parseHex [_] = Nothing
    hexChar :: Char -> Maybe Word8
    hexChar c
      | isDigit c = Just $ fromIntegral $ ord c - ord '0'
      | c >= 'a' && c <= 'f' = Just $ fromIntegral $ ord c - ord 'a' + 10
      | c >= 'A' && c <= 'F' = Just $ fromIntegral $ ord c - ord 'A' + 10
      | otherwise = Nothing

roleOptionsParser :: Parser [RoleOption]
roleOptionsParser = many $ option parseRoleOption
  ( long "role"
      <> metavar "NAME.AMOUNT"
      <> help "Role name (UTF-8) and amount, separated by the last dot."
  )

roleHexOptionsParser :: Parser [RoleOption]
roleHexOptionsParser = many $ option parseRoleOptionHex
  ( long "role-hex"
      <> metavar "HEX.AMOUNT"
      <> help "Role name (hex encoded) and amount, separated by the last dot."
  )

txOutRefParser :: Parser TxOutRef
txOutRefParser = option parseTxOutRef
  ( long "tx-out-ref"
      <> metavar "TXID#INDEX"
      <> help "Transaction output reference (txid#index)"
  )

-- | Read a @--tx-out-ref@ option into the suite's role-token spec. When
-- the user did not pass the flag at all we keep the field at its default
-- empty 'TxOutRef' (which 'roleTokensSpecProvided' treats as absent).
txOutRefSpecParser :: Parser TxOutRef
txOutRefSpecParser = txOutRefParser <|> pure (TxOutRef "" 0)

-- | Combined parser for the three role-token flags reused by both the
-- dedicated @role-tokens-minting@ subcommand and the @suite@ subcommand.
roleTokensSpecParser :: Parser RoleTokensSpec
roleTokensSpecParser =
  RoleTokensSpec
    <$> roleOptionsParser
    <*> roleHexOptionsParser
    <*> txOutRefSpecParser

develScriptsParser :: Parser Bool
develScriptsParser = switch
  ( long "devel-scripts"
      <> help "Compile the devel script variants with tracing preserved."
  )

outputDirParser :: Parser FilePath
outputDirParser = strOption
  ( long "output-dir"
      <> short 'o'
      <> value "out"
      <> showDefault
      <> help "Directory where `.plutus` files and hashes will be written."
  )

outputAbsolutePathsParser :: Parser Bool
outputAbsolutePathsParser = switch
  ( long "output-absolute-paths"
      <> help "Emit absolute (resolved against the current working directory) paths in the produced ScriptSuite / ScriptOutput documents instead of relative ones."
  )

marloweCompileParser :: ParserInfo MarloweCompileCommand
marloweCompileParser =
  info
    ( MarloweCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
        <*> outputAbsolutePathsParser
    )
    (progDesc "Compile Marlowe validator script.")

payoutCompileParser :: ParserInfo PayoutCompileCommand
payoutCompileParser =
  info
    ( PayoutCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
        <*> outputAbsolutePathsParser
    )
    (progDesc "Compile Role Payout validator script.")

openRolesCompileParser :: ParserInfo OpenRolesCompileCommand
openRolesCompileParser =
  info
    ( OpenRolesCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
        <*> outputAbsolutePathsParser
    )
    (progDesc "Compile Open Roles validator script.")

roleTokenMintingCompileParser :: ParserInfo RoleTokenMintingCompileCommand
roleTokenMintingCompileParser =
  info
    ( RoleTokenMintingCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
        <*> roleOptionsParser
        <*> roleHexOptionsParser
        <*> txOutRefParser
        <*> outputAbsolutePathsParser
    )
    (progDesc "Compile Role Token Minting policy.")

suiteCompileParser :: ParserInfo SuiteCompileCommand
suiteCompileParser =
  info
    ( SuiteCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
        <*> outputAbsolutePathsParser
        <*> roleTokensSpecParser
    )
    (progDesc
      ( "Compile the full Marlowe script suite (marlowe, payout, open roles) \
        \and emit a ScriptSuite JSON/YAML description. When --role, --role-hex \
        \and --tx-out-ref are all supplied, the role-token minting policy is \
        \included in the suite as well."
      )
  )

-- | Validate that the three role-token flags are either all supplied or
-- none of them. Called from 'runSuiteCompile' after parsing.
validateRoleTokensSpec :: RoleTokensSpec -> Either String ()
validateRoleTokensSpec spec =
  let
    RoleTokensSpec{roleOptionsSpec, roleHexOptionsSpec, txOutRefSpec = TxOutRef txid ix} = spec
    hasRoles = not (null roleOptionsSpec) || not (null roleHexOptionsSpec)
    hasTxOutRef = txid /= "" || ix /= 0
  in case (hasRoles, hasTxOutRef) of
    (False, False) -> Right ()
    (True, True) -> Right ()
    (False, True) ->
      Left "When providing --tx-out-ref you must also provide --role and/or --role-hex."
    (True, False) ->
      Left "When providing --role/--role-hex you must also provide --tx-out-ref."

compileCommandParser :: ParserInfo CompileCommand
compileCommandParser = info parser (progDesc "Compile and export Marlowe validator scripts.")
  where
    parser = hsubparser $
      command "marlowe" (MarloweCompile <$> marloweCompileParser)
      <> command "payout" (PayoutCompile <$> payoutCompileParser)
      <> command "open-roles" (OpenRolesCompile <$> openRolesCompileParser)
      <> command "role-tokens-minting" (RoleTokenMintingCompile <$> roleTokenMintingCompileParser)
      <> command "suite" (SuiteCompile <$> suiteCompileParser)

runCompileCommand :: CompileCommand -> IO ()
runCompileCommand = \case
  MarloweCompile cmd -> runMarloweCompile cmd
  PayoutCompile cmd -> runPayoutCompile cmd
  OpenRolesCompile cmd -> runOpenRolesCompile cmd
  RoleTokenMintingCompile cmd -> runRoleTokenMintingCompile cmd
  SuiteCompile cmd -> runSuiteCompile cmd

runMarloweCompile :: MarloweCompileCommand -> IO ()
runMarloweCompile MarloweCompileCommand{develScripts, outputDir, messageFormat, outputAbsolutePaths} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " marlowe script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compileMarloweScript variant outputDir outputAbsolutePaths
  either (emitError messageFormat) (emitSummary messageFormat) result

runOpenRolesCompile :: OpenRolesCompileCommand -> IO ()
runOpenRolesCompile OpenRolesCompileCommand{develScripts, outputDir, messageFormat, outputAbsolutePaths} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " open roles script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compileOpenRoles variant outputDir outputAbsolutePaths
  either (emitError messageFormat) (emitSummary messageFormat) result

runPayoutCompile :: PayoutCompileCommand -> IO ()
runPayoutCompile PayoutCompileCommand{develScripts, outputDir, messageFormat, outputAbsolutePaths} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " payout script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compilePayoutScript variant outputDir outputAbsolutePaths
  either (emitError messageFormat) (emitSummary messageFormat) result

runRoleTokenMintingCompile :: RoleTokenMintingCompileCommand -> IO ()
runRoleTokenMintingCompile cmd = do
  let
    RoleTokenMintingCompileCommand{develScripts, outputDir, messageFormat, roleOptions, roleHexOptions, txOutRef, outputAbsolutePaths} = cmd
    variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " payout script to " <> show outputDir <> "."
    _ -> pure ()
  let
    roles = mkRoleTokens $ flip map (roleOptions ++ roleHexOptions) \(RoleOption (RoleName name) (Amount amount)) -> do
      let
        tokenName = PV3.TokenName . PV3.toBuiltin $ name
      (tokenName, amount)
  result <- compileRoleTokenMintingScript variant outputDir roles txOutRef outputAbsolutePaths
  either (emitError messageFormat) (emitSummary messageFormat) result

runSuiteCompile :: SuiteCompileCommand -> IO ()
runSuiteCompile SuiteCompileCommand{develScripts, outputDir, messageFormat, outputAbsolutePaths, roleTokensSpec} = do
  case validateRoleTokensSpec roleTokensSpec of
    Left err -> emitError messageFormat err
    Right () -> do
      let variant = if develScripts then DevelScripts else ProductionScripts
          RoleTokensSpec{roleOptionsSpec = ro, roleHexOptionsSpec = rh, txOutRefSpec = tr} = roleTokensSpec
          includeRoleTokens = roleTokensSpecProvided roleTokensSpec
          mRoleTokensInput
            | includeRoleTokens =
                Just (mkRoleTokens (map roleOptToPair (ro <> rh)), tr)
            | otherwise = Nothing
      case messageFormat of
        MessageFormatText -> putStrLn $ "Writing " <> show variant <> " script suite to " <> show outputDir <> "."
        _ -> pure ()
      result <- compileSuite variant outputDir outputAbsolutePaths mRoleTokensInput
      either (emitError messageFormat) (emitSummarySuite messageFormat) result
  where
    roleOptToPair (RoleOption (RoleName name) (Amount amount)) =
      (PV3.TokenName . PV3.toBuiltin $ name, amount)

toPV3TxOutRef :: TxOutRef -> PV3.TxOutRef
toPV3TxOutRef (TxOutRef tid ix) =
  let txIdBuiltin = toBuiltin tid :: PV3.BuiltinByteString
      txId = PV3.TxId txIdBuiltin
  in PV3.TxOutRef txId ix

compileMarloweScript :: ScriptVariant -> FilePath -> Bool -> IO (Either String ScriptOutput)
compileMarloweScript variant outputDir outputAbsolutePaths = do
  let (hash, bytes) = case variant of
        DevelScripts -> (Devel.marloweValidatorHash, Devel.marloweValidatorBytes)
        ProductionScripts -> (Production.marloweValidatorHash, Production.marloweValidatorBytes)
      scriptName = MarloweSemantics
      baseName = T.unpack . scriptNameToText $ scriptName
      scriptFile = outputDir </> baseName <> ".plutus"
      hashFile = outputDir </> baseName <> ".plutus.hash"
      scriptHash = show hash
  createDirectoryIfMissing True outputDir
  result <- writeFileTextEnvelope
    (File scriptFile)
    Nothing
    (PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes))
  case result of
    Left err -> pure $ Left $ show err
    Right () -> do
      writeFile hashFile (scriptHash <> "\n")
      resolved <- resolvePaths outputAbsolutePaths scriptFile hashFile
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile = fst resolved, hashFile = snd resolved}

compilePayoutScript :: ScriptVariant -> FilePath -> Bool -> IO (Either String ScriptOutput)
compilePayoutScript variant outputDir outputAbsolutePaths = do
  let (hash, bytes) = case variant of
        DevelScripts -> (Devel.rolePayoutValidatorHash, Devel.rolePayoutValidatorBytes)
        ProductionScripts -> (Production.rolePayoutValidatorHash, Production.rolePayoutValidatorBytes)
      scriptName = MarloweRolePayout
      baseName = T.unpack . scriptNameToText $ scriptName
      scriptFile = outputDir </> baseName <> ".plutus"
      hashFile = outputDir </> baseName <> ".plutus.hash"
      scriptHash = show hash
  createDirectoryIfMissing True outputDir
  result <- writeFileTextEnvelope
    (File scriptFile)
    Nothing
    (PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes))
  case result of
    Left err -> pure $ Left $ show err
    Right () -> do
      writeFile hashFile (scriptHash <> "\n")
      resolved <- resolvePaths outputAbsolutePaths scriptFile hashFile
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile = fst resolved, hashFile = snd resolved}

compileOpenRoles :: ScriptVariant -> FilePath -> Bool -> IO (Either String ScriptOutput)
compileOpenRoles variant outputDir outputAbsolutePaths = do
  let (hash, bytes) = case variant of
        DevelScripts -> (Devel.openRolesValidatorHash, Devel.openRolesValidatorBytes)
        ProductionScripts -> (Production.openRolesValidatorHash, Production.openRolesValidatorBytes)
      scriptName = OpenRoles
      baseName = T.unpack . scriptNameToText $ scriptName
      scriptFile = outputDir </> baseName <> ".plutus"
      hashFile = outputDir </> baseName <> ".plutus.hash"
      scriptHash = show hash
  createDirectoryIfMissing True outputDir
  result <- writeFileTextEnvelope
    (File scriptFile)
    Nothing
    (PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes))
  case result of
    Left err -> pure $ Left $ show err
    Right () -> do
      writeFile hashFile (scriptHash <> "\n")
      resolved <- resolvePaths outputAbsolutePaths scriptFile hashFile
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile = fst resolved, hashFile = snd resolved}

compileRoleTokenMintingScript :: ScriptVariant -> FilePath -> RoleTokens -> TxOutRef -> Bool -> IO (Either String ScriptOutput)
compileRoleTokenMintingScript variant outputDir roles txOutRef outputAbsolutePaths = do
  let (hash, bytes) = case variant of
        DevelScripts -> (Devel.mkRoleTokensPolicyHash roles (toPV3TxOutRef txOutRef), Devel.mkRoleTokensPolicyBytes roles (toPV3TxOutRef txOutRef))
        ProductionScripts -> (Production.mkRoleTokensPolicyHash roles (toPV3TxOutRef txOutRef), Production.mkRoleTokensPolicyBytes roles (toPV3TxOutRef txOutRef))
      scriptName = MarloweRoleTokens
      baseName = T.unpack . scriptNameToText $ scriptName
      scriptFile = outputDir </> baseName <> ".plutus"
      hashFile = outputDir </> baseName <> ".plutus.hash"
      scriptHash = show hash
  createDirectoryIfMissing True outputDir
  result <- writeFileTextEnvelope
    (File scriptFile)
    Nothing
    (PlutusScript PlutusScriptV3 (PlutusScriptSerialised bytes))
  case result of
    Left err -> pure $ Left $ show err
    Right () -> do
      writeFile hashFile (scriptHash <> "\n")
      resolved <- resolvePaths outputAbsolutePaths scriptFile hashFile
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile = fst resolved, hashFile = snd resolved}

-- | Compile the full Marlowe script suite (marlowe semantics + payout +
-- open roles) into @outputDir@ and return a 'ScriptSuite' describing the
-- generated files. When @mRoleTokens@ is supplied the role-token minting
-- policy is bundled into the suite as well.
compileSuite
  :: ScriptVariant
  -> FilePath
  -> Bool
  -> Maybe (RoleTokens, TxOutRef)
  -> IO (Either String ScriptSuite)
compileSuite variant outputDir outputAbsolutePaths mRoleTokens = liftIO $ runExceptT $ do
  semantics <- ExceptT $ compileMarloweScript variant outputDir outputAbsolutePaths
  payout <- ExceptT $ compilePayoutScript variant outputDir outputAbsolutePaths
  openRoles <- ExceptT $ compileOpenRoles variant outputDir outputAbsolutePaths
  roleTokensOut <-
    forM mRoleTokens $ \(tokens, txOutRef) ->
      ExceptT $ compileRoleTokenMintingScript variant outputDir tokens txOutRef outputAbsolutePaths
  resolvedOutputDir <- liftIO $ if outputAbsolutePaths then absPath outputDir else pure outputDir
  pure
    ScriptSuite
      { suiteVariant = variant
      , responseOutputDir = resolvedOutputDir
      , marloweSemantics = semantics
      , marloweRolePayout = payout
      , openRoles = openRoles
      , roleTokens = roleTokensOut
      }

-- | Resolve @scriptFile@ and @hashFile@ to absolute paths when
-- @outputAbsolutePaths@ is set, or leave them untouched otherwise.
resolvePaths :: Bool -> FilePath -> FilePath -> IO (FilePath, FilePath)
resolvePaths False sf hf = pure (sf, hf)
resolvePaths True sf hf = do
  absSf <- absPath sf
  absHf <- absPath hf
  pure (absSf, absHf)

-- | Make a path absolute, resolving against the current working directory.
absPath :: FilePath -> IO FilePath
absPath p = makeAbsolute p

emitSummary :: MessageFormat -> ScriptOutput -> IO ()
emitSummary messageFormat output = case messageFormat of
  MessageFormatText -> do
    putStrLn $ "  wrote " <> scriptFile output
    putStrLn $ "  hash  " <> scriptHash output
  MessageFormatJson -> LBS8.putStrLn $ A.encodePretty output
  MessageFormatYaml -> BS8.putStrLn $ Y.encode output

emitSummarySuite :: MessageFormat -> ScriptSuite -> IO ()
emitSummarySuite messageFormat suite = case messageFormat of
  MessageFormatText -> do
    let renderScript label ScriptOutput{scriptFile, scriptHash} = do
          putStrLn $ "  [" <> label <> "]"
          putStrLn $ "    wrote " <> scriptFile
          putStrLn $ "    hash  " <> scriptHash
    renderScript "marlowe-semantics" (marloweSemantics suite)
    renderScript "marlowe-rolepayout" (marloweRolePayout suite)
    renderScript "marlowe-openroles" (openRoles suite)
  MessageFormatJson -> LBS8.putStrLn $ A.encodePretty suite
  MessageFormatYaml -> BS8.putStrLn $ Y.encode suite

emitError :: MessageFormat -> String -> IO a
emitError messageFormat err =
  case messageFormat of
    MessageFormatText -> die err
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "compile command failed"
    MessageFormatYaml -> BS8.putStrLn (Y.encode err) >> die "compile command failed"

lastIndexOf :: Char -> Text -> Maybe Int
lastIndexOf c t = case T.findIndex (== c) (T.reverse t) of
  Nothing -> Nothing
  Just i -> Just $ T.length t - 1 - i

