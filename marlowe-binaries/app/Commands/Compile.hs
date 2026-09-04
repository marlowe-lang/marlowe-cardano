module Commands.Compile where

import Cardano.Api (File (File), PlutusScriptVersion (PlutusScriptV3), Script (PlutusScript), writeFileTextEnvelope)
import Cardano.Api.Plutus (PlutusScript (PlutusScriptSerialised))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString (ByteString, pack)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Char (isDigit, ord)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.Yaml qualified as Y
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
 )
import PlutusTx.Builtins (toBuiltin)
import qualified Data.Text as T
import qualified PlutusLedgerApi.V3 as PV3
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath ((</>))
import Marlowe.Plutus.RoleTokens (RoleTokens, mkRoleTokens)
import Marlowe.Plutus.Binaries.Api.Compile (ScriptOutput(ScriptOutput, scriptName, scriptHash, scriptFile, hashFile), scriptNameToText, ScriptName(MarloweSemantics, MarloweRolePayout, OpenRoles))

data CompileCommand
  = MarloweCompile MarloweCompileCommand
  | PayoutCompile PayoutCompileCommand
  | OpenRolesCompile OpenRolesCompileCommand
  | RoleTokenMintingCompile RoleTokenMintingCompileCommand

data MarloweCompileCommand = MarloweCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  }

data PayoutCompileCommand = PayoutCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  }

data OpenRolesCompileCommand = OpenRolesCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  }

data RoleTokenMintingCompileCommand = RoleTokenMintingCompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , roleOptions :: [RoleOption]
  , roleHexOptions :: [RoleOption]
  , txOutRef :: TxOutRef
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

data MessageFormat = MessageFormatText | MessageFormatJson | MessageFormatYaml
  deriving (Eq)

instance Show MessageFormat where
  show = \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

instance A.ToJSON MessageFormat where
  toJSON = A.String . \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

readMessageFormat :: ReadM MessageFormat
readMessageFormat = eitherReader $ \case
  "text" -> Right MessageFormatText
  "json" -> Right MessageFormatJson
  "yaml" -> Right MessageFormatYaml
  other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."

messageFormatParser :: Parser MessageFormat
messageFormatParser =
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

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

marloweCompileParser :: ParserInfo MarloweCompileCommand
marloweCompileParser =
  info
    ( MarloweCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
    )
    (progDesc "Compile Marlowe validator script.")

payoutCompileParser :: ParserInfo PayoutCompileCommand
payoutCompileParser =
  info
    ( PayoutCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
    )
    (progDesc "Compile Role Payout validator script.")

openRolesCompileParser :: ParserInfo OpenRolesCompileCommand
openRolesCompileParser =
  info
    ( OpenRolesCompileCommand
        <$> develScriptsParser
        <*> outputDirParser
        <*> messageFormatParser
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
    )
    (progDesc "Compile Role Token Minting policy.")

compileCommandParser :: ParserInfo CompileCommand
compileCommandParser = info parser (progDesc "Compile and export Marlowe validator scripts.")
  where
    parser = hsubparser $
      command "marlowe" (MarloweCompile <$> marloweCompileParser)
      <> command "payout" (PayoutCompile <$> payoutCompileParser)
      <> command "open-roles" (OpenRolesCompile <$> openRolesCompileParser)
      <> command "role-tokens-minting" (RoleTokenMintingCompile <$> roleTokenMintingCompileParser)

runCompileCommand :: CompileCommand -> IO ()
runCompileCommand = \case
  MarloweCompile cmd -> runMarloweCompile cmd
  PayoutCompile cmd -> runPayoutCompile cmd
  OpenRolesCompile cmd -> runOpenRolesCompile cmd
  RoleTokenMintingCompile cmd -> runRoleTokenMintingCompile cmd

runMarloweCompile :: MarloweCompileCommand -> IO ()
runMarloweCompile MarloweCompileCommand{develScripts, outputDir, messageFormat} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " marlowe script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compileMarloweScript variant outputDir
  either (emitError messageFormat) (emitSummary messageFormat) result

runOpenRolesCompile :: OpenRolesCompileCommand -> IO ()
runOpenRolesCompile OpenRolesCompileCommand{develScripts, outputDir, messageFormat} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " open roles script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compileOpenRoles variant outputDir
  either (emitError messageFormat) (emitSummary messageFormat) result

runPayoutCompile :: PayoutCompileCommand -> IO ()
runPayoutCompile PayoutCompileCommand{develScripts, outputDir, messageFormat} = do
  let variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " payout script to " <> show outputDir <> "."
    _ -> pure ()
  result <- compilePayoutScript variant outputDir
  either (emitError messageFormat) (emitSummary messageFormat) result

runRoleTokenMintingCompile :: RoleTokenMintingCompileCommand -> IO ()
runRoleTokenMintingCompile cmd = do
  let
    RoleTokenMintingCompileCommand{develScripts, outputDir, messageFormat, roleOptions, roleHexOptions, txOutRef} = cmd
    variant = if develScripts then DevelScripts else ProductionScripts
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show variant <> " payout script to " <> show outputDir <> "."
    _ -> pure ()
  let
    roles = mkRoleTokens $ flip map (roleOptions ++ roleHexOptions) \(RoleOption (RoleName name) (Amount amount)) -> do
      let
        tokenName = PV3.TokenName . PV3.toBuiltin $ name
      (tokenName, amount)
  result <- compileRoleTokenMintingScript variant outputDir roles txOutRef
  either (emitError messageFormat) (emitSummary messageFormat) result

toPV3TxOutRef :: TxOutRef -> PV3.TxOutRef
toPV3TxOutRef (TxOutRef tid ix) =
  let txIdBuiltin = toBuiltin tid :: PV3.BuiltinByteString
      txId = PV3.TxId txIdBuiltin
  in PV3.TxOutRef txId ix

data ScriptVariant = DevelScripts | ProductionScripts
  deriving (Eq)

compileMarloweScript :: ScriptVariant -> FilePath -> IO (Either String ScriptOutput)
compileMarloweScript variant outputDir = do
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
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile, hashFile}

compilePayoutScript :: ScriptVariant -> FilePath -> IO (Either String ScriptOutput)
compilePayoutScript variant outputDir = do
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
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile, hashFile}

compileOpenRoles :: ScriptVariant -> FilePath -> IO (Either String ScriptOutput)
compileOpenRoles variant outputDir = do
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
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile, hashFile}

compileRoleTokenMintingScript :: ScriptVariant -> FilePath -> RoleTokens -> TxOutRef -> IO (Either String ScriptOutput)
compileRoleTokenMintingScript variant outputDir roles txOutRef = do
  let (hash, bytes) = case variant of
        DevelScripts -> (Devel.mkRoleTokensPolicyHash roles (toPV3TxOutRef txOutRef), Devel.mkRoleTokensPolicyBytes roles (toPV3TxOutRef txOutRef))
        ProductionScripts -> (Production.mkRoleTokensPolicyHash roles (toPV3TxOutRef txOutRef), Production.mkRoleTokensPolicyBytes roles (toPV3TxOutRef txOutRef))
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
      pure $ Right ScriptOutput{scriptName, scriptHash, scriptFile, hashFile}

emitSummary :: MessageFormat -> ScriptOutput -> IO ()
emitSummary messageFormat output = case messageFormat of
  MessageFormatText -> do
    putStrLn $ "  wrote " <> scriptFile output
    putStrLn $ "  hash  " <> scriptHash output
  MessageFormatJson -> LBS8.putStrLn $ A.encodePretty output
  MessageFormatYaml -> BS8.putStrLn $ Y.encode output

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

instance Show ScriptVariant where
  show = \case
    DevelScripts -> "devel"
    ProductionScripts -> "production"

