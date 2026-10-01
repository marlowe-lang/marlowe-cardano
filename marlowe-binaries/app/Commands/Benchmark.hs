{-# LANGUAGE ApplicativeDo #-}

module Commands.Benchmark
  ( benchmarkCommandParser
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Y
import Marlowe.Plutus.Binaries.Api.Benchmark
    ( BenchmarkCaseResult(..),
      BenchmarkGenerateRequest(..),
      BenchmarkGenerateResponse(..),
      BenchmarkGeneratedSuite(..),
      BenchmarkRequest(..),
      BenchmarkResponse(..),
      ResourceUsage(..),
      ScenarioType(..),
      benchmarkScripts,
      generateBenchmarks,
      EvaluationError(..), scenarioId2Hex )
import Marlowe.Plutus.Binaries.Api.Compile ( ScriptOutput(..), ScriptSuite(ScriptSuite, marloweSemantics, marloweRolePayout) )
import Marlowe.Contrib.OptParse.MessageFormat (MessageFormat (..), messageFormatParser)
import Options.Applicative
  ( Parser
  , ParserInfo
  , ReadM
  , auto
  , command
  , eitherReader
  , help
  , hsubparser
  , info
  , long
  , metavar
  , option
  , optional
  , progDesc
  , showDefault
  , strOption
  , value
  )
import Paths_marlowe_binaries (getDataDir)
import System.Exit (die)
import System.FilePath ((</>))
import qualified Data.Aeson.Encode.Pretty as A

benchmarkCommandParser :: ParserInfo (IO ())
benchmarkCommandParser =
  info
    ( hsubparser
        . fold
        $ [ command "generate" (runGenerateCommand <$> generateCommandParser)
          , command "run" (runRunCommand <$> runCommandParser)
          ]
    )
    (progDesc "Generate and run Marlowe benchmarks.")

data BenchmarkRunCommand = BenchmarkRunCommand
  { benchmarkRootDir :: Maybe FilePath
  , messageFormat :: MessageFormat
  , payoutScriptFile :: Maybe FilePath
  , scenarioRootDir :: Maybe FilePath
  , semanticsScriptFile :: Maybe FilePath
  }

runCommandParser :: ParserInfo BenchmarkRunCommand
runCommandParser =
  info
    ( BenchmarkRunCommand
        <$> optional do
          strOption
            ( long "benchmark-dir"
                <> metavar "DIR"
                <> help "Benchmark root directory containing scenarios. Defaults to packaged benchmarks."
            )
        <*> messageFormatParser
        <*> optional do
          scriptFileOption "payout-script-file" "Path to the payout script `.plutus` file. If omitted, the devel script is complied on the fly and used."
        <*> optional do
          strOption
            ( long "scenario-root-dir"
                <> metavar "DIR"
                <> help "Root directory for scenario file paths. Defaults to the benchmark root directory."
            )
        <*> optional do
          scriptFileOption "semantics-script-file" "Path to the semantics script `.plutus` file. If omitted, the devel script is complied on the fly and used."
    )
    (progDesc "Run Marlowe benchmarks from a benchmark root directory.")

data BenchmarkGenerateCommand = BenchmarkGenerateCommand
  { scenarioDir :: Maybe FilePath
  , scenarioType :: ScenarioType
  , maxScenarios :: Maybe Int
  }

generateCommandParser :: ParserInfo BenchmarkGenerateCommand
generateCommandParser =
  info
    ( BenchmarkGenerateCommand
        <$> optional do
              strOption
                ( long "scenario-dir"
                    <> metavar "DIR"
                    <> help "Directory to write generated benchmark scenarios to. Defaults to a `scenarios` subdirectory of the packaged benchmark directory."
                )
        <*> option readScenarioType
          ( long "scenario-type"
              <> metavar "golden|random"
              <> value AllScenarios
              <> showDefault
              <> help "Type of scenarios to generate."
          )
        <*> optional do
              option auto
                ( long "max-scenarios"
                    <> metavar "INT"
                    <> help "Maximum number of scenarios to generate."
                )
    )
    (progDesc "Generate Marlowe benchmark scenarios.")

readScenarioType :: ReadM ScenarioType
readScenarioType = eitherReader $ \case
  "golden" -> Right GoldenScenarios
  "random" -> Right RandomScenarios
  other -> Left $ "Unknown scenario type: " <> other <> ". Expected one of: golden, random."

runRunCommand :: BenchmarkRunCommand -> IO ()
runRunCommand cmd = do
  dataDir <- getDataDir
  (semScript, payScript) <- case (cmd.semanticsScriptFile, cmd.payoutScriptFile) of
    (Nothing, Nothing) -> do
      input <- LBS8.getContents
      case A.eitherDecode input of
        Left err -> die $ "Failed to parse CompileResponse from stdin: " <> err
        Right (ScriptSuite { marloweSemantics, marloweRolePayout }) -> do
          pure (marloweSemantics.scriptFile, marloweRolePayout.scriptFile)
    (Just s, Just p) -> pure (s, p)
    (_, _) -> die "Both semantics and payout script files must be provided, or neither to read them from stdin."
  benchmarkRootDir <- case cmd.benchmarkRootDir of
    Just dir -> pure dir
    Nothing -> pure $ dataDir </> "benchmarks"
  scenarioRootDir <- case cmd.scenarioRootDir of
    Just dir -> pure dir
    Nothing -> pure $ benchmarkRootDir </> "scenarios"
  let
    request =
        BenchmarkRequest
          { benchmarkRootDir
          , scenarioRootDir
          , semanticsScriptFile = semScript
          , payoutScriptFile = payScript
          }
  runExceptT (benchmarkScripts request) >>= \case
    Left err -> emitBenchmarkError cmd.messageFormat err
    Right response -> emitBenchmarkSummary cmd.messageFormat response

runGenerateCommand :: BenchmarkGenerateCommand -> IO ()
runGenerateCommand BenchmarkGenerateCommand{scenarioDir, scenarioType, maxScenarios} = do
  dataDir <- getDataDir
  let
    rootDir = fromMaybe (dataDir </> "benchmarks") scenarioDir
  result <- runExceptT $ generateBenchmarks
    (BenchmarkGenerateRequest rootDir scenarioType maxScenarios)
  case result of
    Left err -> emitBenchmarkError MessageFormatText err
    Right response -> emitGenerateSummary MessageFormatText response

emitBenchmarkSummary :: MessageFormat -> BenchmarkResponse -> IO ()
emitBenchmarkSummary messageFormat response@BenchmarkResponse{benchmarkResults} =
  case messageFormat of
    MessageFormatText -> mapM_ printResult benchmarkResults
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty response
    MessageFormatYaml -> BS8.putStrLn $ Y.encode response
 where
  printResult :: BenchmarkCaseResult -> IO ()
  printResult result = do
    putStrLn $ "scenario: " <> (T.unpack $ scenarioId2Hex result.scenarioId)
    case result.result of
      Left err -> do
        putStrLn $ "  error: " <> case err of
          OtherError msg -> msg
          EvaluationError msg logs -> msg <> "\n    logs: " <> show logs
      Right (usage, logs) -> do
        putStrLn $ "  cpu: " <> show usage.cpu
        putStrLn $ "  memory: " <> show usage.memory
        putStrLn $ "  logs: " <> show logs
    putStrLn ""

emitGenerateSummary :: MessageFormat -> BenchmarkGenerateResponse -> IO ()
emitGenerateSummary messageFormat response@BenchmarkGenerateResponse{generatedBenchmarkSuites} =
  case messageFormat of
    MessageFormatText -> mapM_ printSuite generatedBenchmarkSuites
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty response
    MessageFormatYaml -> BS8.putStrLn $ Y.encode response
 where
  printSuite BenchmarkGeneratedSuite{generatedScenarioDir, generatedScenarios} = do
    putStrLn $ "directory: " <> generatedScenarioDir
    putStrLn $ "  scenarios: " <> show (length generatedScenarios)

emitBenchmarkError :: MessageFormat -> String -> IO a
emitBenchmarkError messageFormat err =
  case messageFormat of
    MessageFormatText -> die err
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty $ A.toJSON err) >> die "benchmark command failed"
    MessageFormatYaml -> BS8.putStrLn (Y.encode err) >> die "benchmark command failed"

scriptFileOption :: String -> String -> Parser FilePath
scriptFileOption name description =
  strOption
    ( long name
        <> metavar "FILE"
        <> help description
    )
