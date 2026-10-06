module Commands where

import Commands.Benchmark (benchmarkCommandParser)
import Commands.Blueprint (blueprintCommandParser, runBlueprintCommand)
import Commands.Compile (compileCommandParser, runCompileCommand)
import Data.Foldable (fold)
import Options.Applicative (Parser, command, hsubparser)

commandParser :: Parser (IO ())
commandParser =
  hsubparser $
    fold
      [ command "benchmark" benchmarkCommandParser
      , command "blueprint" $ runBlueprintCommand <$> blueprintCommandParser
      , command "compile" $ runCompileCommand <$> compileCommandParser
      ]
