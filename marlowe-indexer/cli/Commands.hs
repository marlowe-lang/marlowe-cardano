module Commands where

import Commands.Status (mkStatusCommandParser, runStatusCommand)
import Data.Foldable (fold)
import Options.Applicative (Parser, command, hsubparser)

mkCommandParser :: Parser (IO ())
mkCommandParser =
  hsubparser $
    fold
      [ command "status" $ runStatusCommand <$> mkStatusCommandParser
      ]