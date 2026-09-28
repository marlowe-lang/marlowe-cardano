module Commands.Options.Database where

import qualified Data.Text as T
import qualified Hasql.Connection.Settings as Hasql
import Options.Applicative (Parser, help, long, metavar, option, ReadM)
import qualified Options.Applicative as O

databaseUriParser :: Parser Hasql.Settings
databaseUriParser = do
  let
    readSettings :: ReadM Hasql.Settings
    readSettings = O.eitherReader \s -> do
      let
        settings = Hasql.connectionString (T.pack s)
      pure settings
  option readSettings
    ( long "database-uri"
        <> metavar "DATABASE_URI"
        <> help "URI of the indexer database."
    )