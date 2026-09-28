module Options where

import Commands (mkCommandParser)
import Data.Foldable (fold)
import Data.Version (showVersion)
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  progDesc,
  short,
 )
import Paths_marlowe_indexer (version)

options :: ParserInfo (IO ())
options = info parser description

parser :: Parser (IO ())
parser = helper <*> versionOption <*> mkCommandParser

versionOption :: Parser (a -> a)
versionOption =
  infoOption ("marlowe-indexer-cli " <> showVersion version) $
    long "version" <> short 'v' <> help "Show version."

description :: InfoMod (IO ())
description =
  fold
    [ fullDesc
    , progDesc "Command-line utilities for inspecting the Marlowe indexer."
    , header "marlowe-indexer-cli: inspect Marlowe indexer state."
    ]