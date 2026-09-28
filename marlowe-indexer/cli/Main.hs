module Main where

import Control.Monad (join)
import Options (options)
import Options.Applicative (execParser)

main :: IO ()
main = join (execParser options)