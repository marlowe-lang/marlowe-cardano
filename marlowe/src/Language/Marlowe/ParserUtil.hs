module Language.Marlowe.ParserUtil where

import qualified Data.Aeson as JSON
import Data.Aeson.Types hiding (Error, Value)
import Data.Scientific (Scientific, floatingOrInteger)

getInteger :: String -> Scientific -> Parser Integer
getInteger ctx x = case (floatingOrInteger x :: Either Double Integer) of
  Right a -> return a
  Left _ -> fail $ "parsing " ++ ctx ++ " failed, expected integer, but encountered floating point"

withInteger :: String -> JSON.Value -> Parser Integer
withInteger ctx = withScientific ctx $ getInteger ctx
