module Language.Marlowe.Runtime.CLI.Command.Query where

import qualified Cardano.Api as C
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe.Class (runMarloweQueryClient)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.Marlowe.Protocol.Query.Client (getStatus)
import Language.Marlowe.Protocol.Query.Types (RuntimeStatus (..))
import Language.Marlowe.Runtime.CLI.Command.Query.Store
import Language.Marlowe.Runtime.CLI.Monad
import Options.Applicative

data QueryCommand
  = StoreQuery StoreQueryCommand
  | Status

queryCommandParser :: ParserInfo QueryCommand
queryCommandParser = info parser $ progDesc "Query the runtime"
  where
    parser =
      hsubparser $
        mconcat
          [ command "store" $ StoreQuery <$> storeQueryCommandParser
          , command "status" $ info (pure Status) $ progDesc "Query the status of the runtime"
          ]

runtimeStatusToJSON :: RuntimeStatus -> A.Value
runtimeStatusToJSON RuntimeStatus{..} = do
  let networkIdJson = case networkId of
        C.Mainnet -> A.String "mainnet"
        C.Testnet (C.NetworkMagic networkMagic) ->
          A.object
            ["testnet" A..= networkMagic]
  A.object
    [ "nodeTip" A..= nodeTip
    , "nodeTipUTC" A..= nodeTipUTC
    , "runtimeChainTip" A..= runtimeChainTip
    , "runtimeChainTipUTC" A..= runtimeChainTipUTC
    , "runtimeTip" A..= runtimeTip
    , "runtimeTipUTC" A..= runtimeTipUTC
    , "networkId" A..= networkIdJson
    , "runtimeVersion" A..= runtimeVersion
    ]

runQueryCommand :: QueryCommand -> CLI ()
runQueryCommand = \case
  StoreQuery cmd -> runStoreQueryCommand cmd
  Status -> do
    runtimeStatus <- runMarloweQueryClient getStatus
    let prettyJSON :: A.Value -> T.Text
        prettyJSON = T.decodeUtf8 . LBS.toStrict . A.encodePretty
    liftIO . T.putStrLn . prettyJSON . runtimeStatusToJSON $ runtimeStatus
    pure ()
