module Commands.Status where

import qualified Cardano.Api as C
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Hasql.Connection.Settings as Hasql
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as Hasql
import qualified Language.Marlowe.Runtime.Indexer.Database as DB
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetStatus
  ( EraHistoryStatus (..)
  , IndexerStatus (..)
  , NodeStatus (..)
  , ProtocolParametersStatus (..)
  , Status (..)
  )
import Language.Marlowe.Runtime.ChainSync.Api
  ( BlockHeader (..)
  , ChainTip (..)
  , IndexerTip (..)
  , NodeTip (..)
  , unBlockNo
  , unSlotNo
  )
import qualified Options.Applicative as O

import Commands.Options.Database (databaseUriParser)
import Commands.Options.MessageFormat (MessageFormat, emitJSONError, emitResponseWithTyped, messageFormatParser)

data StatusCommand = StatusCommand
  { statusDatabaseUri :: Hasql.Settings
  , statusMessageFormat :: MessageFormat
  }

mkStatusCommandParser :: O.ParserInfo StatusCommand
mkStatusCommandParser = O.info parser (O.progDesc "Print the indexer's recorded status of the local cardano-node and the indexer's own progress.")
 where
  parser =
    StatusCommand
      <$> databaseUriParser
      <*> messageFormatParser

runStatusCommand :: StatusCommand -> IO ()
runStatusCommand cmd = do
  let
    cfg = Hasql.settings [Hasql.staticConnectionSettings cmd.statusDatabaseUri]
  pool <- Pool.acquire cfg
  let
    queries = PostgreSQL.databaseQueries pool :: DB.DatabaseQueries IO
  status <- queries.getStatus
  case status of
    Left decodeErr -> emitJSONError cmd.statusMessageFormat (A.toJSON decodeErr)
    Right r -> emitResponseWithTyped cmd.statusMessageFormat r renderStatusText

renderStatusText :: Status -> BS.ByteString
renderStatusText Status{statusNode, statusIndexer} = BS8.unlines $
  [ "Marlowe indexer status:"
  , ""
  , "Node:"
  ] ++ renderNodeStatus statusNode ++
  [ ""
  , "Indexer:"
  ] ++ renderIndexerStatus statusIndexer

renderNodeStatus :: NodeStatus -> [BS.ByteString]
renderNodeStatus NodeStatus{nodeTip, nodeEraHistory, nodeNetworkId, nodeSystemStart, nodeProtocolParameters} =
  [ "  tip:                  " <> renderNodeTipText nodeTip
  , "  eraHistory:           " <> renderEraHistoryText nodeEraHistory
  , "  networkId:            " <> renderNetworkIdText nodeNetworkId
  , "  systemStart:          " <> renderSystemStartText nodeSystemStart
  , "  protocolParameters:   " <> renderProtocolParametersText nodeProtocolParameters
  ]

renderIndexerStatus :: IndexerStatus -> [BS.ByteString]
renderIndexerStatus IndexerStatus{indexerTip} =
  [ "  tip:         " <> renderIndexerTipText indexerTip
  ]

renderNodeTipText :: Maybe NodeTip -> BS.ByteString
renderNodeTipText = maybe "(not set)" renderChainTip . fmap (\(NodeTip t) -> t)

renderIndexerTipText :: Maybe IndexerTip -> BS.ByteString
renderIndexerTipText = maybe "(not set)" renderChainTip . fmap (\(IndexerTip t) -> t)

renderEraHistoryText :: Maybe EraHistoryStatus -> BS.ByteString
renderEraHistoryText = maybe "(not set)" \EraHistoryStatus{eraHistoryBytes} ->
  BS8.pack $ show eraHistoryBytes <> " bytes"

renderNetworkIdText :: Maybe C.NetworkId -> BS.ByteString
renderNetworkIdText = maybe "(not set)" \case
  C.Mainnet -> "mainnet"
  C.Testnet (C.NetworkMagic m) -> BS8.pack $ "testnet " <> show m

renderSystemStartText :: Maybe C.SystemStart -> BS.ByteString
renderSystemStartText = maybe "(not set)" \ss -> BS8.pack $ show ss

renderProtocolParametersText :: Maybe ProtocolParametersStatus -> BS.ByteString
renderProtocolParametersText = maybe "(not set)" \ProtocolParametersStatus{protocolParametersBytes} ->
  BS8.pack $ show protocolParametersBytes <> " bytes"

renderChainTip :: ChainTip -> BS.ByteString
renderChainTip ChainTip{blockHeader = Nothing} = "(genesis)"
renderChainTip ChainTip{blockHeader = Just BlockHeader{slotNo, headerHash, blockNo}} =
  BS8.pack $
    "slot=" <> show (unSlotNo slotNo)
      <> " block=" <> show (unBlockNo blockNo)
      <> " hash=" <> show headerHash