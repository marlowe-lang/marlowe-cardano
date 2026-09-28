{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetStatus where

import qualified Cardano.Api as C
import qualified Data.Aeson as A
import Data.Binary (get)
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api (IndexerTip (..), NodeTip (..))

data StatusRecord = StatusRecord
  { statusRecordNode :: !(Map.Map T.Text BS.ByteString)
  , statusRecordIndexer :: !(Map.Map T.Text BS.ByteString)
  }

data Status = Status
  { statusNode :: !NodeStatus
  , statusIndexer :: !IndexerStatus
  }
  deriving stock (Show)

data NodeStatus = NodeStatus
  { nodeTip :: Maybe NodeTip
  , nodeEraHistory :: Maybe EraHistoryStatus
  }
  deriving stock (Show)

data IndexerStatus = IndexerStatus
  { indexerTip :: Maybe IndexerTip
  }
  deriving stock (Show)

data EraHistoryStatus = EraHistoryStatus
  { eraHistoryBytes :: Int
  }
  deriving stock (Show)

newtype DecodeError = DecodeError String
  deriving stock (Show)

instance A.ToJSON Status where
  toJSON Status{statusNode, statusIndexer} = A.object
    [ "node" A..= statusNode
    , "indexer" A..= statusIndexer
    ]

instance A.ToJSON NodeStatus where
  toJSON NodeStatus{nodeTip, nodeEraHistory} = A.object
    [ "tip" A..= nodeTip
    , "eraHistory" A..= nodeEraHistory
    ]

instance A.ToJSON IndexerStatus where
  toJSON IndexerStatus{indexerTip} = A.object
    [ "tip" A..= indexerTip
    ]

instance A.ToJSON EraHistoryStatus where
  toJSON EraHistoryStatus{eraHistoryBytes} = A.object
    [ "bytes" A..= eraHistoryBytes
    ]

instance A.ToJSON DecodeError where
  toJSON (DecodeError s) = A.object
    [ "error" A..= ("DecodeError" :: String)
    , "details" A..= s
    ]

getStatus :: H.Transaction (Either DecodeError Status)
getStatus = do
  nodeRows <- V.toList
    <$> H.statement
      ()
      [vectorStatement|
        SELECT attr :: text, value :: bytea
        FROM marlowe.node_status
      |]
  indexerRows <- V.toList
    <$> H.statement
      ()
      [vectorStatement|
        SELECT attr :: text, value :: bytea
        FROM marlowe.indexer_status
      |]
  pure $ buildStatus
    StatusRecord
      { statusRecordNode = Map.fromList nodeRows
      , statusRecordIndexer = Map.fromList indexerRows
      }

buildStatus :: StatusRecord -> Either DecodeError Status
buildStatus StatusRecord{statusRecordNode, statusRecordIndexer} = do
  nodeStatus <- NodeStatus
    <$> decodeNodeTip (Map.lookup "tip" statusRecordNode)
    <*> decodeEraHistory (Map.lookup "eraHistory" statusRecordNode)
  indexerStatus <- IndexerStatus
    <$> decodeIndexerTip (Map.lookup "tip" statusRecordIndexer)
  pure Status
    { statusNode = nodeStatus
    , statusIndexer = indexerStatus
    }

decodeNodeTip :: Maybe BS.ByteString -> Either DecodeError (Maybe NodeTip)
decodeNodeTip = traverse decodeNodeTipBytes

decodeNodeTipBytes :: BS.ByteString -> Either DecodeError NodeTip
decodeNodeTipBytes bs =
  case Binary.runGetOrFail get (LBS.fromStrict bs) of
    Left (_, _, err) -> Left $ DecodeError $ "Failed to decode node tip: " <> err
    Right (_, _, tip) -> Right tip

decodeIndexerTip :: Maybe BS.ByteString -> Either DecodeError (Maybe IndexerTip)
decodeIndexerTip = traverse decodeIndexerTipBytes

decodeIndexerTipBytes :: BS.ByteString -> Either DecodeError IndexerTip
decodeIndexerTipBytes bs =
  case Binary.runGetOrFail get (LBS.fromStrict bs) of
    Left (_, _, err) -> Left $ DecodeError $ "Failed to decode indexer tip: " <> err
    Right (_, _, tip) -> Right tip

decodeEraHistory :: Maybe BS.ByteString -> Either DecodeError (Maybe EraHistoryStatus)
decodeEraHistory = traverse \bs ->
  case C.deserialiseFromCBOR (C.proxyToAsType (Proxy :: Proxy C.EraHistory)) bs of
    Left err -> Left $ DecodeError $ "Failed to decode era history: " <> show err
    Right _ -> Right $ EraHistoryStatus (BS.length bs)