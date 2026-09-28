module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions qualified as TS
import Language.Marlowe.Runtime.Indexer.Database qualified as DB
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks qualified as CommitBlocks
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitEraHistory qualified as CommitEraHistory
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitIndexerTip qualified as CommitIndexerTip
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitNodeTip qualified as CommitNodeTip
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback qualified as CommitRollback
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints qualified as GetIntersectionPoints
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO qualified as GetMarloweUTxO
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetStatus qualified as GetStatus
import UnliftIO (throwIO)

databaseQueries :: MonadIO m => Pool-> DB.DatabaseQueries m
databaseQueries pool =
  DB.DatabaseQueries
    { commitRollback = \point -> runTransaction pool $ CommitRollback.commitRollback point
    , commitBlocks = \blocks -> runTransaction pool $ CommitBlocks.commitBlocks blocks
    , commitEraHistory = \eraHistory -> runTransaction pool $ CommitEraHistory.commitEraHistory eraHistory
    , commitIndexerTip = \tip -> runTransaction pool $ CommitIndexerTip.commitIndexerTip tip
    , commitNodeTip = \tip -> runTransaction pool $ CommitNodeTip.commitNodeTip tip
    , getIntersectionPoints = \securityParameter -> runTransaction pool $ GetIntersectionPoints.getIntersectionPoints securityParameter
    , getLatestMarloweUTxO = runTransaction pool GetMarloweUTxO.getLatestMarloweUTxO
    , getMarloweUTxO = \slotNo -> runTransaction pool $ GetMarloweUTxO.getMarloweUTxO slotNo
    , getStatus = runTransaction pool GetStatus.getStatus
    }

runTransaction :: MonadIO m => Pool -> Transaction a -> m a
runTransaction pool transaction = do
  result <- liftIO $ Pool.use pool $ TS.transaction TS.Serializable TS.Write transaction
  case result of
    Left ex -> throwIO ex
    Right a -> pure a
