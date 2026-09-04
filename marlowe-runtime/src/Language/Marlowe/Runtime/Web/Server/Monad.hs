{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Defines a custom Monad for the web server's handler functions to run in.
module Language.Marlowe.Runtime.Web.Server.Monad (
  ApplyInputs,
  ContractStore,
  GetContractSource,
  ImportBundle,
  InitContract,
  LoadTxError (..),
  ServerDependencies (..),
  ServerM (..),
  WithBundleImporter,
  applyInputsL,
  burnRoleTokensL,
  getContractSourceL,
  withBundleImporterL,
  initContractL,
  loadContractL,
  loadPayoutL,
  loadPayoutsL,
  loadTransactionL,
  loadTransactionsL,
  loadWithdrawalL,
  loadWithdrawalsL,
  withdrawL,
  runEff0,
  runEff1,
  runEff2,
  runEff3,
  runEff4,
  runServer,
  runServerMExtract,
) where

import qualified Language.Marlowe.Runtime.Web.Contract.API as Web
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, StakeCredential, TokenName, TxId, TxOutRef)
import Language.Marlowe.Object.Types (ContractHash)
import Language.Marlowe.Runtime.Transaction.Api
    ( Accounts,
      ContractInitializedInEra,
      RoleTokenFilter,
      RoleTokensConfig,
      BurnRoleTokensError,
      BurnRoleTokensTx,
      WithdrawTx,
      InitError,
      ApplyInputsError,
      InputsApplied,
      WithdrawError,
      ContractInitialized )
import Language.Marlowe.Runtime.Web.Adapter.Control.Lens (makeSuffixedLenses)

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Runtime.Core.Api
    ( Contract,
      ContractId,
      MarloweTransactionMetadata,
      Transaction,
      Inputs,
      MarloweVersionTag(V1) )
import Language.Marlowe.Runtime.Query (SomeContractState, TempTx)
import qualified Language.Marlowe.Runtime.Query as Query
import Log (LogT, MonadLog, Logger, LogLevel)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Lens (Getter, view)
import Log.Monad (runLogT)
import Data.Time (UTCTime)
import Data.Set (Set)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Language.Marlowe.Runtime.Contract.TransferServer (ImportBundle)

-- | Our monad stack is not fully compatible with Servant's `Handler` as we want to avoid
-- `ExceptT` (which is part of the `Handler`). We avoid `ExceptT` because it doesn't
-- play nicely with `MonadUnliftIO`. See https://harporoeder.com/posts/servant-13-reader-io/
-- plus some comments here: https://github.com/haskell-servant/servant/issues/950
-- In other words we use IO exceptions all the way down and catch them during the final
-- server hoisting.
newtype ServerM a = ServerM {runServerM :: LogT (ReaderT (ServerDependencies ServerM) IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog
    , MonadIO
    , MonadReader (ServerDependencies ServerM)
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    -- , MonadError ServerError
    , MonadBase IO
    , MonadUnliftIO
    )

runServer :: Logger -> LogLevel -> ServerDependencies ServerM -> ServerM a -> IO a
runServer logger logLevel deps server =
  flip runReaderT deps $ runLogT "marlowe-runtime-server" logger logLevel $ runServerM do
    server

-- | Run a `ServerM` action and extract the result. Used to bootstrap
-- construction of `ServerDependencies ServerM` from inside `IO`. The
-- `Logger` and `LogLevel` are placeholders; the actual server is started
-- later via `runServer` with the real logger.
runServerMExtract
  :: ServerDependencies ServerM
  -> ServerM a
  -> IO a
runServerMExtract deps server =
  flip runReaderT deps $ runLogT "marlowe-runtime-server" undefined undefined $ runServerM do
    server

runEff0
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (m a)
  -> t m a
runEff0 l = do
  fn <- lift $ view l
  lift fn

runEff1
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> m b)
  -> a
  -> t m b
runEff1 l a = do
  fn <- lift $ view l
  lift $ fn a

runEff2
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> m c)
  -> a
  -> b
  -> t m c
runEff2 l a b = do
  fn <- lift $ view l
  lift $ fn a b

runEff3
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> c -> m d)
  -> a
  -> b
  -> c
  -> t m d
runEff3 l a b c = do
  fn <- lift $ view l
  lift $ fn a b c

runEff4
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> c -> d -> m e)
  -> a
  -> b
  -> c
  -> d
  -> t m e
runEff4 l a b c d = do
  fn <- lift $ view l
  lift $ fn a b c d

-- FIXME: To speed up the migration I hard coded `V1` below.
-- We should probably migrate the Api so it is working in most cases
-- mostly with existential wrappers.
type ApplyInputs m =
  WalletContext
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs V1
  -> m (Either ApplyInputsError (InputsApplied V1))

type BurnRoleTokens m =
  WalletContext
  -> RoleTokenFilter
  -> m (Either BurnRoleTokensError (BurnRoleTokensTx V1))

type WithBundleImporter m a = (ImportBundle m -> m a) -> m a

-- | A thin adapter around `ContractStore.getContract` that returns a
-- `ContractWithAdjacency` with `DatumHash`es instead of `ContractHash`es.
-- This is the read side of the merkleized contract store, used by the
-- (not-yet-implemented) `GET /contracts/sources/{id}` endpoints.
type GetContractSource m = Web.ContractSourceId -> m (Maybe (ContractWithAdjacency 'V1))

type InitContract m =
  Maybe StakeCredential
  -> WalletContext
  -> Maybe TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Lovelace
  -> Accounts
  -> Either (Contract V1) ContractHash
  -> m (Either InitError (ContractInitialized V1))

type LoadContract m =
  ContractId
  -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractInitializedInEra) SomeContractState))
  -- ^ Nothing if the ID is not found

-- | Signature for a delegate that loads a list of payouts.
type LoadPayouts m =
  Query.PayoutFilter
  -> Query.Range TxOutRef
  -> m (Maybe (Query.Page TxOutRef Query.PayoutHeader))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads a single payout.
type LoadPayout m =
  TxOutRef
  -> m (Maybe Query.SomePayoutState)

data LoadTxError
  = ContractNotFound
  | TxNotFound

type LoadTransactions m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> Query.Range TxId
  -- ^ The range of transactions to load.
  -> m (Either LoadTxError (Query.Page TxId (Transaction 'V1)))

-- | Signature for a delegate that loads a transaction for a contract.
type LoadTransaction m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> TxId
  -- ^ ID of the transaction to load.
  -> m (Maybe Query.SomeTransaction)

-- | Signature for a delegate that loads a list of withdrawals.
type LoadWithdrawals m =
  Query.WithdrawalFilter
  -> Query.Range TxId
  -> m (Maybe (Query.Page TxId Query.Withdrawal))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads the state of a single withdrawal.
type LoadWithdrawal m =
  TxId
  -- ^ ID of the contract to load
  -> m (Maybe Query.Withdrawal)
  -- ^ Nothing if the ID is not found

type Withdraw m =
  WalletContext
  -> Set TxOutRef
  -> m (Either WithdrawError (WithdrawTx V1))

data ServerDependencies m = ServerDependencies
  { applyInputs :: ApplyInputs m
  , burnRoleTokens :: BurnRoleTokens m
  , initContract :: InitContract m
  , getContractSource :: GetContractSource m
  , loadContract :: LoadContract m
  , loadPayout :: LoadPayout m
  , loadPayouts :: LoadPayouts m
  , loadTransaction :: LoadTransaction m
  , loadTransactions :: LoadTransactions m
  , loadWithdrawal :: LoadWithdrawal m
  , loadWithdrawals :: LoadWithdrawals m
  , withdraw :: Withdraw m
  , withBundleImporter :: forall a. WithBundleImporter m a
  }

makeSuffixedLenses ''ServerDependencies

