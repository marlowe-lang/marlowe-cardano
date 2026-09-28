-- FIXME: Drop this when the porting is done.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Runtime.Web.Client (
  Page (..),
  -- getContractSource,
  -- getContractSourceAdjacency,
  -- getContractSourceClosure,
  -- getContracts,
  -- getPayout,
  -- getPayouts,
  -- getWithdrawal,
  -- getWithdrawals,
  -- healthcheck,
  getContract,
  getContractNext,
  getContractTransactions,
  postContract,
  postTransaction,
  -- postContractCreateTx,
  postContractSource,
  getContractSource,
  getContractSourceAdjacency,
  getContractSourceClosure,
  -- postWithdrawal,
  -- postWithdrawalCreateTx,
  -- putContract,
  -- putWithdrawal,
) where

import Cardano.Api qualified as C
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Aeson qualified as A
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Version (Version)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Marlowe.Plutus.Next (Next)
import Language.Marlowe.Runtime.Web.API (RuntimeAPI, runtimeApi)
import Language.Marlowe.Runtime.Web.Adapter.CommaList ( CommaList (CommaList),)
import Language.Marlowe.Runtime.Web.Adapter.Links (retractLink)
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))
import Language.Marlowe.Runtime.Web.Contract.API ( ContractHeader, ContractSourceId, ContractState, GetContractsResponse, PostContractSourceResponse, PostContractsRequest, PostContractsResponse, GetContractResponse, ContractId, PreserveActions (PreserveActions))
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (GetTransactionsResponse, PostTransactionsRequest(PostTransactionsRequest), PostTransactionsResponse)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedResponse)
import Language.Marlowe.Runtime.Web.Core.Address ( Address, StakeAddress,)
import Language.Marlowe.Runtime.Web.Core.Asset ( AssetId, PolicyId,)
import Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId)
import Language.Marlowe.Runtime.Web.Core.Object.Schema ()
import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Language.Marlowe.Runtime.Web.Core.Tip (ChainTip)
import Language.Marlowe.Runtime.Web.Core.Tx ( TextEnvelope, TxId, TxOutRef,)
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Language.Marlowe.Runtime.Web.Payout.API ( GetPayoutsResponse, PayoutHeader, PayoutState, PayoutStatus,)
import Language.Marlowe.Runtime.Web.Tx.API ( CardanoTx, CreateTxEnvelope, TxHeader, WithdrawTxEnvelope, ApplyInputsTxEnvelope)
import Language.Marlowe.Runtime.Web.Withdrawal.API (GetWithdrawalsResponse, PostWithdrawalsRequest, Withdrawal, WithdrawalHeader,)
import Marlowe.Plutus.Semantics.Types (Contract)
import Pipes (Producer)
import Servant (HasResponseHeader, ResponseHeader (..), getResponse, lookupResponseHeader, type (:<|>) ((:<|>)))
import Servant.API (Headers)
import Servant.Client (Client, matchUnion)
import Servant.Client.Streaming (ClientM)
import Servant.Client.Streaming qualified as ServantStreaming
import Servant.Pagination (ExtractRange (extractRange), HasPagination (..), PutRange (..), Range, Ranges)
import Servant.Pipes ()

runtimeClient :: Client ClientM RuntimeAPI
runtimeClient = ServantStreaming.client runtimeApi

data Page field resource = Page
  { totalCount :: Int
  , nextRange :: Maybe (Range field (RangeType resource field))
  , items :: [resource]
  }
  deriving (Eq, Show)

-- healthcheck :: ClientM Bool
-- healthcheck = do
--   let _ :<|> _ :<|> _ :<|> _ :<|> healthcheck' = runtimeClient
--   (True <$ healthcheck') `catchError` const (pure False)
-- 
-- getContracts
--   :: Maybe (Set PolicyId)
--   -> Maybe (Set Text)
--   -> Maybe (Set Address)
--   -> Maybe (Set AssetId)
--   -> Maybe (Range "contractId" TxOutRef)
--   -> ClientM (Page "contractId" ContractHeader)
-- getContracts roleCurrencies tags partyAddresses partyRoles range = do
--   let contractsClient :<|> _ = runtimeClient
--   let getContracts' :<|> _ = contractsClient
--   response <-
--     getContracts'
--       (foldMap Set.toList roleCurrencies)
--       (foldMap Set.toList tags)
--       (foldMap Set.toList partyAddresses)
--       (foldMap Set.toList partyRoles)
--       (putRange <$> range)
--   totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
--   nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
-- 
--   let ListObject items = getResponse response
--   pure $ Page
--     { totalCount
--     , nextRange = extractRangeSingleton @GetContractsResponse <$> nextRanges
--     , items = retractLink @"contract" . retractLink @"transactions" <$> items
--     }
-- 
-- postContract
--   :: Maybe StakeAddress
--   -> Address
--   -> Maybe (Set Address)
--   -> Maybe (Set TxOutRef)
--   -> PostContractsRequest
--   -> ClientM (CreateTxEnvelope CardanoTxBody)
-- postContract stakeAddress changeAddress otherAddresses collateralUtxos request = do
--   let (_ :<|> getPost :<|> _) :<|> _ = runtimeClient
--   let (postContractCreateTxBody' :<|> _) = getPost stakeAddress
--   response <-
--     postContractCreateTxBody'
--       request
--       changeAddress
--       (setToCommaList <$> otherAddresses)
--       (setToCommaList <$> collateralUtxos)
--   pure $ retractLink $ getResponse response
--
--        "X-Stake-Address"
--  Header' '[Required, Strict] "X-Change-Address" Address
--    :> Header' '[Required, Strict] "X-Wallet-UTxO" (CommaList (TransactionUnspentOutput C.ConwayEra))

postContract
  :: Maybe StakeAddress
  -> Address
  -> [Web.TransactionUnspentOutput C.ConwayEra]
  -> PostContractsRequest
  -> ClientM (CreateTxEnvelope CardanoTx)
postContract stakeAddress changeAddress availableUTxOs request = do
  let (_ :<|> mkPostContract :<|> _) :<|> _ = runtimeClient
  let postContract' = mkPostContract stakeAddress
  union <-
    postContract'
      request
      changeAddress
      (CommaList availableUTxOs)
  case matchUnion union of
    Just (response :: PostContractsResponse CardanoTx) -> pure (retractLink response)
    Nothing -> liftIO $ fail "Unexpected response from postContract"

getContract :: ContractId -> ClientM ContractState
getContract contractId = do
  let (contractClient :<|> _) :<|> _ = runtimeClient
  let getContract' :<|> _ = contractClient contractId
  response <- getContract'
  case matchUnion response of
    Just (contractState :: GetContractResponse) -> pure (retractLink contractState)
    Nothing -> liftIO $ fail "Unexpected response from getContract"

getContractTransactions :: ContractId -> ClientM [TxHeader]
getContractTransactions contractId = do
  let
    (mkContractsClient :<|> _) :<|> _ = runtimeClient
    _ :<|> (_ :<|> transactionsClient) = mkContractsClient contractId
    getTransactionsClient :<|> _ = transactionsClient
  response <- getTransactionsClient Nothing
  case matchUnion response of
    Just (paginatedResponse :: PaginatedResponse '["transactionId"] GetTransactionsResponse) -> do
      let ListObject items = getResponse paginatedResponse
      pure $ retractLink @"transaction" <$> items
    Nothing -> liftIO $ fail "Unexpected response from getContractTransactions"

getContractNext :: ContractId -> UTCTime -> UTCTime -> [Party] -> ClientM Value
getContractNext contractId validityStart validityEnd parties = do
  let (contractApiClient :<|> _) :<|> _ = runtimeClient
  let _ :<|> nextClient :<|> _ = contractApiClient contractId
  union <- nextClient validityStart validityEnd parties
  case matchUnion union of
    Just (nextStep :: Next) -> pure (A.toJSON nextStep)
    Nothing -> liftIO $ fail "Unexpected response from getContractNext"

postTransaction
  :: Address
  -> [Web.TransactionUnspentOutput C.ConwayEra]
  -> ContractId
  -> PostTransactionsRequest
  -> ClientM (ApplyInputsTxEnvelope CardanoTx)
postTransaction changeAddress availableUTxOs contractId request = do
  let
    (mkContractsClient :<|> _) :<|> _ = runtimeClient
    _ :<|> (_ :<|> transactionsClient) = mkContractsClient contractId
    _ :<|> postTransactionClient :<|> _ = transactionsClient

  union <-
    postTransactionClient
      request
      changeAddress
      (CommaList availableUTxOs)
  case matchUnion union of
    Just (response :: PostTransactionsResponse CardanoTx) -> pure (retractLink response)
    Nothing -> liftIO $ fail "Unexpected response from postTransaction"

postContractSource
  :: Set Value
  -> Label
  -> Producer ObjectBundle IO ()
  -> ClientM PostContractSourceResponse
postContractSource preserveActions main bundles = do
  let (_ :<|> _ :<|> sourceClient) :<|> _ = runtimeClient
  let postContractSourceClient :<|> _ = sourceClient
  union <- postContractSourceClient main (Just (PreserveActions preserveActions)) bundles
  case matchUnion union of
    Just (response :: PostContractSourceResponse) -> pure response
    Nothing -> liftIO $ fail "Unexpected response from postContractSource"

getContractSource
  :: ContractSourceId
  -> Bool
  -> ClientM Contract
getContractSource contractSourceId expand = do
  let (_ :<|> _ :<|> sourceClient) :<|> _ = runtimeClient
  let _ :<|> contractSourceClient = sourceClient
  let getContractSource' :<|> _ = contractSourceClient contractSourceId
  union <- getContractSource' expand
  case matchUnion union of
    Just (response :: Contract) -> pure response
    Nothing -> liftIO $ fail "Unexpected response from getContractSource"

getContractSourceAdjacency
  :: ContractSourceId
  -> ClientM (Set ContractSourceId)
getContractSourceAdjacency contractSourceId = do
  let (_ :<|> _ :<|> sourceClient) :<|> _ = runtimeClient
  let _ :<|> contractSourceClient = sourceClient
  let _ :<|> adjacencyClient :<|> _ = contractSourceClient contractSourceId
  union <- adjacencyClient
  case matchUnion union of
    Just (response :: ListObject ContractSourceId) ->
      pure $ Set.fromList $ results response
    Nothing -> liftIO $ fail "Unexpected response from getContractSourceAdjacency"

getContractSourceClosure
  :: ContractSourceId
  -> ClientM (Set ContractSourceId)
getContractSourceClosure contractSourceId = do
  let (_ :<|> _ :<|> sourceClient) :<|> _ = runtimeClient
  let _ :<|> contractSourceClient = sourceClient
  let _ :<|> _ :<|> closureClient = contractSourceClient contractSourceId
  union <- closureClient
  case matchUnion union of
    Just (response :: ListObject ContractSourceId) ->
      pure $ Set.fromList $ results response
    Nothing -> liftIO $ fail "Unexpected response from getContractSourceClosure"


-- getWithdrawals
--   :: Maybe (Set PolicyId)
--   -> Maybe (Range "withdrawalId" TxId)
--   -> ClientM (Page "withdrawalId" WithdrawalHeader)
-- getWithdrawals roleCurrencies range = do
--   let _ :<|> withdrawalsClient :<|> _ = runtimeClient
--   let getWithdrawals' :<|> _ = withdrawalsClient
--   response <- getWithdrawals' (foldMap Set.toList roleCurrencies) $ putRange <$> range
--   totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
--   nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
--   let ListObject items = getResponse response
--   pure $ Page
--     { totalCount
--     , nextRange = extractRangeSingleton @GetWithdrawalsResponse <$> nextRanges
--     , items = retractLink @"withdrawal" <$> items
--     }
-- 
-- postWithdrawal
--   :: Address
--   -> Maybe (Set Address)
--   -> Maybe (Set TxOutRef)
--   -> PostWithdrawalsRequest
--   -> ClientM (WithdrawTxEnvelope CardanoTxBody)
-- postWithdrawal changeAddress otherAddresses collateralUtxos request = do
--   let _ :<|> withdrawalsClient :<|> _ = runtimeClient
--   let _ :<|> (postWithdrawal' :<|> _) :<|> _ = withdrawalsClient
--   response <-
--     postWithdrawal'
--       request
--       changeAddress
--       (setToCommaList <$> otherAddresses)
--       (setToCommaList <$> collateralUtxos)
--   pure . retractLink . getResponse $ response
-- 
-- postWithdrawalCreateTx
--   :: Address
--   -> Maybe (Set Address)
--   -> Maybe (Set TxOutRef)
--   -> PostWithdrawalsRequest
--   -> ClientM (WithdrawTxEnvelope CardanoTx)
-- postWithdrawalCreateTx changeAddress otherAddresses collateralUtxos request = do
--   let _ :<|> withdrawalsClient :<|> _ = runtimeClient
--   let _ :<|> (_ :<|> postWithdrawalCreateTx') :<|> _ = withdrawalsClient
--   response <-
--     postWithdrawalCreateTx'
--       request
--       changeAddress
--       (setToCommaList <$> otherAddresses)
--       (setToCommaList <$> collateralUtxos)
--   pure . retractLink . getResponse $ response
-- 
-- getWithdrawal :: TxId -> ClientM Withdrawal
-- getWithdrawal withdrawalId = do
--   let _ :<|> withdrawalsClient :<|> _ = runtimeClient
--   let _ :<|> _ :<|> contractApi = withdrawalsClient
--   let getWithdrawal' :<|> _ = contractApi withdrawalId
--   response <- getWithdrawal'
--   pure . getResponse $ response
-- 
-- putWithdrawal :: TxId -> TextEnvelope -> ClientM ()
-- putWithdrawal withdrawalId tx = do
--   let _ :<|> withdrawalsClient :<|> _ = runtimeClient
--   let _ :<|> _ :<|> contractApi = withdrawalsClient
--   let _ :<|> putWithdrawal' = contractApi withdrawalId
--   _ <- putWithdrawal' tx
--   pure ()
-- 
-- getPayouts
--   :: Maybe (Set TxOutRef)
--   -> Maybe (Set AssetId)
--   -> Maybe PayoutStatus
--   -> Maybe (Range "payoutId" TxOutRef)
--   -> ClientM (Page "payoutId" PayoutHeader)
-- getPayouts contractIds roleTokens unclaimed range = do
--   let _ :<|> _ :<|> payoutsClient :<|> _ = runtimeClient
--   let getPayouts' :<|> _ = payoutsClient
--   response <-
--     getPayouts' (foldMap Set.toList contractIds) (foldMap Set.toList roleTokens) unclaimed $
--       putRange <$> range
--   totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
--   nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
--   let ListObject items = getResponse response
--   pure $ Page
--     { totalCount
--     , nextRange = extractRangeSingleton @GetPayoutsResponse <$> nextRanges
--     , items = retractLink @"payout" <$> items
--     }
-- 
-- getPayout
--   :: TxOutRef
--   -> ClientM PayoutState
-- getPayout payoutId = do
--   let _ :<|> _ :<|> payoutsClient :<|> _ = runtimeClient
--   let _ :<|> getPayout' = payoutsClient
--   response <- getPayout' payoutId
--   pure . retractLink . retractLink . retractLink . getResponse $ response
-- 
-- getContractNext :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ClientM Next
-- getContractNext contractId validityStart validityEnd parties = do
--   let contractsClient :<|> _ = runtimeClient
--   let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
--   let _ :<|> _ :<|> next' :<|> _ = contractApi contractId
--   response <- next' validityStart validityEnd parties
--   pure . getResponse $ response
-- 
-- setToCommaList :: Set a -> CommaList a
-- setToCommaList = CommaList . Set.toList
-- 
-- reqHeaderValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM a
-- reqHeaderValue = \case
--   Header a -> pure a
--   UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
--   MissingHeader -> liftIO $ fail $ "Required header missing " <> symbolVal (Proxy @name)
-- 
-- headerValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM (Maybe a)
-- headerValue = \case
--   Header a -> pure $ Just a
--   UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
--   MissingHeader -> pure Nothing
-- 
-- extractRangeSingleton
--   :: (HasPagination resource field)
--   => Ranges '[field] resource
--   -> Range field (RangeType resource field)
-- extractRangeSingleton = fromJust . extractRange

