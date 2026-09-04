-- | This module defines a server for the /contracts REST API.
module Language.Marlowe.Runtime.Web.Contract.Server (server) where

import Language.Marlowe.Runtime.Core.Api qualified as Core
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (
  runUVerbT,
 )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace (..), fromUTxOsList, toUTxOTuple)
import Language.Marlowe.Object.Types (ContractHash(ContractHash))
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Server.DTO (
  ToDTO (toDTO),
  fromDTOThrow,
  )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM, loadContractL, runEff1, initContractL,
 )
import Language.Marlowe.Runtime.Web.Contract.API as Web (
  ContractId,
  ContractsAPI,
  GetContractResponse, ContractAPI, ContractOrSourceId (ContractOrSourceId), PostContractsResponse,
  PostContractsRequest (contract, threadTokenName, roles, accounts, metadata, tags, version, minUTxODeposit),
  unContractSourceId, minUTxODeposit
 )
import qualified Language.Marlowe.Runtime.Web.Contract.Next.Server as Next
import Language.Marlowe.Runtime.Web.Server.ApiError (
   badRequest',
   notFound',
   throwDTOError,
  )
import Servant (
  HasServer (ServerT),
  Proxy (Proxy),
  type (:<|>) ((:<|>)),
  Union,
 )
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Language.Marlowe.Runtime.Web.Contract.Transaction.Server as Transactions
import qualified Language.Marlowe.Runtime.Web.Contract.Source.Server as Source
import Language.Marlowe.Runtime.Web.Core.Address (StakeAddress, Address)
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (unCommaList))
import Language.Marlowe.Runtime.Web.Core.Tx as Web
import qualified Language.Marlowe.Runtime.Web.Core.Address as Web
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Runtime.Core.Api
    ( MarloweVersion(MarloweV1),
      SomeMarloweVersion(SomeMarloweVersion), MarloweTransactionMetadata(MarloweTransactionMetadata, marloweMetadata, transactionMetadata) )
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Transaction.Api (ContractInitializedInEra(ContractInitializedInEra, contractId, txBody, safetyErrors), ContractInitialized(ContractInitialized))
import Control.Lens (view)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext(WalletContext))
import Control.Monad.Catch (MonadThrow(throwM))
import Language.Marlowe.Runtime.Web.Tx.API (CardanoTx, CreateTxEnvelope (CreateTxEnvelope))

server :: ServerT ContractsAPI ServerM
server =
  contractServer
  :<|> postCreateTxResponse
  :<|> Source.server

contractServer :: Web.ContractId -> ServerT ContractAPI ServerM
contractServer contractId =
  getOne contractId
  :<|> Next.server contractId
  :<|> Transactions.server contractId

getOne
  :: Web.ContractId
  -> ServerM (Union '[ GetContractResponse ])
getOne contractId = runUVerbT do
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  runEff1 loadContractL contractId' >>= \case
    Nothing -> lift $ throwM $ notFound' "Contract not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      pure $ IncludeLink (Proxy @"transactions") contractState

postCreateTxBody
  :: PostContractsRequest
  -> Maybe Web.StakeAddress
  -> Web.Address
  -> CommaList (Web.TransactionUnspentOutput C.ConwayEra)
  -> ServerM (Core.ContractId, TxBodyInAnyEra, [SafetyError])
postCreateTxBody req stakeAddressDTO changeAddressDTO availableUtxosDTO = do
  SomeMarloweVersion MarloweV1 <- fromDTOThrow (badRequest' "Unsupported Marlowe version") req.version
  stakeAddress <- fromDTOThrow (badRequest' "Invalid stake address value") stakeAddressDTO
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  availableUTxOs <- fromDTOThrow (badRequest' "Invalid funding UTxO value") (unCommaList availableUtxosDTO)
  threadTokenName' <- fromDTOThrow (badRequest' "Invalid thread token name") req.threadTokenName
  roles' <- fromDTOThrow (badRequest' "Invalid roles value") req.roles
  accounts' <- fromDTOThrow (badRequest' "Invalid initial accounts") req.accounts
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") req.metadata
  marloweMetadata <-
    fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null req.tags then Nothing else Just req.tags
  let
    ContractOrSourceId contract' = req.contract
    marloweTransactionMetadata = MarloweTransactionMetadata{marloweMetadata, transactionMetadata}
    walletContext = WalletContext
      (fromUTxOsList availableUTxOs)
      (Set.fromList $ map (fst . toUTxOTuple) availableUTxOs)
      changeAddress
  initContract <- view initContractL
  initContract
    stakeAddress
    walletContext
    threadTokenName'
    roles'
    marloweTransactionMetadata
    (Lovelace <$> req.minUTxODeposit)
    accounts'
    (ContractHash . unContractSourceId <$> contract')
    >>= \case
      Left err -> throwDTOError err
      Right
        (ContractInitialized C.BabbageEraOnwardsBabbage ContractInitializedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractInitialized C.BabbageEraOnwardsConway ContractInitializedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractInitialized C.BabbageEraOnwardsDijkstra ContractInitializedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)

postCreateTxResponse
  :: Maybe StakeAddress
  -> PostContractsRequest
  -> Address
  -> CommaList (Web.TransactionUnspentOutput C.ConwayEra)
  -> ServerM (Union '[PostContractsResponse CardanoTx])
postCreateTxResponse stakeAddressDTO req changeAddressDTO availableUTxOsDTO = runUVerbT do
  (contractId, TxBodyInAnyEra txBody, safetyErrors) <- lift $
    postCreateTxBody req stakeAddressDTO changeAddressDTO availableUTxOsDTO
  let tx = C.makeSignedTransaction [] txBody
  let (contractId', tx') = toDTO (contractId, tx)
  let body = CreateTxEnvelope contractId' tx' safetyErrors
  pure (IncludeLink (Proxy @"contract") body :: PostContractsResponse CardanoTx)


