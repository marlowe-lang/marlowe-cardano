{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Tx.API (
  TxHeader (..),
  Tx (..),
  CardanoTx,
  ContractTx,
  CreateTxEnvelope (..),
  ApplyInputsTx,
  ApplyInputsTxEnvelope (..),
  WithdrawTx,
  WithdrawTxEnvelope (..),
  PostTxAPI,
  PutSignedTxAPI,
  TxJSON,
) where

import qualified Data.HashMap.Strict.InsOrd as HashMap
import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
 )
import GHC.Generics (Generic)
import Network.HTTP.Media ((//))
import Servant (
  Accept,
  Header',
  JSON,
  NoContent,
  Proxy (..),
  PutAccepted,
  ReqBody,
  Required,
  Strict,
  type (:>),
 )
import Servant.API (Accept (..))

import Data.Map (Map)
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToSchema,
  declareSchemaRef,
  properties,
  required,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError)
import qualified Marlowe.Plutus.Semantics as V1
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList)
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Asset (Assets)
import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope,
  TxId,
  TxOutRef,
  TxStatus, TransactionUnspentOutput,
 )
import Language.Marlowe.Runtime.Web.Payout.API (Payout)
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )
import qualified Cardano.Api as C
import Control.DeepSeq (NFData)

data TxHeader = TxHeader
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , utxo :: Maybe TxOutRef
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance NFData TxHeader

instance HasPagination TxHeader "transactionId" where
  type RangeType TxHeader "transactionId" = TxId
  getFieldValue _ TxHeader{..} = transactionId

data Tx = Tx
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , inputUtxo :: TxOutRef
  , inputContract :: Semantics.Contract
  , inputState :: Semantics.State
  , inputs :: [Semantics.Input]
  , outputUtxo :: Maybe TxOutRef
  , outputContract :: Maybe Semantics.Contract
  , outputState :: Maybe Semantics.State
  , assets :: Assets
  , payouts :: [Payout]
  , consumingTx :: Maybe TxId
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , reconstructedSemanticInput :: V1.TransactionInput
  , reconstructedSemanticOutput :: V1.TransactionOutput
  , txBody :: Maybe TextEnvelope
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance NFData Tx

data TxJSON a
data CardanoTx
data ContractTx
data ApplyInputsTx
data WithdrawTx

type PutSignedTxAPI = ReqBody '[JSON] TextEnvelope :> PutAccepted '[JSON] NoContent

type PostTxAPI api =
  Header' '[Required, Strict] "X-Change-Address" Address
    :> Header' '[Required, Strict] "X-Wallet-UTxO" (CommaList (TransactionUnspentOutput C.ConwayEra))
    :> api

instance Accept (TxJSON ContractTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.contract-tx-json"

instance Accept (TxJSON ApplyInputsTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.apply-inputs-tx-json"

instance Accept (TxJSON WithdrawTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.withdraw-tx-json"

data CreateTxEnvelope tx = CreateTxEnvelope
  { contractId :: TxOutRef
  , txEnvelope :: TextEnvelope
  , safetyErrors :: [SafetyError]
  }
  deriving (Show, Eq, Generic)

instance NFData (CreateTxEnvelope tx)

instance ToJSON (CreateTxEnvelope CardanoTx) where
  toJSON CreateTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("tx", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]
instance FromJSON (CreateTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "CreateTxEnvelope"
      ( \obj ->
          CreateTxEnvelope
            <$> obj
              .: "contractId"
            <*> obj
              .: "tx"
            <*> obj
              .: "safetyErrors"
      )

instance ToSchema (CreateTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "CreateTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ HashMap.fromList
                [ ("contractId", contractIdSchema)
                , ("tx", txEnvelopeSchema)
                , ("safetyErrors", safetyErrorsSchema)
                ]
          & required .~ ["contractId", "tx"]

data WithdrawTxEnvelope tx = WithdrawTxEnvelope
  { withdrawalId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (WithdrawTxEnvelope CardanoTx) where
  toJSON WithdrawTxEnvelope{..} =
    object
      [ ("withdrawalId", toJSON withdrawalId)
      , ("tx", toJSON txEnvelope)
      ]

instance FromJSON (WithdrawTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "WithdrawTxEnvelope"
      ( \obj ->
          WithdrawTxEnvelope
            <$> obj .: "withdrawalId"
            <*> obj .: "tx"
      )

instance ToSchema (WithdrawTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "WithdrawTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ HashMap.fromList
                [ ("withdrawalId", withdrawalIdSchema)
                , ("tx", txEnvelopeSchema)
                ]
          & required .~ ["withdrawalId", "tx"]

data ApplyInputsTxEnvelope tx = ApplyInputsTxEnvelope
  { contractId :: TxOutRef
  , transactionId :: TxId
  , txEnvelope :: TextEnvelope
  , safetyErrors :: [SafetyError]
  }
  deriving (Show, Eq, Generic)

instance NFData (ApplyInputsTxEnvelope CardanoTx)

instance ToJSON (ApplyInputsTxEnvelope CardanoTx) where
  toJSON ApplyInputsTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("transactionId", toJSON transactionId)
      , ("tx", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]

instance FromJSON (ApplyInputsTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "ApplyInputsTxEnvelope"
      ( \obj -> do
          contractId <- obj .: "contractId"
          transactionId <- obj .: "transactionId"
          txEnvelope <- obj .: "tx"
          safetyErrors <- obj .: "safetyErrors"
          pure ApplyInputsTxEnvelope{..}
      )

instance ToSchema (ApplyInputsTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "ApplyInputsTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ HashMap.fromList
                 [ ("contractId", contractIdSchema)
                 , ("transactionId", transactionIdSchema)
                 , ("tx", txEnvelopeSchema)
                 , ("safetyErrors", safetyErrorsSchema)
                 ]
          & required .~ ["contractId", "transactionId", "tx"]

