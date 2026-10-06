{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Types where

--------------------------------------------------------------------------------------------
-- Old style example:
--
-- data PingPong where
--   StIdle :: PingPong
--   StBusy :: PingPong
--   StDone :: PingPong
--
-- instance Protocol PingPong where
--
--   -- | The actual messages in our protocol.
--   --
--   -- These involve transitions between different states within the 'PingPong'
--   -- states. A ping request goes from idle to busy, and a pong response go from
--   -- busy to idle.
--   --
--   -- This example is so simple that we have all the messages directly as
--   -- constructors within this type. In more complex cases it may be better to
--   -- factor all (or related) requests and all responses within one case (in
--   -- which case the state transitions may depend on the particular message via
--   -- the usual GADT tricks).
--   --
--   data Message PingPong from to where
--     MsgPing :: Message PingPong StIdle StBusy
--     MsgPong :: Message PingPong StBusy StIdle
--     MsgDone :: Message PingPong StIdle StDone
--
--   -- | We have to explain to the framework what our states mean, in terms of
--   -- who is expected to send and receive in the different states.
--   --
--   -- Idle states are where it is for the client to send a message.
--   --
--   data ClientHasAgency st where
--     TokIdle :: ClientHasAgency StIdle
--
--   -- | Busy states are where the server is expected to send a reply (a pong).
--   --
--   data ServerHasAgency st where
--     TokBusy :: ServerHasAgency StBusy
--
--   -- | In the done state neither client nor server can send messages.
--   --
--   data NobodyHasAgency st where
--     TokDone :: NobodyHasAgency StDone
--
--   exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
--   exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
--   exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}
--
--
-- deriving instance Show (Message PingPong from to)
--
-- instance Show (ClientHasAgency (st :: PingPong)) where
--   show TokIdle = "TokIdle"
--
-- instance Show (ServerHasAgency (st :: PingPong)) where
--   show TokBusy = "TokBusy"
--
--
--------------------------------------------------------------------------------------------
-- New style example:
--
-- {-# LANGUAGE DataKinds          #-}
-- {-# LANGUAGE EmptyCase          #-}
-- {-# LANGUAGE FlexibleInstances  #-}
-- {-# LANGUAGE GADTs              #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies       #-}
--
--
-- module Network.TypedProtocol.PingPong.Type where
--
-- import           Network.TypedProtocol.Core
--
--
-- \| The ping\/pong protocol and the states in its protocol state machine.
--
-- This protocol serves as a simple example of the typed protocols framework
-- to help understand the framework and as a template for writing other
-- protocols.
--
-- For a slightly more realistic example, see the request\/response protocol
-- "Network.TypedProtocol.ResResp.Type".
--
-- This declares the protocol itself. It is used both as a type level tag for
-- the protocol and as the kind of the types of the states in the protocol
-- state machine. That is @PingPong@ is a kind, and @StIdle@ is a type of
-- that kind.
--
-- If the protocol needs any type parameters (e.g. for thing that end up in
-- the messages) then those type parameters go here. See the request\/response
-- protocol for an example. It is parametrised over the types of the request
-- and response.
--
-- data PingPong where
--   StIdle :: PingPong
--   StBusy :: PingPong
--   StDone :: PingPong
--
-- data SPingPong (st :: PingPong) where
--   SingIdle :: SPingPong StIdle
--   SingBusy :: SPingPong StBusy
--   SingDone :: SPingPong StDone
--
-- deriving instance Show (SPingPong st)
--
-- instance StateTokenI StIdle where stateToken = SingIdle
-- instance StateTokenI StBusy where stateToken = SingBusy
-- instance StateTokenI StDone where stateToken = SingDone
--
-- instance Protocol PingPong where
--
--   -- | The actual messages in our protocol.
--   --
--   -- These involve transitions between different states within the 'PingPong'
--   -- states. A ping request goes from idle to busy, and a pong response go from
--   -- busy to idle.
--   --
--   -- This example is so simple that we have all the messages directly as
--   -- constructors within this type. In more complex cases it may be better to
--   -- factor all (or related) requests and all responses within one case (in
--   -- which case the state transitions may depend on the particular message via
--   -- the usual GADT tricks).
--   --
--   data Message PingPong from to where
--     MsgPing :: Message PingPong StIdle StBusy
--     MsgPong :: Message PingPong StBusy StIdle
--     MsgDone :: Message PingPong StIdle StDone
--
--   type StateAgency StIdle = ClientAgency
--   type StateAgency StBusy = ServerAgency
--   type StateAgency StDone = NobodyAgency
--
--   type StateToken = SPingPong
--
--
-- deriving instance Show (Message PingPong from to)
--
--------------------------------------------------------------------------------------------
-- AI! this is our old implementation of the core protocol.
-- could you please try to port it to the new API which is indicated by the example above?
-- The old typed-protocol library API is also provided above.
--
-- Our old style implementation:
--
-- import Data.Binary (getWord8, putWord8)
-- import Data.Foldable (fold)
-- import Data.List (intersperse)
-- import Data.Proxy (Proxy (..))
-- import GHC.Show (showSpace)
-- import Language.Marlowe.Protocol.BulkSync.Types (MarloweBulkSync)
-- import qualified Language.Marlowe.Protocol.BulkSync.Types as MarloweBulkSync
-- import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
-- import qualified Language.Marlowe.Protocol.HeaderSync.Types as MarloweHeaderSync
-- import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
-- import qualified Language.Marlowe.Protocol.Load.Types as MarloweLoad
-- import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
-- import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
-- import qualified Language.Marlowe.Protocol.Sync.Types as MarloweSync
-- import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
-- import qualified Language.Marlowe.Protocol.Transfer.Types as MarloweTransfer
-- import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
-- import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
-- import Network.Protocol.Codec
-- import Network.Protocol.Codec.Spec (ShowProtocol (..))
-- import Network.Protocol.Handshake.Types (HasSignature (..))
-- import Network.Protocol.Job.Types (Job)
-- import qualified Network.Protocol.Job.Types as Job
-- import Network.Protocol.Peer.Trace
-- import Network.Protocol.Query.Types (Query)
-- import qualified Network.Protocol.Query.Types as Query
-- import Network.TypedProtocol
--
-- data MarloweRuntime where
--   StInit :: MarloweRuntime
--   StMarloweSync :: MarloweSync -> MarloweRuntime
--   StMarloweHeaderSync :: MarloweHeaderSync -> MarloweRuntime
--   StMarloweBulkSync :: MarloweBulkSync -> MarloweRuntime
--   StMarloweQuery :: MarloweQuery -> MarloweRuntime
--   StTxJob :: Job MarloweTxCommand -> MarloweRuntime
--   StMarloweLoad :: MarloweLoad -> MarloweRuntime
--   StContractQuery :: Query ContractRequest -> MarloweRuntime
--   StMarloweTransfer :: MarloweTransfer -> MarloweRuntime
--
-- instance Protocol MarloweRuntime where
--   data Message MarloweRuntime st st' where
--     MsgRunMarloweSync :: Message MarloweRuntime 'StInit ('StMarloweSync 'MarloweSync.StInit)
--     MsgRunMarloweHeaderSync :: Message MarloweRuntime 'StInit ('StMarloweHeaderSync 'MarloweHeaderSync.StIdle)
--     MsgRunMarloweBulkSync :: Message MarloweRuntime 'StInit ('StMarloweBulkSync 'MarloweBulkSync.StIdle)
--     MsgRunMarloweQuery :: Message MarloweRuntime 'StInit ('StMarloweQuery 'Query.StReq)
--     MsgRunMarloweLoad :: Message MarloweRuntime 'StInit ('StMarloweLoad ('MarloweLoad.StProcessing 'MarloweLoad.RootNode))
--     MsgRunMarloweTransfer :: Message MarloweRuntime 'StInit ('StMarloweTransfer 'MarloweTransfer.StIdle)
--     MsgRunTxJob :: Message MarloweRuntime 'StInit ('StTxJob 'Job.StInit)
--     MsgRunContractQuery :: Message MarloweRuntime 'StInit ('StContractQuery 'Query.StReq)
--     MsgMarloweSync :: Message MarloweSync st st' -> Message MarloweRuntime ('StMarloweSync st) ('StMarloweSync st')
--     MsgMarloweHeaderSync
--       :: Message MarloweHeaderSync st st' -> Message MarloweRuntime ('StMarloweHeaderSync st) ('StMarloweHeaderSync st')
--     MsgMarloweBulkSync
--       :: Message MarloweBulkSync st st' -> Message MarloweRuntime ('StMarloweBulkSync st) ('StMarloweBulkSync st')
--     MsgMarloweQuery :: Message MarloweQuery st st' -> Message MarloweRuntime ('StMarloweQuery st) ('StMarloweQuery st')
--     MsgMarloweLoad :: Message MarloweLoad st st' -> Message MarloweRuntime ('StMarloweLoad st) ('StMarloweLoad st')
--     MsgMarloweTransfer
--       :: Message MarloweTransfer st st' -> Message MarloweRuntime ('StMarloweTransfer st) ('StMarloweTransfer st')
--     MsgTxJob :: Message (Job MarloweTxCommand) st st' -> Message MarloweRuntime ('StTxJob st) ('StTxJob st')
--     MsgContractQuery
--       :: Message (Query ContractRequest) st st' -> Message MarloweRuntime ('StContractQuery st) ('StContractQuery st')
--
--   data ClientHasAgency st where
--     TokInit :: ClientHasAgency 'StInit
--     TokClientMarloweSync :: MarloweSync.ClientHasAgency st -> ClientHasAgency ('StMarloweSync st)
--     TokClientMarloweHeaderSync :: MarloweHeaderSync.ClientHasAgency st -> ClientHasAgency ('StMarloweHeaderSync st)
--     TokClientMarloweBulkSync :: MarloweBulkSync.ClientHasAgency st -> ClientHasAgency ('StMarloweBulkSync st)
--     TokClientMarloweQuery :: Query.ClientHasAgency st -> ClientHasAgency ('StMarloweQuery st)
--     TokClientMarloweLoad :: MarloweLoad.ClientHasAgency st -> ClientHasAgency ('StMarloweLoad st)
--     TokClientMarloweTransfer :: MarloweTransfer.ClientHasAgency st -> ClientHasAgency ('StMarloweTransfer st)
--     TokClientTxJob :: Job.ClientHasAgency st -> ClientHasAgency ('StTxJob st)
--     TokClientContractQuery :: Query.ClientHasAgency st -> ClientHasAgency ('StContractQuery st)
--
--   data ServerHasAgency st where
--     TokServerMarloweSync :: MarloweSync.ServerHasAgency st -> ServerHasAgency ('StMarloweSync st)
--     TokServerMarloweHeaderSync :: MarloweHeaderSync.ServerHasAgency st -> ServerHasAgency ('StMarloweHeaderSync st)
--     TokServerMarloweBulkSync :: MarloweBulkSync.ServerHasAgency st -> ServerHasAgency ('StMarloweBulkSync st)
--     TokServerMarloweQuery :: Query.ServerHasAgency st -> ServerHasAgency ('StMarloweQuery st)
--     TokServerMarloweLoad :: MarloweLoad.ServerHasAgency st -> ServerHasAgency ('StMarloweLoad st)
--     TokServerMarloweTransfer :: MarloweTransfer.ServerHasAgency st -> ServerHasAgency ('StMarloweTransfer st)
--     TokServerTxJob :: Job.ServerHasAgency st -> ServerHasAgency ('StTxJob st)
--     TokServerContractQuery :: Query.ServerHasAgency st -> ServerHasAgency ('StContractQuery st)
--
--   data NobodyHasAgency st where
--     TokNobodyMarloweSync :: MarloweSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweSync st)
--     TokNobodyMarloweHeaderSync :: MarloweHeaderSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweHeaderSync st)
--     TokNobodyMarloweBulkSync :: MarloweBulkSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweBulkSync st)
--     TokNobodyMarloweQuery :: Query.NobodyHasAgency st -> NobodyHasAgency ('StMarloweQuery st)
--     TokNobodyMarloweLoad :: MarloweLoad.NobodyHasAgency st -> NobodyHasAgency ('StMarloweLoad st)
--     TokNobodyMarloweTransfer :: MarloweTransfer.NobodyHasAgency st -> NobodyHasAgency ('StMarloweTransfer st)
--     TokNobodyTxJob :: Job.NobodyHasAgency st -> NobodyHasAgency ('StTxJob st)
--     TokNobodyContractQuery :: Query.NobodyHasAgency st -> NobodyHasAgency ('StContractQuery st)
--
--   exclusionLemma_ClientAndServerHaveAgency = \case
--     TokInit -> \case {}
--     TokClientMarloweSync tok -> \case
--       TokServerMarloweSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientMarloweHeaderSync tok -> \case
--       TokServerMarloweHeaderSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientMarloweBulkSync tok -> \case
--       TokServerMarloweBulkSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientMarloweQuery tok -> \case
--       TokServerMarloweQuery tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientMarloweLoad tok -> \case
--       TokServerMarloweLoad tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientMarloweTransfer tok -> \case
--       TokServerMarloweTransfer tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientTxJob tok -> \case
--       TokServerTxJob tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--     TokClientContractQuery tok -> \case
--       TokServerContractQuery tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
--
--   exclusionLemma_NobodyAndClientHaveAgency = \case
--     TokNobodyMarloweSync tok -> \case
--       TokClientMarloweSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyMarloweHeaderSync tok -> \case
--       TokClientMarloweHeaderSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyMarloweBulkSync tok -> \case
--       TokClientMarloweBulkSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyMarloweQuery tok -> \case
--       TokClientMarloweQuery tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyMarloweLoad tok -> \case
--       TokClientMarloweLoad tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyMarloweTransfer tok -> \case
--       TokClientMarloweTransfer tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyTxJob tok -> \case
--       TokClientTxJob tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--     TokNobodyContractQuery tok -> \case
--       TokClientContractQuery tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
--
--   exclusionLemma_NobodyAndServerHaveAgency = \case
--     TokNobodyMarloweSync tok -> \case
--       TokServerMarloweSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyMarloweHeaderSync tok -> \case
--       TokServerMarloweHeaderSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyMarloweBulkSync tok -> \case
--       TokServerMarloweBulkSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyMarloweQuery tok -> \case
--       TokServerMarloweQuery tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyMarloweLoad tok -> \case
--       TokServerMarloweLoad tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyMarloweTransfer tok -> \case
--       TokServerMarloweTransfer tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyTxJob tok -> \case
--       TokServerTxJob tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--     TokNobodyContractQuery tok -> \case
--       TokServerContractQuery tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
--
-- instance ShowProtocol MarloweRuntime where
--   showsPrecMessage p tok = \case
--     MsgRunMarloweSync -> showString "MsgRunMarloweSync"
--     MsgRunMarloweHeaderSync -> showString "MsgRunMarloweHeaderSync"
--     MsgRunMarloweBulkSync -> showString "MsgRunMarloweBulkSync"
--     MsgRunMarloweQuery -> showString "MsgRunMarloweQuery"
--     MsgRunMarloweLoad -> showString "MsgRunMarloweLoad"
--     MsgRunMarloweTransfer -> showString "MsgRunMarloweTransfer"
--     MsgRunTxJob -> showString "MsgRunTxJob"
--     MsgRunContractQuery -> showString "MsgRunContractQuery"
--     MsgMarloweSync msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweSync"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweSync tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweSync tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgMarloweHeaderSync msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweHeaderSync"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweHeaderSync tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweHeaderSync tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgMarloweBulkSync msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweBulkSync"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweBulkSync tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweBulkSync tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgMarloweQuery msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweQuery"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweQuery tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweQuery tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgMarloweLoad msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweLoad"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweLoad tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweLoad tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgMarloweTransfer msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgMarloweTransfer"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientMarloweTransfer tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerMarloweTransfer tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgTxJob msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgTxJob"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientTxJob tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerTxJob tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--     MsgContractQuery msg ->
--       showParen
--         (p >= 11)
--         ( showString "MsgContractQuery"
--             . showSpace
--             . case tok of
--               ClientAgency (TokClientContractQuery tok') -> showsPrecMessage p (ClientAgency tok') msg
--               ServerAgency (TokServerContractQuery tok') -> showsPrecMessage p (ServerAgency tok') msg
--         )
--
--   showsPrecServerHasAgency p = \case
--     TokServerMarloweSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweSync"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerMarloweHeaderSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweHeaderSync"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerMarloweBulkSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweBulkSync"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerMarloweQuery tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweQuery"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerMarloweLoad tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweLoad"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerMarloweTransfer tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerMarloweTransfer"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerTxJob tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerTxJob"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--     TokServerContractQuery tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokServerContractQuery"
--             . showSpace
--             . showsPrecServerHasAgency 11 tok
--         )
--
--   showsPrecClientHasAgency p = \case
--     TokInit -> showString "TokInit"
--     TokClientMarloweSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweSync"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientMarloweHeaderSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweHeaderSync"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientMarloweBulkSync tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweBulkSync"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientMarloweQuery tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweQuery"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientMarloweLoad tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweLoad"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientMarloweTransfer tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientMarloweTransfer"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientTxJob tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientTxJob"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--     TokClientContractQuery tok ->
--       showParen
--         (p >= 11)
--         ( showString "TokClientContractQuery"
--             . showSpace
--             . showsPrecClientHasAgency 11 tok
--         )
--
-- instance OTelProtocol MarloweRuntime where
--   protocolName _ = "marlowe_runtime"
--   messageAttributes tok = \case
--     MsgRunMarloweSync ->
--       MessageAttributes
--         { messageType = "run_marlowe_sync"
--         , messageParameters = []
--         }
--     MsgRunMarloweHeaderSync ->
--       MessageAttributes
--         { messageType = "run_marlowe_header_sync"
--         , messageParameters = []
--         }
--     MsgRunMarloweBulkSync ->
--       MessageAttributes
--         { messageType = "run_marlowe_bulk_sync"
--         , messageParameters = []
--         }
--     MsgRunMarloweQuery ->
--       MessageAttributes
--         { messageType = "run_marlowe_query"
--         , messageParameters = []
--         }
--     MsgRunMarloweLoad ->
--       MessageAttributes
--         { messageType = "run_marlowe_load"
--         , messageParameters = []
--         }
--     MsgRunMarloweTransfer ->
--       MessageAttributes
--         { messageType = "run_marlowe_import"
--         , messageParameters = []
--         }
--     MsgRunTxJob ->
--       MessageAttributes
--         { messageType = "run_tx_job"
--         , messageParameters = []
--         }
--     MsgRunContractQuery ->
--       MessageAttributes
--         { messageType = "run_contract_query"
--         , messageParameters = []
--         }
--     MsgMarloweSync msg -> case tok of
--       ClientAgency (TokClientMarloweSync tok') ->
--         subMessageAttributes "marlowe_sync" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweSync tok') ->
--         subMessageAttributes "marlowe_sync" $ messageAttributes (ServerAgency tok') msg
--     MsgMarloweHeaderSync msg -> case tok of
--       ClientAgency (TokClientMarloweHeaderSync tok') ->
--         subMessageAttributes "marlowe_header_sync" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweHeaderSync tok') ->
--         subMessageAttributes "marlowe_header_sync" $ messageAttributes (ServerAgency tok') msg
--     MsgMarloweBulkSync msg -> case tok of
--       ClientAgency (TokClientMarloweBulkSync tok') ->
--         subMessageAttributes "marlowe_Bulk_sync" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweBulkSync tok') ->
--         subMessageAttributes "marlowe_Bulk_sync" $ messageAttributes (ServerAgency tok') msg
--     MsgMarloweQuery msg -> case tok of
--       ClientAgency (TokClientMarloweQuery tok') ->
--         subMessageAttributes "marlowe_query" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweQuery tok') ->
--         subMessageAttributes "marlowe_query" $ messageAttributes (ServerAgency tok') msg
--     MsgMarloweLoad msg -> case tok of
--       ClientAgency (TokClientMarloweLoad tok') ->
--         subMessageAttributes "marlowe_load" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweLoad tok') ->
--         subMessageAttributes "marlowe_load" $ messageAttributes (ServerAgency tok') msg
--     MsgMarloweTransfer msg -> case tok of
--       ClientAgency (TokClientMarloweTransfer tok') ->
--         subMessageAttributes "marlowe_import" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerMarloweTransfer tok') ->
--         subMessageAttributes "marlowe_import" $ messageAttributes (ServerAgency tok') msg
--     MsgTxJob msg -> case tok of
--       ClientAgency (TokClientTxJob tok') ->
--         subMessageAttributes "tx_job" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerTxJob tok') ->
--         subMessageAttributes "tx_job" $ messageAttributes (ServerAgency tok') msg
--     MsgContractQuery msg -> case tok of
--       ClientAgency (TokClientContractQuery tok') ->
--         subMessageAttributes "contract_query" $ messageAttributes (ClientAgency tok') msg
--       ServerAgency (TokServerContractQuery tok') ->
--         subMessageAttributes "contract_query" $ messageAttributes (ServerAgency tok') msg
--     where
--       subMessageAttributes subProtocol attrs@MessageAttributes{..} =
--         attrs
--           { messageType = subProtocol <> "." <> messageType
--           }
--
-- instance BinaryMessage MarloweRuntime where
--   putMessage = \case
--     ClientAgency TokInit -> \case
--       MsgRunMarloweSync -> putWord8 0x00
--       MsgRunMarloweHeaderSync -> putWord8 0x01
--       MsgRunMarloweBulkSync -> putWord8 0x07
--       MsgRunMarloweQuery -> putWord8 0x02
--       MsgRunTxJob -> putWord8 0x03
--       MsgRunMarloweLoad -> putWord8 0x04
--       MsgRunContractQuery -> putWord8 0x05
--       MsgRunMarloweTransfer -> putWord8 0x06
--     ClientAgency (TokClientMarloweSync tok) -> \case
--       MsgMarloweSync msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientMarloweHeaderSync tok) -> \case
--       MsgMarloweHeaderSync msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientMarloweBulkSync tok) -> \case
--       MsgMarloweBulkSync msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientMarloweQuery tok) -> \case
--       MsgMarloweQuery msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientMarloweLoad tok) -> \case
--       MsgMarloweLoad msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientMarloweTransfer tok) -> \case
--       MsgMarloweTransfer msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientTxJob tok) -> \case
--       MsgTxJob msg -> putMessage (ClientAgency tok) msg
--     ClientAgency (TokClientContractQuery tok) -> \case
--       MsgContractQuery msg -> putMessage (ClientAgency tok) msg
--     ServerAgency (TokServerMarloweSync tok) -> \case
--       MsgMarloweSync msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerMarloweHeaderSync tok) -> \case
--       MsgMarloweHeaderSync msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerMarloweBulkSync tok) -> \case
--       MsgMarloweBulkSync msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerMarloweQuery tok) -> \case
--       MsgMarloweQuery msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerMarloweLoad tok) -> \case
--       MsgMarloweLoad msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerMarloweTransfer tok) -> \case
--       MsgMarloweTransfer msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerTxJob tok) -> \case
--       MsgTxJob msg -> putMessage (ServerAgency tok) msg
--     ServerAgency (TokServerContractQuery tok) -> \case
--       MsgContractQuery msg -> putMessage (ServerAgency tok) msg
--
--   getMessage = \case
--     ClientAgency TokInit ->
--       getWord8 >>= \case
--         0x00 -> pure $ SomeMessage MsgRunMarloweSync
--         0x01 -> pure $ SomeMessage MsgRunMarloweHeaderSync
--         0x02 -> pure $ SomeMessage MsgRunMarloweQuery
--         0x03 -> pure $ SomeMessage MsgRunTxJob
--         0x04 -> pure $ SomeMessage MsgRunMarloweLoad
--         0x05 -> pure $ SomeMessage MsgRunContractQuery
--         0x06 -> pure $ SomeMessage MsgRunMarloweTransfer
--         0x07 -> pure $ SomeMessage MsgRunMarloweBulkSync
--         tag -> fail $ "invalid message tag " <> show tag
--     ClientAgency (TokClientMarloweSync tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweSync msg
--     ClientAgency (TokClientMarloweHeaderSync tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweHeaderSync msg
--     ClientAgency (TokClientMarloweBulkSync tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweBulkSync msg
--     ClientAgency (TokClientMarloweQuery tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweQuery msg
--     ClientAgency (TokClientMarloweLoad tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweLoad msg
--     ClientAgency (TokClientMarloweTransfer tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgMarloweTransfer msg
--     ClientAgency (TokClientTxJob tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgTxJob msg
--     ClientAgency (TokClientContractQuery tok) -> do
--       SomeMessage msg <- getMessage (ClientAgency tok)
--       pure $ SomeMessage $ MsgContractQuery msg
--     ServerAgency (TokServerMarloweSync tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweSync msg
--     ServerAgency (TokServerMarloweHeaderSync tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweHeaderSync msg
--     ServerAgency (TokServerMarloweBulkSync tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweBulkSync msg
--     ServerAgency (TokServerMarloweQuery tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweQuery msg
--     ServerAgency (TokServerMarloweLoad tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweLoad msg
--     ServerAgency (TokServerMarloweTransfer tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgMarloweTransfer msg
--     ServerAgency (TokServerTxJob tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgTxJob msg
--     ServerAgency (TokServerContractQuery tok) -> do
--       SomeMessage msg <- getMessage (ServerAgency tok)
--       pure $ SomeMessage $ MsgContractQuery msg
--
-- instance HasSignature MarloweRuntime where
--   signature _ =
--     fold $
--       intersperse
--         " "
--         [ "MarloweRuntime"
--         , signature $ Proxy @MarloweSync
--         , signature $ Proxy @MarloweHeaderSync
--         , signature $ Proxy @MarloweBulkSync
--         , signature $ Proxy @MarloweQuery
--         , signature $ Proxy @MarloweLoad
--         , signature $ Proxy @MarloweTransfer
--         , signature $ Proxy @(Job MarloweTxCommand)
--         , signature $ Proxy @(Query ContractRequest)
--         ]

import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol

data MarloweRuntime where
  StInit :: MarloweRuntime
  StMarloweSync :: MarloweSync -> MarloweRuntime
  StMarloweHeaderSync :: MarloweHeaderSync -> MarloweRuntime
  StMarloweBulkSync :: MarloweBulkSync -> MarloweRuntime
  StMarloweQuery :: MarloweQuery -> MarloweRuntime
  StTxJob :: Job MarloweTxCommand -> MarloweRuntime
  StMarloweLoad :: MarloweLoad -> MarloweRuntime
  StContractQuery :: Query ContractRequest -> MarloweRuntime
  StMarloweTransfer :: MarloweTransfer -> MarloweRuntime

data SMarloweRuntime (st :: MarloweRuntime) where
  SingInit :: SMarloweRuntime StInit
  SingMarloweSync :: StateToken st -> SMarloweRuntime (StMarloweSync st)
  SingMarloweHeaderSync :: StateToken st -> SMarloweRuntime (StMarloweHeaderSync st)
  SingMarloweBulkSync :: StateToken st -> SMarloweRuntime (StMarloweBulkSync st)
  SingMarloweQuery :: StateToken st -> SMarloweRuntime (StMarloweQuery st)
  SingTxJob :: StateToken st -> SMarloweRuntime (StTxJob st)
  SingMarloweLoad :: StateToken st -> SMarloweRuntime (StMarloweLoad st)
  SingContractQuery :: StateToken st -> SMarloweRuntime (StContractQuery st)
  SingMarloweTransfer :: StateToken st -> SMarloweRuntime (StMarloweTransfer st)

deriving instance Show (SMarloweRuntime st)

instance StateTokenI StInit where stateToken = SingInit
instance (StateTokenI st) => StateTokenI (StMarloweSync st) where stateToken = SingMarloweSync stateToken
instance (StateTokenI st) => StateTokenI (StMarloweHeaderSync st) where stateToken = SingMarloweHeaderSync stateToken
instance (StateTokenI st) => StateTokenI (StMarloweBulkSync st) where stateToken = SingMarloweBulkSync stateToken
instance (StateTokenI st) => StateTokenI (StMarloweQuery st) where stateToken = SingMarloweQuery stateToken
instance (StateTokenI st) => StateTokenI (StTxJob st) where stateToken = SingTxJob stateToken
instance (StateTokenI st) => StateTokenI (StMarloweLoad st) where stateToken = SingMarloweLoad stateToken
instance (StateTokenI st) => StateTokenI (StContractQuery st) where stateToken = SingContractQuery stateToken
instance (StateTokenI st) => StateTokenI (StMarloweTransfer st) where stateToken = SingMarloweTransfer stateToken

instance Protocol MarloweRuntime where
  data Message MarloweRuntime from to where
    MsgRunMarloweSync :: Message MarloweRuntime StInit (StMarloweSync MarloweSync.StInit)
    MsgRunMarloweHeaderSync :: Message MarloweRuntime StInit (StMarloweHeaderSync MarloweHeaderSync.StIdle)
    MsgRunMarloweBulkSync :: Message MarloweRuntime StInit (StMarloweBulkSync MarloweBulkSync.StIdle)
    MsgRunMarloweQuery :: Message MarloweRuntime StInit (StMarloweQuery Query.StReq)
    MsgRunMarloweLoad :: Message MarloweRuntime StInit (StMarloweLoad (MarloweLoad.StProcessing MarloweLoad.RootNode))
    MsgRunMarloweTransfer :: Message MarloweRuntime StInit (StMarloweTransfer MarloweTransfer.StIdle)
    MsgRunTxJob :: Message MarloweRuntime StInit (StTxJob Job.StInit)
    MsgRunContractQuery :: Message MarloweRuntime StInit (StContractQuery Query.StReq)
    MsgMarloweSync :: Message MarloweSync st st' -> Message MarloweRuntime (StMarloweSync st) (StMarloweSync st')
    MsgMarloweHeaderSync
      :: Message MarloweHeaderSync st st' -> Message MarloweRuntime (StMarloweHeaderSync st) (StMarloweHeaderSync st')
    MsgMarloweBulkSync
      :: Message MarloweBulkSync st st' -> Message MarloweRuntime (StMarloweBulkSync st) (StMarloweBulkSync st')
    MsgMarloweQuery :: Message MarloweQuery st st' -> Message MarloweRuntime (StMarloweQuery st) (StMarloweQuery st')
    MsgMarloweLoad :: Message MarloweLoad st st' -> Message MarloweRuntime (StMarloweLoad st) (StMarloweLoad st')
    MsgMarloweTransfer
      :: Message MarloweTransfer st st' -> Message MarloweRuntime (StMarloweTransfer st) (StMarloweTransfer st')
    MsgTxJob :: Message (Job MarloweTxCommand) st st' -> Message MarloweRuntime (StTxJob st) (StTxJob st')
    MsgContractQuery
      :: Message (Query ContractRequest) st st' -> Message MarloweRuntime (StContractQuery st) (StContractQuery st')

  type StateAgency StInit = ClientAgency
  type StateAgency (StMarloweSync st) = StateAgency st
  type StateAgency (StMarloweHeaderSync st) = StateAgency st
  type StateAgency (StMarloweBulkSync st) = StateAgency st
  type StateAgency (StMarloweQuery st) = StateAgency st
  type StateAgency (StTxJob st) = StateAgency st
  type StateAgency (StMarloweLoad st) = StateAgency st
  type StateAgency (StContractQuery st) = StateAgency st
  type StateAgency (StMarloweTransfer st) = StateAgency st

  type StateToken = SMarloweRuntime

deriving instance Show (Message MarloweRuntime from to)
