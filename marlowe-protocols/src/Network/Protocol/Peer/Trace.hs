{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer.Trace where

import Data.Functor ((<&>))

-- import Data.Proxy
-- import Data.Text (Text)
import Network.TypedProtocol

-- import Network.TypedProtocol.Peer (Peer (..))
-- import Observe.Event.Render.OpenTelemetry
-- import OpenTelemetry.Trace.Core
-- import qualified OpenTelemetry.Trace.TraceState as TraceState

-- | A peer in a protocol session which is suitable for tracing.
data PeerTraced ps (pr :: PeerRole) (st :: ps) m a where
  -- | A peer sends a message to the other peer.
  YieldTraced
    :: WeHaveAgencyProof pr st
    -> Message ps st st'
    -> YieldTraced ps pr st' m a
    -> PeerTraced ps pr st m a
  -- | A peer awaits a message from the other peer.
  AwaitTraced
    :: TheyHaveAgencyProof pr st
    -> (forall (st' :: ps). Message ps st st' -> AwaitTraced ps pr st' m a)
    -> PeerTraced ps pr st m a
  -- | A peer is processing information.
  EffectTraced
    :: m (PeerTraced ps pr st m a)
    -> PeerTraced ps pr st m a
  -- | The session is done.
  DoneTraced
    :: NobodyHasAgencyProof pr st
    -> a
    -> PeerTraced ps pr st m a

deriving instance (Functor m) => Functor (PeerTraced ps pr st m)

data YieldTraced ps (pr :: PeerRole) (st :: ps) m a where
  Call
    :: TheyHaveAgencyProof pr st
    -> (forall st'. Message ps st st' -> PeerTraced ps pr st' m a)
    -> YieldTraced ps pr st m a
  Cast
    :: PeerTraced ps pr st m a
    -> YieldTraced ps pr st m a
  Close
    :: NobodyHasAgencyProof pr st
    -> a
    -> YieldTraced ps pr st m a

deriving instance (Functor m) => Functor (YieldTraced ps pr st m)

data AwaitTraced ps pr st m a where
  Respond
    :: WeHaveAgencyProof pr st
    -> m (Response ps pr st m a)
    -> AwaitTraced ps pr st m a
  Receive
    :: PeerTraced ps pr st m a
    -> AwaitTraced ps pr st m a
  Closed
    :: NobodyHasAgencyProof pr st
    -> m a
    -> AwaitTraced ps pr st m a

deriving instance (Functor m) => Functor (AwaitTraced ps pr st m)

data Response ps pr st m a where
  Response
    :: Message ps st st'
    -> PeerTraced ps pr st' m a
    -> Response ps pr st m a

deriving instance (Functor m) => Functor (Response ps pr st m)

hoistPeerTraced
  :: (Functor m)
  => (forall x. m x -> n x)
  -> PeerTraced ps pr st m a
  -> PeerTraced ps pr st n a
hoistPeerTraced f = \case
  YieldTraced tok msg yield -> YieldTraced tok msg case yield of
    Call tok' k -> Call tok' $ hoistPeerTraced f . k
    Cast next -> Cast $ hoistPeerTraced f next
    Close tok' a -> Close tok' a
  AwaitTraced tok k -> AwaitTraced tok \msg -> case k msg of
    Respond tok' m ->
      Respond tok' $
        f $
          m <&> \case
            Response msg' next -> Response msg' $ hoistPeerTraced f next
    Receive next -> Receive $ hoistPeerTraced f next
    Closed tok' ma -> Closed tok' $ f ma
  DoneTraced tok a -> DoneTraced tok a
  EffectTraced m -> EffectTraced $ f $ hoistPeerTraced f <$> m

-- peerTracedToPeer :: (Functor m) => PeerTraced ps pr st m a -> Peer ps pr st m a
-- peerTracedToPeer = \case
--   YieldTraced tok msg yield -> Yield tok msg case yield of
--     Call tok' k -> Await tok' $ peerTracedToPeer . k
--     Cast next -> peerTracedToPeer next
--     Close tok' a -> Done tok' a
--   AwaitTraced tok k -> Await tok \msg -> case k msg of
--     Respond tok' m ->
--       Effect $
--         m <&> \case
--           Response msg' next -> Yield tok' msg' $ peerTracedToPeer next
--     Receive next -> peerTracedToPeer next
--     Closed tok' ma -> Effect $ Done tok' <$> ma
--   DoneTraced tok a -> Done tok a
--   EffectTraced m -> Effect $ peerTracedToPeer <$> m
--
-- peerToPeerTraced :: (Functor m) => Peer ps pr st m a -> PeerTraced ps pr st m a
-- peerToPeerTraced = \case
--   Yield tok msg next -> YieldTraced tok msg $ Cast $ peerToPeerTraced next
--   Await tok k -> AwaitTraced tok \msg -> Receive $ peerToPeerTraced $ k msg
--   Done tok a -> DoneTraced tok a
--   Effect m -> EffectTraced $ peerToPeerTraced <$> m
--
-- data SomeSubMessage ps ps' (x :: ps) (stLift :: ps -> ps') (st' :: ps') where
--   SomeSubMessage :: Message ps x x' -> SomeSubMessage ps ps' x stLift (stLift x')
--
-- data LiftProtocol ps ps' (stLift :: ps -> ps') = LiftProtocol
--   { liftClient :: forall (x :: ps). ClientHasAgency x -> ClientHasAgency (stLift x)
--   -- ^ Lift a client agency token from the sub-protocol.
--   , liftServer :: forall (x :: ps). ServerHasAgency x -> ServerHasAgency (stLift x)
--   -- ^ Lift a server agency token from the sub-protocol.
--   , liftNobody :: forall (x :: ps). NobodyHasAgency x -> NobodyHasAgency (stLift x)
--   -- ^ Lift a nobody agency token from the sub-protocol.
--   , liftMessage :: forall (x :: ps) (x' :: ps). Message ps x x' -> Message ps' (stLift x) (stLift x')
--   -- ^ Lift a message from the sub-protocol.
--   , unliftMessage :: forall (x :: ps) (st' :: ps'). Message ps' (stLift x) st' -> SomeSubMessage ps ps' x stLift st'
--   -- ^ Unlift a message back into the sub protocol. The return type must include a proof
--   -- that the target state of any message from this state embeds a state from
--   -- the sub protocol.
--   }
--
-- -- | Lift a peer from one protocol into a protocol that embeds it as a
-- -- sub-protocol.
-- liftPeerTraced
--   :: forall ps ps' pr (st :: ps) (stLift :: ps -> ps') m a
--    . (Functor m)
--   => LiftProtocol ps ps' stLift
--   -> PeerTraced ps pr st m a
--   -> PeerTraced ps' pr (stLift st) m a
-- liftPeerTraced LiftProtocol{..} = go
--   where
--     go :: PeerTraced ps pr st' m a -> PeerTraced ps' pr (stLift st') m a
--     go = \case
--       YieldTraced tok msg yield -> YieldTraced (liftAgency tok) (liftMessage msg) case yield of
--         Call tok' k ->
--           Call (liftAgency tok') \msg' -> case unliftMessage msg' of
--             SomeSubMessage subMsg -> go $ k subMsg
--         Cast next -> Cast $ go next
--         Close tok' a -> Close (liftNobody tok') a
--       AwaitTraced tok k -> AwaitTraced (liftAgency tok) \msg -> case unliftMessage msg of
--         SomeSubMessage subMsg -> case k subMsg of
--           Respond tok' m ->
--             Respond (liftAgency tok') $
--               m <&> \case
--                 Response msg' next -> Response (liftMessage msg') $ go next
--           Receive next -> Receive $ go next
--           Closed tok' ma -> Closed (liftNobody tok') ma
--       DoneTraced tok a -> DoneTraced (liftNobody tok) a
--       EffectTraced m -> EffectTraced $ go <$> m
--
--     liftAgency :: PeerHasAgency pr' x -> PeerHasAgency pr' (stLift x)
--     liftAgency = \case
--       ClientAgency tok -> ClientAgency $ liftClient tok
--       ServerAgency tok -> ServerAgency $ liftServer tok
--
-- data TypedProtocolsSelector ps f where
--   ReceiveSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
--   CallSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
--   RespondSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
--   CastSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
--   CloseSelector :: PeerHasAgency pr st -> Message ps st st' -> TypedProtocolsSelector ps ()
--
-- defaultSpanContext :: SpanContext
-- defaultSpanContext =
--   SpanContext
--     defaultTraceFlags
--     False
--     "00000000000000000000000000000000"
--     "0000000000000000"
--     TraceState.empty
--
-- data MessageAttributes = MessageAttributes
--   { messageType :: Text
--   , messageParameters :: [PrimitiveAttribute]
--   }
--
-- class (Protocol ps) => OTelProtocol ps where
--   protocolName :: Proxy ps -> Text
--   messageAttributes :: PeerHasAgency pr st -> Message ps st st' -> MessageAttributes
--
-- renderTypedProtocolSelectorOTel
--   :: forall ps. (OTelProtocol ps) => RenderSelectorOTel (TypedProtocolsSelector ps)
-- renderTypedProtocolSelectorOTel = \case
--   ReceiveSelector tok msg ->
--     OTelRendered
--       { eventName = "receive " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
--       , eventKind = Consumer
--       , renderField = const []
--       }
--   CallSelector tok msg ->
--     OTelRendered
--       { eventName = "call " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
--       , eventKind = Client
--       , renderField = const []
--       }
--   RespondSelector tok msg ->
--     OTelRendered
--       { eventName = "respond " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
--       , eventKind = Server
--       , renderField = const []
--       }
--   CastSelector tok msg ->
--     OTelRendered
--       { eventName = "cast " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
--       , eventKind = Producer
--       , renderField = const []
--       }
--   CloseSelector tok msg ->
--     OTelRendered
--       { eventName = "close " <> protocolName (Proxy @ps) <> ":" <> messageType (messageAttributes tok msg)
--       , eventKind = Producer
--       , renderField = const []
--       }
