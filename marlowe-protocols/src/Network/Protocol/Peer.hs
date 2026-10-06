{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer where

import Network.TypedProtocol.Peer (Peer (..), Receiver (..))

hoistPeer :: (Functor m) => (forall x. m x -> n x) -> Peer protocol pr pl st m a -> Peer protocol pr pl st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
  YieldPipelined wa msgs receiver peer -> YieldPipelined wa msgs (hoistReceiver f receiver) $ hoistPeer f peer
  Collect contNoMsg contReceived ->
    Collect
      (hoistPeer f <$> contNoMsg)
      (hoistPeer f . contReceived)

hoistReceiver
  :: (Functor m)
  => (forall x. m x -> n x)
  -> Receiver protocol pr st stdone m c
  -> Receiver protocol pr st stdone n c
hoistReceiver f = \case
  ReceiverEffect m -> ReceiverEffect $ f $ hoistReceiver f <$> m
  ReceiverDone c -> ReceiverDone c
  ReceiverAwait ta handle -> ReceiverAwait ta $ \msg -> hoistReceiver f (handle msg)
