{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Network.Protocol.Singleton where

import Data.Kind (Constraint, Type)
import Network.TypedProtocol hiding (FlipAgency, TheyHaveAgency)

type SingClientAgency :: forall ps. ps -> Constraint
class SingClientAgency st where
  singClientAgency :: ClientAgency st

type SingServerAgency :: forall ps. ps -> Constraint
class SingServerAgency st where
  singServerAgency :: ServerAgency st

type SingNobodyAgency :: forall ps. ps -> Constraint
class SingNobodyAgency st where
  singNobodyAgency :: NobodyAgency st

data SPeerRole (pr :: PeerRole) where
  SAsClient :: SPeerRole 'AsClient
  SAsServer :: SPeerRole 'AsServer

class SingPeerRole (pr :: PeerRole) where
  singPeerRole :: SPeerRole pr

class SingWeHaveAgency pr st where
  singWeHaveAgency :: WeHaveAgency pr st

instance (SingClientAgency st) => SingWeHaveAgency 'AsClient st where
  singWeHaveAgency = ClientAgency singClientAgency

instance (SingServerAgency st) => SingWeHaveAgency 'AsServer st where
  singWeHaveAgency = ServerAgency singServerAgency

type family FlipAgency (pr :: PeerRole) = (r :: PeerRole) | r -> pr where
  FlipAgency 'AsClient = 'AsServer
  FlipAgency 'AsServer = 'AsClient

type TheyHaveAgency pr = ActiveAgency (FlipAgency pr)

class SingTheyHaveAgency pr st where
  singTheyHaveAgency :: TheyHaveAgency pr st

instance (SingServerAgency st) => SingTheyHaveAgency 'AsClient st where
  singTheyHaveAgency = ServerAgency singServerAgency

instance (SingClientAgency st) => SingTheyHaveAgency 'AsServer st where
  singTheyHaveAgency = ClientAgency singClientAgency

class OurRole (pr :: PeerRole) where
  type OurAgency pr :: ps -> Type
  ourAgency :: OurAgency pr (st :: ps) -> WeHaveAgency pr st

class TheirRole (pr :: PeerRole) where
  type TheirAgency pr :: ps -> Type
  theirAgency :: TheirAgency pr (st :: ps) -> TheyHaveAgency pr st

instance OurRole 'AsClient where
  type OurAgency 'AsClient = ClientAgency
  ourAgency = ClientAgency

instance OurRole 'AsServer where
  type OurAgency 'AsServer = ServerAgency
  ourAgency = ServerAgency

instance TheirRole 'AsClient where
  type TheirAgency 'AsClient = ServerAgency
  theirAgency = ServerAgency

instance TheirRole 'AsServer where
  type TheirAgency 'AsServer = ClientAgency
  theirAgency = ClientAgency
