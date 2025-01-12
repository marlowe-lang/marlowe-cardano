{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.CLI.Monad where

import Control.Monad (MonadPlus, (>=>))
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class (MonadMarlowe (..))
import Control.Monad.Trans.Reader (ReaderT)
import Language.Marlowe.Protocol.Client (hoistMarloweRuntimeClient)
import Language.Marlowe.Runtime.CLI.Env (Env (..))
import Options.Applicative (Alternative)
import System.Exit (die)
import UnliftIO (MonadUnliftIO)

-- | A monad type for Marlowe Runtime CLI programs.
newtype CLI a = CLI {runCLI :: MarloweT (ReaderT Env IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadFail
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadBase IO
    , MonadBaseControl IO
    , MonadUnliftIO
    )

instance MonadMarlowe CLI where
  runMarloweRuntimeClient client = CLI $ runMarloweRuntimeClient $ hoistMarloweRuntimeClient runCLI client

-- | Get the environment.
askEnv :: CLI Env
askEnv = CLI ask

-- | Get a value from the environment.
asksEnv :: (Env -> a) -> CLI a
asksEnv = CLI . asks

-- | Run a CLI program in a locally modified environment.
localEnv :: (Env -> Env) -> CLI a -> CLI a
localEnv f = CLI . local f . runCLI

runCLIExceptT :: (Show e) => ExceptT e CLI a -> CLI a
runCLIExceptT =
  runExceptT >=> \case
    Left ex -> liftIO $ die $ show ex
    Right a -> pure a
