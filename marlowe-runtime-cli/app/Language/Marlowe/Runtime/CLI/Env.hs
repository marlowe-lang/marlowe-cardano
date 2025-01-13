{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.CLI.Env where

import Control.Concurrent.STM (STM)

-- | The environment for the Marlowe Runtime CLI.
newtype Env = Env
  { sigInt :: STM ()
  }
