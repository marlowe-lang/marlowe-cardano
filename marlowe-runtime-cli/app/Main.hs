{-# LANGUAGE CPP #-}

module Main where

#ifdef mingw32_HOST_OS
import Control.Concurrent.STM (retry)
#else
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
#endif
import GHC.IO.Handle (hSetBuffering)
import Language.Marlowe.Runtime.CLI.Command
import System.IO (BufferMode (..), stderr, stdout)
import System.Posix (Handler (..), installHandler, sigINT)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options@Options{..} <- getOptions

-- TODO Windows support
#ifdef mingw32_HOST_OS
  let sigInt = retry
#else
  sigIntVar <- newEmptyTMVarIO
  _ <- installHandler sigINT (CatchOnce $ atomically $ putTMVar sigIntVar ()) Nothing
  let sigInt = takeTMVar sigIntVar
#endif
  runCLIWithOptions sigInt options $ runCommand cmd
