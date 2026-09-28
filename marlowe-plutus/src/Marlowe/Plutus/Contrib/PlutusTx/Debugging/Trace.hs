{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -fno-full-laziness #-}
{-# OPTIONS -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fno-omit-interface-pragmas #-}
{-# OPTIONS -fno-spec-constr #-}
{-# OPTIONS -fno-specialise #-}
{-# OPTIONS -fno-strictness #-}
{-# OPTIONS -fno-unbox-small-strict-fields #-}
{-# OPTIONS -fno-unbox-strict-fields #-}


module Marlowe.Plutus.Contrib.PlutusTx.Debugging.Trace where

#ifdef TRACE_GHC
import Data.Text qualified as Text
import qualified Debug.Trace as Debug
import qualified Prelude as H
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
#else
import qualified PlutusTx.Prelude as P
#endif

trace
#ifdef TRACE_GHC
  :: Builtins.BuiltinString
#else
  :: P.BuiltinString
#endif
  -> a -> a
#ifdef TRACE_GHC
trace s = Debug.trace (Text.unpack (Builtins.fromBuiltin s))
#else
{-# INLINEABLE trace #-}
trace = P.trace
#endif

traceError
#ifdef TRACE_GHC
  :: Builtins.BuiltinString
#else
  :: P.BuiltinString
#endif
  -> a
#ifdef TRACE_GHC
traceError s = H.error (Text.unpack (Builtins.fromBuiltin s))
#else
{-# INLINEABLE traceError #-}
traceError = P.traceError
#endif

traceIfFalse
#ifdef TRACE_GHC
  :: Builtins.BuiltinString -> H.Bool -> H.Bool
#else
  :: P.BuiltinString -> P.Bool -> P.Bool
#endif
#ifdef TRACE_GHC
traceIfFalse s H.False = trace s H.False
traceIfFalse _ H.True = H.True
#else
{-# INLINEABLE traceIfFalse #-}
traceIfFalse = P.traceIfFalse
#endif

traceVerbose
#ifdef TRACE_GHC
  :: Builtins.BuiltinString -> Builtins.BuiltinString -> a -> a
#else
  :: P.BuiltinString -> P.BuiltinString -> a -> a
#endif
#ifdef TRACE_GHC
traceVerbose verbose _ = trace verbose
#else
{-# INLINEABLE traceVerbose #-}
traceVerbose _ = trace
#endif

traceErrorVerbose
#ifdef TRACE_GHC
  :: Builtins.BuiltinString -> Builtins.BuiltinString -> a
#else
  :: P.BuiltinString -> P.BuiltinString -> a
#endif
#ifdef TRACE_GHC
traceErrorVerbose verbose _ = traceError verbose
#else
{-# INLINEABLE traceErrorVerbose #-}
traceErrorVerbose _ = traceError
#endif

traceIfFalseVerbose
#ifdef TRACE_GHC
  :: Builtins.BuiltinString -> Builtins.BuiltinString -> H.Bool -> H.Bool
#else
  :: P.BuiltinString -> P.BuiltinString -> P.Bool -> P.Bool
#endif
#ifdef TRACE_GHC
traceIfFalseVerbose verbose _ = traceIfFalse verbose
#else
{-# INLINEABLE traceIfFalseVerbose #-}
traceIfFalseVerbose _ = traceIfFalse
#endif

#ifdef TRACE_GHC
showDebug :: (H.Show a) => a -> Builtins.BuiltinString
showDebug = BI.BuiltinString H.. Text.pack H.. H.show
#else
{-# INLINEABLE showDebug #-}
showDebug :: a -> P.BuiltinString
showDebug _ = P.emptyString
#endif
