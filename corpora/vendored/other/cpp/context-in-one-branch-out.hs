{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}

-- A context in one branch only, before a type written over several lines.
module Terminal.Trace where

import GHC.Stack (HasCallStack)

traceMessage ::
#ifdef WITH_CALLSTACK
  (HasCallStack) =>
#endif
  String ->
  [(String, Int)] ->
  IO ()
traceMessage message _pairs = putStrLn message

traceLevel ::
  forall a.
#ifdef WITH_CALLSTACK
  (HasCallStack) =>
#endif
  (Show a) =>
  a ->
  IO ()
traceLevel = print
