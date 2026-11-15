{-# LANGUAGE CPP #-}

module Terminal.Trace where

import GHC.Stack (HasCallStack)

traceMessage ::
#ifdef WITH_CALLSTACK
  (HasCallStack) =>
#endif
  String -> [(String, Int)] -> Maybe String -> Either String Int -> IO ()
traceMessage message _pairs _fallback _outcome  =  putStrLn message
