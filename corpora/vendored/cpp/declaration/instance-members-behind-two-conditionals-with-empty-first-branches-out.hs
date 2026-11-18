{-# LANGUAGE CPP #-}

module Fault.Impossible where

instance Exception Impossible where
#ifdef OLD_BASE
#else
  backtraceDesired _ = False
#endif
#ifdef PLAIN_ERRORS
#else
  displayException _ = "impossible"
#endif
