{-# LANGUAGE CPP #-}

module Fault.Impossible where

instance Exception Impossible where
#ifdef OLD_BASE
#else
    backtraceDesired _ = False
#endif
#ifdef WITH_DISPLAY
    displayException _ = "impossible"
#endif
