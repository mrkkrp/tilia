{-# LANGUAGE CPP #-}

module Fault.Impossible where

instance Exception Impossible where
#ifdef QUIET
    backtraceDesired _ = False
#endif
