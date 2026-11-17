{-# LANGUAGE CPP #-}

module Fault.Impossible where

instance Exception Impossible where
#if !MIN_VERSION_base(4,20,0)
#else
  backtraceDesired _ = False
#endif
