{-# LANGUAGE CPP #-}

module Terminal.Size where

#ifndef NO_CALLSTACK
import GHC.Stack
#define TRACED(ty) HasCallStack => ty
#endif

newtype Size = Size Int

-- A comment with nothing written under it.
