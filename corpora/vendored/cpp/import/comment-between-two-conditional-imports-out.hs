{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

-- A comment set apart between two conditionals of imports stays between
-- them, rather than going with the later conditional into each branch of
-- the earlier one.
module Parser.Debug where

import "this" Parser.Core (Parser)

#if defined(__GLASGOW_HASKELL__)
import System.IO.Unsafe (unsafePerformIO)
#endif

-- Tracing, only when debugging.
#if defined(DEBUG)
import Debug.Trace (trace)
#endif

run :: Parser a -> a
run = undefined
