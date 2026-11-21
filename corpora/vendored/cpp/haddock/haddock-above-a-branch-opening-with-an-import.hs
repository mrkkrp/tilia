{-# LANGUAGE CPP #-}

module Terminal.Platform where

-- | Suspend the program, where the platform lets it. GHC ignores this
-- Haddock where the branch under it opens with an import.
#ifdef mingw32_HOST_OS
suspend :: IO ()
suspend = pure ()
#else
import System.Posix.Signals
suspend :: IO ()
suspend = raiseSignal sigSTOP
#endif
