{-# LANGUAGE CPP #-}

module Terminal.Title (setTitle) where

import System.IO (hFlush, stdout)
#ifdef WITH_TERMINFO
import Terminal.Terminfo (lookupCapability)
#endif
import Control.Monad (void)

setTitle :: String -> IO ()
setTitle t = void (putStr ("\ESC]0;" ++ t ++ "\a")) >> hFlush stdout
