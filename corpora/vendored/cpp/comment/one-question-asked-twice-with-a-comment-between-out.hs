{-# LANGUAGE CPP #-}

module Terminal.Console (isConsole) where

#ifdef WINDOWS
import System.Win32.Console (getConsoleMode)
#endif

-- Everything below works the same everywhere.

#ifdef WINDOWS
isConsole :: Handle -> IO Bool
isConsole h = (/= 0) <$> getConsoleMode h
#endif
