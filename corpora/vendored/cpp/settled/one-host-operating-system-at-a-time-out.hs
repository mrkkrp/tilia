{-# LANGUAGE CPP #-}

module Kiln.Display (display) where

-- GHC defines the macro of one operating system, so no configuration takes
-- both conditionals, and the import never follows a declaration.
#ifdef darwin_HOST_OS
screen :: Int
screen = 0
#endif

#ifdef linux_HOST_OS
import Data.Maybe (fromMaybe)
#endif

display :: Maybe String -> String
display = maybe "" id
