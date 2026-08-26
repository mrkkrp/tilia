{-# LANGUAGE CPP #-}

module Terminal.Capability (probe) where

import Data.Maybe (fromMaybe)
import Terminal.Encode (encode)
#ifdef WITH_TERMINFO
import Data.List (isPrefixOf)
import Terminal.Terminfo (lookupCapability)
#else
import Terminal.Static (staticCapability)
#endif

probe :: String -> String
#ifdef WITH_TERMINFO
probe name
    | "x" `isPrefixOf` name  =  encode name
    | otherwise  =  fromMaybe "" (lookupCapability name)
#else
probe name  =  fromMaybe (encode name) (staticCapability name)
#endif
