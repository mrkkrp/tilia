{-# LANGUAGE CPP #-}

module Terminal.Keymap where

import Data.Maybe (fromMaybe)

#ifdef ORDERED
lookupKey :: Int -> [(Int, String)] -> String
lookupKey k = fromMaybe "" . lookup k
#else
import qualified Data.IntMap as IntMap

lookupKey :: Int -> [(Int, String)] -> String
lookupKey k = fromMaybe "" . IntMap.lookup k . IntMap.fromList
#endif

defaultKey :: Int
defaultKey = 13

backend :: String
#ifdef ORDERED
backend = "list"
#else
backend = "intmap"
#endif
