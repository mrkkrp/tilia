{-# LANGUAGE CPP #-}

-- A comment set apart at the end of a branch of imports, before imports
-- that follow the #endif, comes out the same on every formatting.
module Tags.Main where

#if defined(GHC_API)
import Data.Maybe (fromMaybe)

-- These modules cannot be imported without the GHC API.

#endif
import Data.List (sortOn)

main :: IO ()
main = pure ()
