{-# LANGUAGE CPP #-}

module Ledger where

import qualified Data.Map.Strict as Map

-- Specialised for the accounts the ledger keeps.
#if __GLASGOW_HASKELL__ >= 900
{-# SPECIALIZE Map.insertWith :: (Int -> Int -> Int) -> Account -> Int -> Map.Map Account Int -> Map.Map Account Int #-}
#endif
{-# SPECIALIZE Map.lookup :: Account -> Map.Map Account Int -> Maybe Int #-}

newtype Account = Account Int
