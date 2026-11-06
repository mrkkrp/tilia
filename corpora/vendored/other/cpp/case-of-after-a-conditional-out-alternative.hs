{-# LANGUAGE CPP #-}

module Lookup (check, find) where

check :: IO ()
check = do
  case answer
#ifdef SECOND
    second of
#else
    first of
#endif
    _ -> pure ()

find :: IO ()
find = do
  case lookupModule
#if __GLASGOW_HASKELL__ >= 900
    unitState
#else
    flags
#endif
    (moduleName "Polysemy")
#if __GLASGOW_HASKELL__ >= 904
    NoQualifier of
#else
    Nothing of
#endif
    Found _ -> pure ()
    _ -> fail "not found"
