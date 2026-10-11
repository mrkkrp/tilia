{-# LANGUAGE CPP #-}

module Lookup (check, find) where

check :: IO ()
check = do
  case answer
#ifdef SECOND
    second
#else
    first
#endif
    of
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
    NoQualifier
#else
    Nothing
#endif
    of
    Found _ -> pure ()
    _ -> fail "not found"
