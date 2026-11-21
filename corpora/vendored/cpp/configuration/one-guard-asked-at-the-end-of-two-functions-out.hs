{-# LANGUAGE CPP #-}

module Outcome (describe, retry) where

describe :: Int -> String
describe r = case r of
  0 -> "success"
  _ -> "failure"
#if MIN_VERSION_QuickCheck(2,12,0)
#else
  1 -> "insufficient coverage"
#endif

retry :: Int -> Bool
retry r = case r of
  0 -> False
  _ -> True
#if MIN_VERSION_QuickCheck(2,12,0)
#else
  1 -> True
#endif
