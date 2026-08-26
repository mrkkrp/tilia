{-# LANGUAGE CPP #-}

module Terminal.Describe where

describe :: Int -> String
describe n = go n
  where
    go 0 = "none"
#ifdef VERBOSE
    go 1 = "exactly one"
#else
    go 1 = "1"
#endif
    go k = show k
