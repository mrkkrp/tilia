{-# LANGUAGE CPP #-}

module Test.Params where

-- The Haddock documents the field where it is there, and nothing where it
-- is not.
data Params = Params
  { size :: Maybe Int
  , seed :: Maybe Int
  -- | Where to start running the tests.
#ifdef WITH_SKIP
  , skip :: Maybe Int
#endif
  }
