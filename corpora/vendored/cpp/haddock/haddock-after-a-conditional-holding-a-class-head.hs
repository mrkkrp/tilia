{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Example where

-- A method's Haddock and signature written once, after a conditional that
-- holds the head of the class in each branch.
#ifdef ENABLE_HOOK_ARGS
class Example e where
  type Arg e
#else
class Example e where
#endif
  -- | Evaluates an example.
  evaluate :: e -> IO Bool
