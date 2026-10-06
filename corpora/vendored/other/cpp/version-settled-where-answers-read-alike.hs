{-# LANGUAGE CPP #-}

-- No answer turns an extension on, but taking no branch of the first
-- conditional and the branch of the second leaves a comma with nothing
-- before it.
module Data.Compat
  (
#if !(MIN_VERSION_base(4,16,0))
    Solo (..)
#endif
#if !(MIN_VERSION_base(4,14,0))
  , Pair (..)
#endif
  ) where

#if !(MIN_VERSION_base(4,16,0))
newtype Solo a = Solo a
#endif
#if !(MIN_VERSION_base(4,14,0))
data Pair a = Pair a a
#endif
