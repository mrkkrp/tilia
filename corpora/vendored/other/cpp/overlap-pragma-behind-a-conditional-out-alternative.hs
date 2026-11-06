{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Route.Stop where

class Describe a where
  describe :: a -> String

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  (Show a) => Describe [a] where
  describe = unwords . map show

newtype Stop = Stop Int

deriving instance
#ifdef WITH_OVERLAPS
  {-# OVERLAPPING #-}
#endif
  Show Stop
