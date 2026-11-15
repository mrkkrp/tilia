{-# LANGUAGE CPP #-}

module Network.Address where

data Address = Address
  { -- | @www.haskell.org@
    addressHost :: String,
    -- | @42@
    addressPort :: Int
  }
#if __GLASGOW_HASKELL__ >= 702
  deriving (Eq, Ord, Show, Generic)
#else
  deriving (Eq, Ord, Show)
#endif
