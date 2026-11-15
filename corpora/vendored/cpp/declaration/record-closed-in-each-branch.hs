{-# LANGUAGE CPP #-}

module Network.Address where

data Address = Address
    { addressHost   :: String  -- ^ @www.haskell.org@
    , addressPort   :: Int     -- ^ @42@
#if __GLASGOW_HASKELL__ >= 702
    } deriving (Eq, Ord, Show, Generic)
#else
    } deriving (Eq, Ord, Show)
#endif
