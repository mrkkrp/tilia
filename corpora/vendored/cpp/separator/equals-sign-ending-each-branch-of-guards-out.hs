{-# LANGUAGE CPP #-}

module Data.Set.Refined where

member :: Key -> Set -> Bool
member k s
#if MIN_VERSION_containers(0,6,0)
  | Just i <- lookupIndex k s =
#else
  | Just i <- findIndex k s =
#endif
      i >= 0
  | otherwise = False
