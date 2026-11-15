module Cache.Hit where

hits :: Int
hits = 0 -- counted since the last eviction
-- | Whether the last lookup found anything.
found :: Bool
found = True
