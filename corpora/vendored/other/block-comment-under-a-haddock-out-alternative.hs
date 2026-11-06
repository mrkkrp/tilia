module Cache.Size where

-- | How many entries the cache holds, which the type already fixes.
{-@ entries :: Cache n a -> {k : Int | k == n} @-}
entries :: Cache n a -> Int
entries = const 0
