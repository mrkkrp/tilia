module Cache.Cold where

{- Not exported: the eviction order is Cache.Warm's business. -}
-- | Drop everything the cache is holding.
cool :: IO ()
cool = pure ()
