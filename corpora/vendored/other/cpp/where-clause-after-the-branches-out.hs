{-# LANGUAGE CPP #-}

-- A where clause written once, after the branches of an equation, stays
-- there rather than going into each of them.
module Cache.Evict where

evict :: Int -> Int
#if MIN_VERSION_base(4,16,0)
evict n = keep (n - 1)
#else
evict n = keep n
#endif
  where
    keep = max 0

-- Guards in one branch and not the other.
shrink :: Int -> Int
#if MIN_VERSION_base(4,16,0)
shrink n
  | n > limit = limit
  | otherwise = n
#else
shrink n = min n limit
#endif
  where
    limit = 64

-- The same for an alternative of a case.
stale :: Maybe Int -> Int
stale m = case m of
#if MIN_VERSION_base(4,16,0)
  Just n -> age n
#else
  Just n -> age (n + 1)
#endif
    where
      age = subtract 1
  Nothing -> 0

-- A where clause in each branch stays in each.
expire :: Int -> Int
#if MIN_VERSION_base(4,16,0)
expire n = ttl n
  where
    ttl = (* 2)
#else
expire n = ttl n
  where
    ttl = id
#endif
