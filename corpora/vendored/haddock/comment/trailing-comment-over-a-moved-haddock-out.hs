module Cache.Stats where

data Stats = Stats
  { hits :: Int,
    -- Misc

    -- | lookups that found nothing
    misses :: Int
  }
