module Cache.Stats where

data Stats = Stats
    { hits :: Int
    , -- Misc
      misses :: Int
    -- ^ lookups that found nothing
    }
