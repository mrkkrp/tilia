{-# LANGUAGE Arrows #-}

module Cache.Refresh where

refresh mode = proc key -> do
  fresh <-
    fetch
      -< case key of
        Pinned k -> k
        Loose k -> normalise k
  stale <-
    key
      >- case mode of
        Eager -> fetchNow
        Lazy -> fetchLater
  returnA -< (fresh, stale)
