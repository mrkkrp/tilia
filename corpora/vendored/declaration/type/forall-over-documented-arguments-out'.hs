module Cache.Evict where

evict
    :: forall k
     . Ord k
    => forall v
     . Int
    -- ^ How many entries to keep.

    -- The oldest go first.
    -> Map k v
    -> Map k v
evict = undefined

refresh
    :: forall k
     . Ord k
    => forall v
     . k
    -- ^ The key to refresh.
    -> Map k v
    -> Map k v
refresh = undefined
