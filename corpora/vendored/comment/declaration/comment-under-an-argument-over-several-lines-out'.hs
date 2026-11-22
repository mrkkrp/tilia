module Pipe.Catch where

catchWith
    :: Monad m
    => ( m (Proxy a b m r)
         -> (e -> m (Proxy a b m r))
         -> m (Proxy a b m r)
       )
    -- the handler of the base monad
    -> ( Proxy a b m r
         -> (e -> Proxy a b m r)
         -> Proxy a b m r
       )
       -- what comes of it
catchWith c p0 f = go p0
