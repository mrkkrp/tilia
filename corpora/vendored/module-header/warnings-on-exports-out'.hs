module Archive.Store
    ( {-# DEPRECATED "Use put instead." #-} store
    , put
    , Entry (..)
    )
where

store :: Entry -> IO ()
store = put

put :: Entry -> IO ()
put _ = pure ()
