module Cache.Warm where

{- | Fill the cache before the first request arrives. -}
-- TODO: measure whether this still earns its keep
warm :: IO ()
warm = pure ()
