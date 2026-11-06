module Cache.Evict where

-- | Drop the entries nothing has asked for lately.

-- TODO: the threshold wants to come from the configuration
evict :: IO ()
evict = pure ()
