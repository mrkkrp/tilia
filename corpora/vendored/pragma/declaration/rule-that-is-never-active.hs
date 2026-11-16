module Cache.Debug where

{-# RULES "trace/drop" [ ~ ] forall s. trace s = id #-}

{-# RULES "probe/off" [~]  probe = const () #-}
