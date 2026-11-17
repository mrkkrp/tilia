{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}

module Circuit.Feed where

feed = do { step <- pure $ proc x -> do { returnA -< x }; run step }

relay = proc x -> do { y <- send -< do { encode x }; returnA -< y }

echo = proc x -> do { (\y -> send -< y) do { encode x }; returnA -< x }
