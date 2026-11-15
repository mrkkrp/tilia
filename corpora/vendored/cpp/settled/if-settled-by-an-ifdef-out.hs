{-# LANGUAGE CPP #-}
#ifdef USE_ST
{-# LANGUAGE RankNTypes #-}
#endif

module Run where

#if USE_ST
run :: (forall s. s -> s) -> Int
run f = f 1
#endif

count :: Int -> String
count n = "count: " ++ show n
