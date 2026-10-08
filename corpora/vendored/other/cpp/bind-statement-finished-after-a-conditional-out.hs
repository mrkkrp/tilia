{-# LANGUAGE CPP #-}

module T where

f :: Int -> IO Int
f x = do
#if MIN_VERSION_base(4,0,0)
  (y, _) <-
#else
  y <-
#endif
    g x
  pure y
