{-# LANGUAGE CPP #-}

module T where

f :: Int -> IO Int
f x = do
#if MIN_VERSION_base(4,0,0)
  y <-
    g
      (h x)
      a
      b
#else
  y <-
    k
      (h x)
      a
      b
#endif
  pure y
