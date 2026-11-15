{-# LANGUAGE CPP #-}

module Cache.Warm where

warm :: IO ()
warm = do
#if TRACING
  report (label 1)
  where
    label n = "warm " <> show n
#else
  pure ()
#endif
