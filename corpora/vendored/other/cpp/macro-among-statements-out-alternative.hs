{-# LANGUAGE CPP #-}

-- A use standing for a statement stays at the column of the statements
-- around it.
module Solver.Trail where

#ifdef ENABLE_TRACE
#define TRACING(x) x
#else
#define TRACING(x)
#endif

step :: Int -> IO ()
step n = do
  print n
  TRACING(print n)
  print (n + 1)

steps :: [Int] -> IO ()
steps ns = do
  let total = sum ns
  TRACING(print total)
  forM_ ns $ \n -> do
    step n
    TRACING(print n)
    step (n + 1)
  pure ()
