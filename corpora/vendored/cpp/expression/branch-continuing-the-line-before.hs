{-# LANGUAGE CPP #-}

module Report.Flush (flush) where

flush :: Bool -> IO ()
flush verbose = when verbose $
#if defined(TRACE)
  do traceIO "flushing"
#endif
     hFlush stdout
