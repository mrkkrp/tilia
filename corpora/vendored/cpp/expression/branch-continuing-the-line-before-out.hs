{-# LANGUAGE CPP #-}

module Report.Flush (flush) where

flush :: Bool -> IO ()
flush verbose =
  when verbose $
#if defined(TRACE)
    do
      traceIO "flushing"
      hFlush stdout
#else
    hFlush stdout
#endif
