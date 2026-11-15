{-# LANGUAGE CPP #-}

module Report.Flush (flush) where

flush :: Bool -> IO ()
flush verbose =
#if defined(TRACE)
  when verbose $
    do
      traceIO "flushing"
      hFlush stdout
#else
  when verbose $
    hFlush stdout
#endif
