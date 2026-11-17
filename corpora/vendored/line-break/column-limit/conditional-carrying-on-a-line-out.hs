-- The alternatives of a conditional that carry on the line before the
-- conditional are laid out a step right of that line, and only then broken
-- where they run past column 80.
{-# LANGUAGE CPP #-}

module Report.Flush (flush) where

flush :: Bool -> IO ()
flush verbose =
#if defined(TRACE)
  when verbose $
    do
      traceIO
        "flushing the standard output and the standard error streams first"
      hFlush stdout
#else
  when verbose $
    hFlush stdout
#endif
