{-# LANGUAGE CPP #-}

module Terminal.Signal where

interrupt :: Int
interrupt = 2

#if defined(HAS_SIGWINCH)
resize :: Int
resize = 28
#else
resize :: Int
resize = 0
#endif
