{-# LANGUAGE CPP #-}

module Flags where

flags :: [Flag]
flags =
  [ verbose,
#ifdef WINDOWS
#if WORD_SIZE_IN_BITS == 64
    wide,
#else
    narrow,
#endif
#else
    posix,
#endif
    quiet
  ]
