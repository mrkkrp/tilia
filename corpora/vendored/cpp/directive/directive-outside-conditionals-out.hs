{-# LANGUAGE CPP #-}

module Terminal.Size where

#include "terminal.h"

#if defined(HAVE_IOCTL)
rows :: Int
rows = 24
#else
rows :: Int
rows = 0
#endif

#if defined(HAVE_TERMCAP)
columns :: Int
columns = 80
#else
columns :: Int
columns = 0
#endif
