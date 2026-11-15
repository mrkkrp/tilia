{-# LANGUAGE CPP #-}
#ifdef TRUSTWORTHY
{-# LANGUAGE Trustworthy #-}
#endif

#include "terminal.h"

module Terminal.Size where

#if MIN_VERSION_base(4, 20, 0)
rows :: Int
rows  =  24
#endif

#if MIN_VERSION_base(4, 18, 0)
columns :: Int
columns  =  80
#endif
