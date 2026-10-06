{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if defined(WIDE_CELLS) && defined(HAVE_UNICODE)
{-# LANGUAGE UnboxedSums #-}
#endif

module Terminal.Cell where

import Data.Char (ord)

#if defined(WIDE_CELLS)
#if defined(HAVE_UNICODE)
width :: (# Char | Int #) -> Int
width (# _ | #) = 2
width (# | n #) = n
#else
width :: Int -> Int
width = id
#endif

columns :: Int
#if MIN_VERSION_base(4,10,0)
columns = 80
#else
columns = 72
#endif
#endif

code :: Char -> Int
code = ord
