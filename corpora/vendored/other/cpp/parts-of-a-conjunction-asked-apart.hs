{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if defined(WIDE_CELLS) && defined(HAVE_UNICODE)
{-# LANGUAGE UnboxedSums #-}
#endif
module Terminal.Cell where

#if defined(WIDE_CELLS)
#if defined(HAVE_UNICODE)
width   ::  (# Char | Int #) -> Int
width (# _ | #)  =  2
width (# | n #)  =  n
#endif
#endif
