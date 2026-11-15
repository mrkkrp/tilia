{-# LANGUAGE CPP #-}

module Terminal.Palette where

#ifdef TRUE_COLOUR
colours :: Int
colours  =  16777216

#undef FALLBACK_PALETTE
#else

#define FALLBACK_PALETTE 1
#endif

palette :: [Int]
palette  =  [0 .. 15]
