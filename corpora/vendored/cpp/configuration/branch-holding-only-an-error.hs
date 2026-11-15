{-# LANGUAGE CPP #-}

module Word (wordSize) where

#if !defined(__GLASGOW_HASKELL__)
#error "Only GHC is supported."
#endif

wordSize :: Int
wordSize =
#if WORD_SIZE_IN_BITS == 64
  64
#elif WORD_SIZE_IN_BITS == 32
  32
#else
#error "Unsupported word size."
#endif
