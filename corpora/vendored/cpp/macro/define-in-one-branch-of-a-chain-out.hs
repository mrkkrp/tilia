{-# LANGUAGE CPP #-}

module Platform.Word where

#if defined(i386_HOST_ARCH)
#define WORD_BYTES 4
wordBytes :: Int
wordBytes = 4
#elif defined(x86_64_HOST_ARCH)
#define WORD_BYTES 8
wordBytes :: Int
wordBytes = 8
#else
wordBytes :: Int
wordBytes = 0
#endif
