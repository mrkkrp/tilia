{-# LANGUAGE CPP #-}
#if defined mingw32_HOST_OS
{-# LANGUAGE MagicHash #-}
#endif

-- defined takes its name with or without parentheses, and an answer in one
-- spelling settles a question asked in the other.
module Path.Drive where

drive :: Int
#if defined(mingw32_HOST_OS) && !defined FORCE_POSIX
drive = I# 3#
#else
drive = 0
#endif
