{-# LANGUAGE CPP #-}

module System.Signal (Signal (..), Mask (..)) where

data Signal
    = Hangup
    | Interrupt
#if !defined(mingw32_HOST_OS)
    | User1
#endif
    | Terminate

data Mask
    = Blocked
#ifdef POSIX
    | Masked
#else
    | Ignored
#endif
    | Delivered
