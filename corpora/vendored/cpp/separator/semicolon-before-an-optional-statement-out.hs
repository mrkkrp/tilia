{-# LANGUAGE CPP #-}

module Toolchain.Probe where

probe :: IO ()
probe = do
  { detect
#if defined(mingw32_HOST_OS)
  ; registry
#endif
  ; report
  }
