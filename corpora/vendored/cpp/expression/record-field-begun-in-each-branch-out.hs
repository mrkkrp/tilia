{-# LANGUAGE CPP #-}

module Xeno.DOM (parse) where

parse :: Process
parse =
  Process
    {
#if MIN_VERSION_bytestring(0,11,0)
      openF = \(BS name_start name_len) -> do
#else
      openF = \(PS _ name_start name_len) -> do
#endif
        index <- readRef sizeRef
        write index name_start name_len
    }
