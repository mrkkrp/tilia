{-# LANGUAGE CPP #-}

module Xeno.DOM.Robust (parse) where

parse :: Process
parse = Process {
#if MIN_VERSION_bytestring(0,11,0)
    openF = \(BS name_start name_len) -> do
#else
    openF = \(PS _ name_start name_len) -> do
#endif
      index <- readRef sizeRef
      write index name_start name_len
#if MIN_VERSION_bytestring(0,11,0)
  , textF = \(BS text_start text_len) -> do
#else
  , textF = \(PS _ text_start text_len) -> do
#endif
      write 0 text_start text_len
#if MIN_VERSION_bytestring(0,11,0)
  , closeF = \closeTag@(BS _ _) -> do
#else
  , closeF = \closeTag@(PS s _ _) -> do
#endif
      parent <- readRef parentRef
#if MIN_VERSION_bytestring(0,11,0)
      let openTag = BS (plusForeignPtr offset0 parent) 0
#else
      let openTag = PS s (parent + offset0) 0
#endif
      pure (openTag == closeTag)
  }
