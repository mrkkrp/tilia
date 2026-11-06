{-# LANGUAGE CPP #-}

module Xeno.DOM.Robust (parse) where

parse :: Process
parse =
  Process
    {
#if MIN_VERSION_bytestring(0,11,0)
    openF = \(BS name_start name_len) -> do
      index <- readRef sizeRef
      write index name_start name_len
    , textF = \(BS text_start text_len) -> do
        write 0 text_start text_len
    , closeF = \closeTag@(BS _ _) -> do
        parent <- readRef parentRef
        let openTag = BS (plusForeignPtr offset0 parent) 0
        pure (openTag == closeTag)
#else
    openF = \(PS _ name_start name_len) -> do
      index <- readRef sizeRef
      write index name_start name_len
    , textF = \(PS _ text_start text_len) -> do
        write 0 text_start text_len
    , closeF = \closeTag@(PS s _ _) -> do
        parent <- readRef parentRef
        let openTag = PS s (parent + offset0) 0
        pure (openTag == closeTag)
#endif
    }
