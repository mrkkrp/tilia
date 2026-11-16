{-# LANGUAGE CPP #-}
#ifdef UNICODE
{-# LANGUAGE UnicodeSyntax #-}
#endif
module Text.Glyph (width) where

width :: String -> Int
width = length
