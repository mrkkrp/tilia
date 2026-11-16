{-# LANGUAGE CPP #-}
#ifdef UNICODE
{-# LANGUAGE UnicodeSyntax #-}
#endif

module Text.Glyph (width) where

#ifdef UNICODE
width ∷ String → Int
#else
width :: String -> Int
#endif
width = length
