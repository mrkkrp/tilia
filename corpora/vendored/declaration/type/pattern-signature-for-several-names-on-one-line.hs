{-# LANGUAGE PatternSynonyms #-}

module Colour.Named where

data Colour = Colour Int Int Int

pattern Black,   White :: Colour
pattern Black = Colour 0 0 0
pattern White = Colour 255 255 255
