{-# LANGUAGE TypeFamilies #-}

module Grid.Text where

data family Grid row

data instance
  Grid
    [Char]
  = Text
      ( Int,
        Int
      )
      [Char]
