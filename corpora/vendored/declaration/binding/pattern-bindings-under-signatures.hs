module Grid.Bounds where

width, height :: Int
(width, height) = (80, 24)

margin, gutter :: Int
~(margin, gutter) = (2, 1)

_ = width > height

cols, rows :: Int
(cols, rows) = (w `div` cell, h `div` cell)
  where
    ~(w, h) = case screens of
      [] -> (width, height)
      s : _ -> s
    cell = 8

cells :: [(Int, Int)]
cells = [(x, y) | let (w, h) = (width, height), x <- [0 .. w], y <- [0 .. h]]
