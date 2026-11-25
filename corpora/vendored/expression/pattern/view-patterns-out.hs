{-# LANGUAGE ViewPatterns #-}

module Inventory.Stock where

isEmpty :: Shelf -> Bool
isEmpty (count -> 0) = True
isEmpty _ = False

restock :: ((Shelf, Supplier), Shelf) -> Bool
restock ((shelf, _), count -> 0) = True
restock _ = False

oversized :: Item -> Bool
oversized
  ( dimensions ->
      Box
        width
        height
  ) = width * height > 100
