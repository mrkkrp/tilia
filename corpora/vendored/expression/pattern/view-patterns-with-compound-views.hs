{-# LANGUAGE ViewPatterns #-}

module Inventory.Search where

hasStock :: Shelf -> Bool
hasStock (filter available . items -> _ : _) = True
hasStock _ = False

doubled :: [Int] -> [Int]
doubled (map (* 2) -> counts) = counts
