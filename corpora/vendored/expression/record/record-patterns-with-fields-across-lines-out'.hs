{-# LANGUAGE NamedFieldPuns #-}

module Warehouse.Pallet where

firstCount :: Pallet -> Int
firstCount
    Pallet
        { palletCrates =
            Crate{crateCount} : _
        } = crateCount

corner :: Pallet -> (Int, Int)
corner Pallet{palletSpot = (row, column)} = (row, column)
