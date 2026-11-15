module Inventory.Restock where

restock :: Shelf -> Shelf
restock shelf = refill (capacity - left) shelf
  where
    -- what the shelf holds when full
    capacity = 40

    -- what is on it now
    left = count shelf
