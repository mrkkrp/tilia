module Bag.Store where

class Store s where
  empty :: s
  insert :: Int -> s -> s
  singleton :: Int -> s
  (<+>) :: s -> s -> s
  fromList :: [Int] -> s
  {-# MINIMAL (empty, insert) | (singleton, (<+>)) | fromList #-}

class Lookup s where
  member :: Int -> s -> Bool
  count :: Int -> s -> Int
  elems :: s -> [Int]
  (!?) :: s -> Int -> Maybe Int
  {-# MINIMAL
        elems, ( member
               | count )
      | ( (!?)
        , member
        ) #-}
