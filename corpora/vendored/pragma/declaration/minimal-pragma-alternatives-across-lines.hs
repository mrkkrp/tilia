module Stream.Source where

class Source s where
  next :: s -> Maybe (Int, s)
  peek :: s -> Maybe Int
  skip :: s -> s
  toList :: s -> [Int]
  fold :: (Int -> b -> b) -> b -> s -> b
  {-# MINIMAL
        next
      | peek, skip -- one element at a time
      | toList | fold #-}

