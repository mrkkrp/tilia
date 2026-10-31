module M where

{-# WARNING in "x-p\&artial" f "Partial" #-}
f :: Int -> Int
f = id

{-# WARNING in "x-unsafe" g "Unsafe" #-}
g :: Int -> Int
g = id
