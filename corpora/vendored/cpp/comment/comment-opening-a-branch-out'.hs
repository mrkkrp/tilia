{-# LANGUAGE CPP #-}

module Data.Folds (total, weights) where

import Data.List
    ( nub
    , sortOn
#if defined(STRICT)
    , -- strict, unlike foldl
      foldl'
#endif
    )

total :: [Int] -> Int
total = foldr (+) 0 . nub . sortOn negate

weights :: [Int]
weights =
    [ 1
    , 2
#if defined(HEAVY)
    , {- heavy -} 10
#endif
    , 3
    ]
