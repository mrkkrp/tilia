{-# LANGUAGE CPP #-}

module Census.Count where

adults :: [(String, Int)] -> [String]
adults people =
    [ name
#include "census-filter.h"
    | (name, age) <- people
    , age >= 18
    ]

households :: [[String]] -> Int
households hs =
    let sizes = fmap length hs
#define WEIGHTED 1
        total = sum sizes
     in total

label
    :: Int
#define LABEL_WIDTH 12
    -> String
label = show
