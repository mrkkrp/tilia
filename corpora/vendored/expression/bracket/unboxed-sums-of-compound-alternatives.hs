{-# LANGUAGE UnboxedSums #-}

module Lexer.Span where

located :: Int -> Int -> (# (Int, Int) | () #)
located from to = (# (from, to) | #)

shifted :: Int -> (# Int | [Int] #)
shifted n = (# | [n, n + 1] #)
