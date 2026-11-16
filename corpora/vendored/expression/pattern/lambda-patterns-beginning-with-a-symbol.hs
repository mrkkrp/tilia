{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Stream.Step where

ignoring :: a -> b -> a
ignoring = \ ~x  ~_ -> x

forcing :: Int -> Int -> Int
forcing = \ !acc  !x -> acc + x

quoted :: Int -> Int
quoted = \ $([p|n|]) -> n + 1
