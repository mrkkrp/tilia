#!/usr/bin/env stack
{- stack script
     --snapshot lts-22.21
     --package containers
-}

-- Count how often each word occurs in the input.
module Main (main) where

import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- getContents
  let counts = Map.fromListWith (+) [(w, 1 :: Int) | w <- words input]
  mapM_ print (Map.toList counts)
