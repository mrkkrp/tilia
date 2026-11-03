-- A let that runs past column 80 is broken before its in.
module Loom.Lets where

tension loom = let threads = loomWarp loom in sum (map threadTension threads) `div` length threads
