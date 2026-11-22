{-# LANGUAGE CPP #-}

-- \| Count what the standard input holds. With no module line, GHC ignores
-- this Haddock in every configuration.

import Data.List (genericLength)

main :: IO ()
main = interact (show . count)

#ifdef COUNT_WORDS
count :: String -> Integer
count = genericLength . words
#else
count :: String -> Integer
count = genericLength . lines
#endif
