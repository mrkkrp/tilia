{-# LANGUAGE LexicalNegation #-}

-- With LexicalNegation, a minus right before an operand negates it, so
-- f -x applies f to the negation of x and f - x subtracts.
module Thermostat.Swing where

swing :: Int -> Int
swing x = offset -x

drop' :: Int -> Int
drop' x = offset - x

coldest :: Int
coldest = -40

flipped :: Int -> Int
flipped t = abs -t + max -1 t
