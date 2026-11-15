{-# LANGUAGE NegativeLiterals #-}

-- With NegativeLiterals, -1 is a literal and - 1 a negation, so the space
-- after the minus stays.
module Thermostat.Shift where

shift :: Int -> Int
shift x = x + negate (- 1)

lowest :: Int
lowest = -128
