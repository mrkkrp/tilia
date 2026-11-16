module Ledger.Balance where

infixr 2 ⊕

(⊕) :: Int -> Int -> Int
a ⊕ b = a + b

settle :: Int -> Int -> Int -> Int
settle x y z =
    x
        ⊕ y
            + z

-- Bound in a where without a fixity declaration, the operator is infixl 9.
settleLocally :: Int -> Int -> Int -> Int
settleLocally x y z =
    x ⊕ y
        + z
    where
        a ⊕ b = a * b
