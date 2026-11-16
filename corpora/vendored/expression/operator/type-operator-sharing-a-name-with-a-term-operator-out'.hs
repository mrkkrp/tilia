{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- A type operator the module declares does not take the fixity of the
-- term operator of its name, so + and ^ bind alike in Restock but not in
-- restock.
module Stock.Total where

data Count = None | More Count

type family (a :: Count) + (b :: Count) :: Count

type family (a :: Count) ^ (b :: Count) :: Count

type Restock a b c =
    a
        + b
        ^ c

restock :: Int -> Int -> Int -> Int
restock a b c =
    a
        + b
            ^ c
