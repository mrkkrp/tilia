{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Stock.Count where

type family ((a :: Count) `Within` (b :: Count)) (c :: Count) :: Bool where
    ( a
        `Within` b
        )
        c = a `AtMost` c
