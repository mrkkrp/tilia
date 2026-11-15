{-# LANGUAGE DataKinds #-}

module Matrix.Size where

import GHC.TypeLits (Nat)

rows :: Proxy (n :: Nat) -> Int
rows _ = 0

type Square = Matrix (3 :: Nat) (3::Nat)
