-- |
--
module T (
  -- | 
  f,
  C (..),
  D (..),
) where

-- | 
f :: Int {- ^ -} -> Int
f = id

class C a where
  -- | 
  m :: a

data D = D
  { -- | 
    d :: Int
  } -- ^ 
