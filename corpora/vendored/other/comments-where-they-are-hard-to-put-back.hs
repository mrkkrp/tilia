module M where

-- | A heading

-- and a note under it
data T = T
  { a :: Int, -- the first
    -- the second
    b :: Int
  }

f x -- what to do with this?
  | x > 0 = g x
  | otherwise = h x
  where
    g = id

    -- about h
    h = negate

xs =
  [ one,
    {- inline -} two
    -- last
  ]
