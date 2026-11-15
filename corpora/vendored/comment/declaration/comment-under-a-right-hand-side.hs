module Alternatives where

main :: IO ()
main = do
  let value =
        one <|> two
        -- Alt one two
  print value

combined =
  one <|> two
  -- Alt one two
next = 1

local = value
  where
    value =
      one <|> two
      -- Alt one two
      -- and nothing else
    other = 1

apart = do
  let value =
        one <|> two
  -- about what follows
  print value

bound = do
  x <-
    one <|> two
    -- Alt one two
  print x

instance Semigroup Meta where
  Meta a <> Meta b = Meta (b <> a)
  -- the second one wins

instance Monoid Meta where
  mempty = Meta mempty
