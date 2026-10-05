module Terminal.Grid where

instance Applicative Grid where
  pure = Grid . repeat
  -- applied cell by cell
  Grid fs <*> Grid xs = Grid (zipWith ($) fs xs)

instance Monad Parser where
  return = pure
  {- the first version:
  p >>= f = Parser (\s -> undefined)
  -}
  p >>= f = Parser (\s -> runParser p s >>= \(a, s') -> runParser (f a) s')

  -- set apart as written
  (>>) = (*>)

class Cell a where
  blank :: a
  -- the one shown for a cell nothing was put in
  placeholder :: a
  -- | How wide it is.
  width :: a -> Int
