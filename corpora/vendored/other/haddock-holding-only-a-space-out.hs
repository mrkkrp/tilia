{- | -}
module Kiln.Glaze where

-- A Haddock that holds nothing but a space, as the one above the header
-- does, would lose it at the end of its line and hold nothing, so it comes
-- out as a block comment. The same goes before a constructor, a field and
-- an argument.
data Finish
  = {- | -}
    Matte
  | Gloss

data Kiln = Kiln
  { {- | -} temperature :: Int
  }

fire ::
  {- | -} Int ->
  Kiln
fire = Kiln

-- One with nothing after its trigger stays as it is.

-- |
glaze :: Finish
glaze = Matte
