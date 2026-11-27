module Kiln.Shelf where

parens ::
  Doc ->
  -- | Wrap a document in parentheses.
  Doc
brackets ::
  Doc ->
  -- | Wrap a document in brackets.
  Doc
parens = wrap '(' ')'
brackets = wrap '[' ']'

class Shelved a where
  height ::
    a ->
    -- | How high it sits.
    Word

  depth :: a -> Word
