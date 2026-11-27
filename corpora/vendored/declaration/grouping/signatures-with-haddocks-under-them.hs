module Kiln.Shelf where

parens :: Doc -> Doc -- ^ Wrap a document in parentheses.
brackets :: Doc -> Doc -- ^ Wrap a document in brackets.
parens = wrap '(' ')'
brackets = wrap '[' ']'

class Shelved a where
  height :: a -> Word -- ^ How high it sits.

  depth :: a -> Word
