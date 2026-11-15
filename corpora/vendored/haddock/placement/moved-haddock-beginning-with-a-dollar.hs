module Kiln.Price where

-- A dollar that a Haddock moved in front of what it documents begins with
-- stays as written.
charge ::
  Int -- ^ $1 for each unit
  -> Int {- ^ $5 at least,
  $10 at most -}
  -> Int
charge = const
