{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Shelf.Labels where

-- A comment after a bracket that a tick or an @ is glued to stays after
-- the bracket rather than going after what the bracket holds first.
type Labels =
  '[ -- the first
     Int,
     -- the second
     Bool
   ]

type Pair =
  '( -- the left one
     Int,
     Bool
   )

blank :: [Label]
blank =
  mempty
    @( -- a list of them
       [Label]
     )
