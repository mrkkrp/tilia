{-# LANGUAGE CPP #-}

-- Items only some configurations have, written first one after another with
-- the comma after each, keep the commas after them, wherever the other
-- items have theirs.
module Garden.Shed (Shelf (..), tools) where

data Shelf = Shelf
  {
#ifdef WITH_TOP
    top :: Board,
#endif
#ifdef WITH_MIDDLE
    middle :: Board,
#endif
#ifdef WITH_BOTTOM
    bottom :: Board,
#endif
    brackets :: Int
  }

tools :: [Tool]
tools =
  [
#ifdef WITH_RAKE
    rake,
#endif
#ifdef WITH_HOE
    hoe,
#endif
    spade,
    trowel
  ]
