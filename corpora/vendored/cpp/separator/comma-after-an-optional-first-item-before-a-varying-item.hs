{-# LANGUAGE CPP #-}

-- An item only some configurations have, written first with the comma
-- after it, keeps the comma after it where the item after it varies.
module Garden.Barn (tools) where

tools :: [Tool]
tools =
  [
#ifdef WITH_SHEARS
    shears,
#endif
#ifdef WITH_RAKE
    rake,
#else
    hoe,
#endif
    spade
  ]
