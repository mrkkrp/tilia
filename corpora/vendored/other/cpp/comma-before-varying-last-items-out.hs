{-# LANGUAGE CPP #-}

module Shapes.Derived where

-- The comma before items that only some configurations have, where nothing
-- follows them, goes on the line of the first of them.
data Shape = Circle | Square
  deriving
    ( Eq,
      Ord
#ifdef WITH_SHOW
      , Show
#endif
#ifdef WITH_DATA
      , Data,
      Typeable
#endif
    )

palette :: [Colour]
palette =
  [ red,
    green
#ifdef WIDE_GAMUT
    , teal
#endif
  ]
