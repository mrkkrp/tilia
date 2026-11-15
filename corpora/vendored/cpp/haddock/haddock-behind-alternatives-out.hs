{-# LANGUAGE CPP #-}

module Kiln.Firing where

-- A Haddock that differs between the branches of a conditional is kept in
-- each branch, before a constructor, a field and an argument.
data Firing
  = Bisque
  |
#ifdef GLAZE_ONLY
    -- | Fired once, glazed.
#else
    -- | Fired twice, the second time glazed.
#endif
    Glaze

data Kiln = Kiln
  { temperature :: Int,
#ifdef ELECTRIC
    -- | Watts drawn.
#else
    -- | Gas burnt in an hour.
#endif
    draw :: Int
  }

fire ::
  Kiln ->
#ifdef ELECTRIC
  -- | Minutes to cool.
#else
  -- | Hours to cool.
#endif
  Int ->
  Firing
fire _ _ = Glaze

-- The same for one written after what it documents.
data Cone
  =
#ifdef METRIC
    -- | Below 1100 degrees.
#else
    -- | Below 2000 degrees.
#endif
    Low
  | High
