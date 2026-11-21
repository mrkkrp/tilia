{-# LANGUAGE CPP #-}

-- A comment under code that a conditional holds or leads on to stays at
-- the column it was written at, as without the conditional: a directive
-- stays at the margin whatever the code's indentation, so nothing lines up
-- with it.
module Glaze.Recipe where

flux :: [Int]
flux =
#ifdef HIGH_FIRE
  [20]
#else
  [25]
#endif
  ++ [3]
  -- Feldspar and whiting.
alumina :: Int
alumina =
#ifdef HIGH_FIRE
  15
#else
  12
#endif
  -- Kaolin.
opacity :: [Int]
opacity =
  [ 8
#ifdef ZIRCON
  , 2
#endif
  ]
-- Tin oxide.

silica :: [Int]
silica =
#ifdef HIGH_FIRE
  [60]
#else
  [55]
#endif
  ++ [5]
-- Percentages by weight.
