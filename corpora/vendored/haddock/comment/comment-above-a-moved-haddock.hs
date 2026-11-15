module Terminal.Size where

data Size = Size
  { rows :: Int
  -- Measured once at startup; the terminal is not resized while we run.
  , columns :: Int
  -- ^ How wide the terminal is.
  }
