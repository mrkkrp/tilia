module Terminal.Size where

data Size = Size
  { rows :: Int,
    -- Measured once at startup; the terminal is not resized while we run.

    -- | How wide the terminal is.
    columns :: Int
  }
