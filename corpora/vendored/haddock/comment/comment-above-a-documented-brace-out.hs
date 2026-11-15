module Terminal.Size where

data Size
  = MkSize
  -- static
  { -- | how tall the terminal is
    rows :: Int,
    -- | how wide it is
    columns :: Int
  }
