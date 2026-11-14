module Terminal.Size where

data Size
  = MkSize
  -- static
  { rows :: Int
    -- ^ how tall the terminal is
  , columns :: Int
    -- ^ how wide it is
  }
