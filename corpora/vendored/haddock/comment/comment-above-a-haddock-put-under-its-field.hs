module Garden.Path where

data Path = Path
  { length :: Int,
    -- measured along the middle
    -- | How wide the path is.
    width :: Int
  }
