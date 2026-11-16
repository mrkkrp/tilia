module Garden.Path where

data Path = Path
    { length :: Int
      -- measured along the middle
    , width :: Int
      -- ^ How wide the path is.
    }
