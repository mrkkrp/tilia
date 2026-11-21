{-# LANGUAGE CPP #-}

-- cpp joins the line under a line ending in a backslash to it, so the
-- empty line under each comment that ends in one stays.
module Database.Escape where

escape :: Int -> Int
escape c
  | c == 39 = 1 -- '

  | c == 92 = 2 -- \

  | otherwise = 3

escapes :: [Int]
escapes = [ 39 -- '
          , 92 -- \

          , 34 -- "
          ]

-- | A backslash:
-- \\

backslash :: Char
backslash = '\\'

data Separator = Separator
  { slash :: Char -- ^ not \

  , other :: Char
  }

#ifdef WINDOWS
separator = '\\' -- \

#else
separator = '/' -- /
#endif
