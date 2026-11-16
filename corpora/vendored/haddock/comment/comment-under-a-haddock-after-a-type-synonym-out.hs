module Garden.Frost where

type Week =
  -- | from 1 up to 52
  Int

-- Weeks of a leap year are counted the same.
type Frost = (Week, Int)
