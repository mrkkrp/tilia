module Garden.Frost where

type Week = Int {- ^ from 1 up to 52 -}
-- Weeks of a leap year are counted the same.
type Frost = (Week, Int)
