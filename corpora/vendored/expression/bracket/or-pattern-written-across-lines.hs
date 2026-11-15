{-# LANGUAGE OrPatterns #-}

module Shape.Corners where

hasCorners :: Shape -> Bool
hasCorners s = case s of
  (Square;
   Triangle) -> True
  _ -> False

startsWithCorners :: [Shape] -> Bool
startsWithCorners ((Square;
                    Triangle) : _) = True
startsWithCorners _ = False

isFlat :: Shape -> Bool
isFlat s = case s of
  Line
  Point -> True
  _ -> False

isRound :: Shape -> Bool
isRound (Circle; Ellipse) = True
isRound _ = False
