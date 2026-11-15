{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Geometry.Polar where

pattern Polar :: Double -> Double -> Point
pattern Polar r theta <- (toPolar -> (r, theta))
  where Polar r theta = Point (r * cos theta) (r * sin theta)

pattern Degrees :: Double -> Angle
pattern Degrees d <- (toDegrees -> d) where
  Degrees d
    | d < 0 = Degrees (d + 360)
    | otherwise = Radians (d * pi / 180)
