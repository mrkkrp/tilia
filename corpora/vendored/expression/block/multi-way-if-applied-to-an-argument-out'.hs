{-# LANGUAGE MultiWayIf #-}

module Shape.Resize where

resize :: Bool -> Double -> Double
resize grow size = (if | grow -> (* 2) | otherwise -> (/ 2)) size

shrink :: Bool -> Double -> Double
shrink tight size =
    if
            | tight -> (/ 4)
            | otherwise -> (/ 2)
        size
