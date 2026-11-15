{-# LANGUAGE GADTs #-}

module Traffic.Signal where

data Light where
    Red, Amber, Green :: Light
    Flashing :: Light -> Light

data Phase a where
    Stop, Go :: Phase Bool
    Wait, Hold :: Int -> Phase Int
