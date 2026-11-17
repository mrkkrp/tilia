{-# LANGUAGE GADTs #-}

module Traffic where

data Signal where
    Wait, Hold
        :: Int -> Signal
    Stop, Go
        :: Int
        {- ^ The delay. -}
        -> Signal
