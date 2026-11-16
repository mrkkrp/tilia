{-# LANGUAGE GADTs #-}

module Circuit.Signal where

data Signal = (:>) Int Signal | Silent

data Sample = (:@){level :: Int, offset :: Double}

data Trace a where
    Idle :: Trace a
    (:|) :: a -> Trace a -> Trace a
    Mark, (:!) :: Trace a
