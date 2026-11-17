{-# LANGUAGE GADTs #-}

module Traffic where

data Phase a where
  Wait,
    Hold :: Int -> Phase Int
  Stop,
    Go ::
      Phase Bool
