{-# LANGUAGE TypeData #-}

module Engine.Mode where

type data Mode = Idle | Running | Stopped

type data Gear where
  Low :: Gear
  High :: Gear
