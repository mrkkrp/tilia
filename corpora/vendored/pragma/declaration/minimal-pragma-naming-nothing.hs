module Garden.Tool where

class Tool a where
  sharpen :: a -> a
  sharpen = id
  {-# MINIMAL #-}
