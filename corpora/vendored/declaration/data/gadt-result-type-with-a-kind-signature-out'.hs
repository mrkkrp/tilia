{-# LANGUAGE GADTs #-}

module Stack.Frame where

import Data.Kind (Type)

data Frame where
    Push :: Int -> (Frame :: Type)
