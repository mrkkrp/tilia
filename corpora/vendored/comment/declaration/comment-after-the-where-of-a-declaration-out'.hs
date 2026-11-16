{-# LANGUAGE GADTs #-}

module Garden.Crop where

data Crop a where -- one constructor per kind of bed
    Root :: Int -> Crop Int
    Leaf :: String -> Crop String

class Plantable a where -- seeds and cuttings alike
    depth :: a -> Int

instance Plantable Int where -- in millimetres
    depth = id
