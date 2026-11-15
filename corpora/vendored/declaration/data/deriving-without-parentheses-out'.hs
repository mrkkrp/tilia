module Garden.Bed where

data Soil = Clay | Loam | Sand
    deriving (Show)

newtype Depth = Depth Int deriving (Eq)

data Bed = Bed
    { soil :: Soil
    , depth :: Depth
    }
    deriving (Show)
