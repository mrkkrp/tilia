module Garden.Soil where

data Soil
    = -- | Heavy, and slow to drain.
      Clay
    | -- | Light, and quick to dry out.
      Sand

data Bed = Bed
    { bedDepth :: Int
      -- ^ How deep it is dug.
    , bedSoil :: Soil
      -- ^ What it is dug into.
    }
