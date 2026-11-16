module Garden.Soil where

data Soil
  = -- | Heavy, and slow to drain.
    Clay
  | -- | Light, and quick to dry out.
    Sand

data Bed = Bed
  { -- | How deep it is dug.
    bedDepth :: Int,
    -- | What it is dug into.
    bedSoil :: Soil
  }
