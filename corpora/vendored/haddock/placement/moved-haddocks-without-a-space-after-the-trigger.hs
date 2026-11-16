module Garden.Soil where

data Soil
  = Clay
  -- ^Heavy, and slow to drain.
  | Sand
  -- ^Light, and quick to dry out.

data Bed = Bed
  { -- |How deep it is dug.
    bedDepth :: Int,
    -- |What it is dug into.
    bedSoil :: Soil
  }
