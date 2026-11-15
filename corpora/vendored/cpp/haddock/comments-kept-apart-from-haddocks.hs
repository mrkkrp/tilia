{-# LANGUAGE CPP #-}

module Garden.Bed where

-- | Turn the soil over before planting.

-- TODO: skip the beds that were dug last season
dig :: IO ()
dig = pure ()

data Bed = Bed
  { -- the old name was bedWidth
    -- | How wide the bed is.
    width :: Int,
    depth :: Int
  }

#if defined(RAISED_BEDS)
raised :: Bool
raised = True
#endif
