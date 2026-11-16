{-# LANGUAGE CPP #-}

module Garden.Bed where

-- | Turn the soil over before planting.

-- TODO: skip the beds that were dug last season
dig :: IO ()
dig = pure ()

data Bed = Bed
    { -- the old name was bedWidth
      width :: Int
      -- ^ How wide the bed is.
    , depth :: Int
    }

#if defined(RAISED_BEDS)
raised :: Bool
raised = True
#endif
