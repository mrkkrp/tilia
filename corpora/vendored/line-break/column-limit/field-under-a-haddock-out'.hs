-- A field under a Haddock that runs past column 80 is broken as it would be
-- without the Haddock.
module Shelf.Size where

data Shelf = Shelf
    { height
        :: Map
            ShelfIdentifier
            (Either MeasurementFailure (NonEmpty MeasurementRecord))
      -- ^ The height.
    , width :: Int
    }
