module Garden.Pest where

data Pest
  = -- | Leaves eaten from the edge in.
    -- Most active after rain.
    Slug Int -- counted per bed
  | -- | Sap taken from the stems.
    -- Ladybirds keep them down.
    Aphid
  | Mildew
