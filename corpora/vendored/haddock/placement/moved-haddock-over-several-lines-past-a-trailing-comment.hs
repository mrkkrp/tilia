module Garden.Pest where

data Pest
  = Slug Int -- counted per bed
  -- ^ Leaves eaten from the edge in.
  -- Most active after rain.
  | Aphid
  -- ^ Sap taken from the stems.
  -- Ladybirds keep them down.
  | Mildew
