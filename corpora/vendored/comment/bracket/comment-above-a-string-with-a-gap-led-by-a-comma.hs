module Shipment.Label where

warnings :: Bool -> [String]
warnings cold =
  [ "Fragile"
  -- the long one wraps in the source
  , "Keep away from \
    \heat and moisture"
  -- only in winter
  , if cold then "Do not freeze" else ""
  , "This side up"
  ]

stamps :: [Int -> String]
stamps =
  [ show
  -- weights are in grams
  , \grams -> show grams ++ " g"
  ]
