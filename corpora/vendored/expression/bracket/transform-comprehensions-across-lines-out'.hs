{-# LANGUAGE TransformListComp #-}

module Census.Districts where

import Data.List (permutations)
import GHC.Exts (groupWith, sortWith, the)

latest records = [r | r <- records, then reverse]

byGap xs ys = [(x, y) | x <- xs, y <- ys, then sortWith by (x - y)]

bySum ps = [the s | (s, _) <- ps, then group by (s + 1) using groupWith]

districts households =
    [ ( the district
      , sum size
      )
    | (district, size) <- households
    , then group by
        district
      using
        groupWith
    ]

routes stops =
    [ route
    | route <- stops
    , then group using
        permutations
    ]

heaviest parcels =
    [ parcel
    | parcel <- parcels
    , then
        sortWith
      by
        ( negate
            (weight parcel)
        )
    ]
