module Scale.Weigh where

class Scale s where
    weigh
        :: s
        -> ( Int
           , Int
           )
        -- ^ Tare and gross, in grams.
        -> Int
        -- ^ What the goods weigh.
