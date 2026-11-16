module Scale.Weigh where

class Scale s where
  weigh ::
    s ->
    -- | Tare and gross, in grams.
    ( Int,
      Int
    ) ->
    -- | What the goods weigh.
    Int
