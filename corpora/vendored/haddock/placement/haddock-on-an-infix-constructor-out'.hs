module Circuit.Wire where

data Wire
    = Cable Int
    | -- | Two wires in series.
      Wire :-> Wire
    | -- | Two wires side by side.
      Wire :|| Wire

data Edge
    = -- | A wire between two pins.
      Int :-: Int

data Reading
    = -- | The voltage.
      Double
        -- | A voltage read at a time.
        :@
        -- | The time, in milliseconds.
        Int
