module Circuit.Wire where

data Wire
  = Cable Int
  | -- | Two wires in series.
    Wire :-> Wire
  | -- | Two wires side by side.
    Wire :|| Wire

data Edge = Int :-: Int -- ^ A wire between two pins.

data Reading
  = Double -- ^ The voltage.
    :@ -- ^ A voltage read at a time.
    Int -- ^ The time, in milliseconds.
