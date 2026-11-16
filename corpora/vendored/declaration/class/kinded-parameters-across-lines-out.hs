module Shape.Measure where

class Sized (s :: *)

class
  Measured
    ( m ::
        * -> *
    )

class
  Fitted
    (outer :: *)
    ( inner ::
        *
    )
