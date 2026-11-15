{-# LANGUAGE StrictData #-}

module Sensor.Reading where

data Reading = Reading
    { readingTime :: {-# NOUNPACK #-} !Double
    , readingLabel :: ~String
    }

data Sample = Sample {-# NOUNPACK #-} !Int ~[Double]
