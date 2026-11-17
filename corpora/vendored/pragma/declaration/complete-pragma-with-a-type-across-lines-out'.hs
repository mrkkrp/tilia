{-# LANGUAGE PatternSynonyms #-}

module Sensor.Reading where

data Reading = Reading Int

pattern Low, Normal, High :: Reading
pattern Low <- Reading 0
pattern Normal <- Reading 1
pattern High <- Reading 2

{-# COMPLETE
    Low
    , Normal
    , High :: Reading
    #-}

{-# COMPLETE
    Low
    , Normal
    , High
        :: Reading
    #-}

{-# COMPLETE Low, Normal, High :: Reading #-}
