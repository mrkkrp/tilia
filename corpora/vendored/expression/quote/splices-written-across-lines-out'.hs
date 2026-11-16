{-# LANGUAGE TemplateHaskell #-}

module Sensor.Calibrate where

import Sensor.Calibrate.Tables (offsetFor, scaleFor, threshold)

baseline :: Double
baseline = $$threshold

offset :: Double
offset = $$(offsetFor "probe-a")

scale :: Double
scale =
    $$( scaleFor
            "probe-b"
      )

limits :: (Double, Double)
limits =
    $( tupE
        [[|minBound|], [|maxBound|]]
     )

drift :: Double -> Double
drift x
    | x > baseline =
        $$( scaleFor
                "probe-c"
          )
    | otherwise = $$threshold
