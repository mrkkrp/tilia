{-# LANGUAGE CPP #-}

module Sensor.Read where

infixr 5 +++

(+++) :: [Int] -> [Int] -> [Int]
(+++) = (++)

calibrate :: Bool -> Int
calibrate warm =
#if defined(THERMISTOR)
  if warm then 1 else 2
#else
#if defined(THERMOCOUPLE)
  case warm of _ -> 3
#else
#error "no sensor to calibrate"
#endif
#endif

readings :: [Int]
readings = [1] +++ [2] <> [3]
  <> [4] +++ [5]
