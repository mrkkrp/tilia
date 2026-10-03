{-# LANGUAGE NoImplicitPrelude #-}

module Weather.Forecast where

import Weather.Reading (Reading)
import Prelude (Double, (+))
import Data.List (foldl')
import Control.Monad (when)

warmer :: Double -> Double
warmer = (+ 1)
