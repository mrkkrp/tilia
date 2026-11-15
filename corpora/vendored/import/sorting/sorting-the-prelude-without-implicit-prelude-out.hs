{-# LANGUAGE NoImplicitPrelude #-}

module Weather.Forecast where

import Control.Monad (when)
import Data.List (foldl')
import Prelude (Double, (+))
import Weather.Reading (Reading)

warmer :: Double -> Double
warmer = (+ 1)
