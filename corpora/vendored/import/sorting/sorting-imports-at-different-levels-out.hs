{-# LANGUAGE ExplicitLevelImports #-}

module Weather.Splices where

import Weather.Syntax (rain)
import splice Weather.Syntax (snow, sun)
import quote Weather.Syntax (hail, wind)

forecast :: String
forecast = "fair"
