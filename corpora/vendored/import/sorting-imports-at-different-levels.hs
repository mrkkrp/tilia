{-# LANGUAGE ExplicitLevelImports #-}

module Weather.Splices where

import quote Weather.Syntax (wind)
import Weather.Syntax (rain)
import splice Weather.Syntax (sun)
import splice Weather.Syntax (snow)
import quote Weather.Syntax (hail)

forecast :: String
forecast = "fair"
