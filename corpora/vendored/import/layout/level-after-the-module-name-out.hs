{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Weather.Derive where

import Weather.Lift quote
import Weather.Syntax splice (deriveForecast)
import Weather.Syntax quote qualified as Quoted

forecast :: String
forecast = "fair"
