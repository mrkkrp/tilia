{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Weather.Gusts where

import Weather.Level (Measured (measure), gust, pattern Squall, type Gust, data Calm, data Gale)

strongest :: Gust -> Gale
strongest = measure
