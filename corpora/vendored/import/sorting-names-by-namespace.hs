{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Weather.Gusts where

import Weather.Level (data Gale, type Gust, pattern Squall, Measured (measure), data Calm, gust, data Gale)

strongest :: Gust -> Gale
strongest = measure
