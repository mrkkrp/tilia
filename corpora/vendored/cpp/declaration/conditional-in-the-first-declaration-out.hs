{-# LANGUAGE CPP #-}

module Brewery.Mash where

strikeTemperature :: Int
strikeTemperature =
#if defined(METRIC)
  72
#else
  162
#endif

restMinutes :: Int
restMinutes = 60

spargeLitres :: Int
spargeLitres = 20
