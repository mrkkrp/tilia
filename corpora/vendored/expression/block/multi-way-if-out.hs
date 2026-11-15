{-# LANGUAGE MultiWayIf #-}

module Weather.Advice where

advice :: Int -> String
advice t =
  if | t > 30 -> "stay inside" | t < 0 -> "wrap up" | otherwise -> "go out"

wind :: Int -> String
wind speed =
  if
    | speed > 60 -> "gale"
    | speed > 20 ->
        "breeze"
    | otherwise -> "calm"
