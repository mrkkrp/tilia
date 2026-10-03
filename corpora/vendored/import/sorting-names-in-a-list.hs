{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Weather.Alert where

import Weather.Level
  ( (<+>),
    type (~>),
    raise,
    pattern Storm,
    Level ((:>), Warning, Calm),
    lower,
    (!),
    Alert,
    pattern Breeze,
    type (:*:)
  )

alert :: Level -> Alert
alert = raise
