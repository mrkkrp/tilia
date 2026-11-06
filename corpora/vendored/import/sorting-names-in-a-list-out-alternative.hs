{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module Weather.Alert where

import Weather.Level
  ( Alert,
    Level (Calm, Warning, (:>)),
    lower,
    raise,
    (!),
    (<+>),
    pattern Breeze,
    pattern Storm,
    type (:*:),
    type (~>),
  )

alert :: Level -> Alert
alert = raise
