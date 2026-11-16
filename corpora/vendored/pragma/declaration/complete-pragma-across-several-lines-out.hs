{-# LANGUAGE PatternSynonyms #-}

module Signal.Level where

data Level = Level Int

pattern Low :: Level
pattern Low <- Level 0

pattern Mid :: Level
pattern Mid <- Level 1

pattern High :: Level
pattern High <- Level 2

{-# COMPLETE
  Low,
  Mid,
  High
  #-}

{-# COMPLETE
  Low,
  Mid,
  High
  #-}
