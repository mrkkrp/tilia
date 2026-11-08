-- A pragma naming a constructor, a field or a method stays next to the type
-- it belongs to, as one naming the type itself does.
module Codec.Settings where

{-# DEPRECATED Lossless "Use Quality 100 instead." #-}
data Mode
  = Lossless
  | Quality{qualityLevel :: Int}
{-# DEPRECATED qualityLevel "Use level instead." #-}

class Encoder e where
  encode :: e -> String
  encodeAll :: [e] -> String
{-# DEPRECATED encodeAll "Use encode on each instead." #-}

data Format = Png | Jpeg
{-# DEPRECATED Jpeg "Use Png instead." #-}
