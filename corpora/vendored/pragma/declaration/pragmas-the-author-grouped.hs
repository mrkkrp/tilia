-- Pragmas written together stay together, whatever their sort, and apart
-- from the declaration above them where they do not name it.
module Codec.Pixels where

brighten :: Int -> Int
brighten = (+ 1)

darken :: Int -> Int
darken = subtract 1
{-# INLINE brighten #-}
{-# NOINLINE darken #-}
{-# RULES "brighten/darken" forall x. brighten (darken x) = x #-}
{-# RULES "darken/brighten" forall x. darken (brighten x) = x #-}

{-# DEPRECATED brighten "Use lighten instead." #-}
{-# DEPRECATED darken "Use dim instead." #-}
