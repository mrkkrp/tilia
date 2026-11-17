module Kiln.Firing
  ( {-# DEPRECATED ["Glazes are mixed by weight now.",
                    "Use weigh."] #-} measure,
    fire,
    {-# DEPRECATED "Use fire." #-} bake,
    {-# WARNING in "x-heat"
          "Runs past the cone's rating." #-} overfire,
  )
where

measure, fire, bake, overfire :: Int
measure = 0
fire = 1
bake = fire
overfire = 2
