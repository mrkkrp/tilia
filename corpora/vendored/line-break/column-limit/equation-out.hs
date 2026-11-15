-- An equation that runs past column 80 is broken after its =, -> or <-.
module Loom.Equations where

weave threads thread colour =
  summarise (findWithDefault colour thread threads) (count threads)

shade colour = case colour of
  Dyed dye ->
    blendWithBackground (saturationOf dye) (lightnessOf dye) (backgroundOf loom)
  Plain -> colour

bobbins threads = do
  spun <-
    spinAll (filter (not . frayed) threads) (length threads) (twistOf loom)
  pure spun
