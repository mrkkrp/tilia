-- Guards that run past column 80 go on lines of their own and break there,
-- but a guard written beside the head of its equation before a do keeps
-- its line.
module Loom.Guards where

shade colour
  | isDyed colour && saturationOf colour > threshold && lightnessOf colour < 50 =
      Dark
shade colour = Light

weaveWith loom | hasWarp loom && hasWeft loom && tensionOf loom > minimumTension && isReady loom = do
  spin loom
  beat loom
