-- Comments in a do block that open as Haddocks would document nothing, so
-- their triggers are escaped.
module Kiln.Firing (fire) where

fire :: IO ()
fire = do
  -- | heat slowly at first, or the glaze cracks
  rampUp 100
  -- * hold at the peak
  -- ^ for about ten minutes
  hold 10
  {- | let it cool on its own:
  * no opening the lid
  -}
  coolDown
  -- $ventilation
  pure ()
