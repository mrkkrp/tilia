-- An operator chain that runs past column 80 is broken before its
-- operators, or after a lone trailing $, one operand per line.
module Loom.Chains where

tightened spun =
  map (adjustTension tension) spun ++ map (adjustTension (tension + 1)) (reverse spun)

beam loom = pure $ arrangeOnBeam (reverse (loomWarp loom)) (beamWidth defaultLoom) (beamDepth loom)

wind loom = liftIO $ spoolAll (loomWarp loom) $ tighten (tensionOf loom) $ threadsOf defaultLoom
