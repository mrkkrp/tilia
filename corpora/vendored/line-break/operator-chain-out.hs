-- An operator chain keeps its line however long, since whether an operator
-- trails or hangs depends on where its operands were written.
module Loom.Chains where

tightened spun =
  map (adjustTension tension) spun ++ map (adjustTension (tension + 1)) (reverse spun)

beam loom =
  pure $ arrangeOnBeam (reverse (loomWarp loom)) (beamWidth defaultLoom) (beamDepth loom)
