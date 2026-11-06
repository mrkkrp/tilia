-- Expressions of every kind that breaks, each written on one line, between
-- 90 and 120 columns wide.
module Loom.Expressions where

weave threads thread colour = summarise (findWithDefault colour thread threads) (count threads) (tension threads)

looms = [Loom "warp" 120 Cotton, Loom "weft" 80 Wool, Loom "tapestry" 200 Silk, Loom "rug" 90 Jute]

counts = (countWarp defaultLoom, countWeft defaultLoom, countSelvedge defaultLoom, shuttleCount)

defaultLoom = Loom {loomName = "workshop", loomWarp = [], loomWeft = [], loomTension = 3, loomWidth = 120}

restrung loom = loom {loomWeft = respool (loomWeft loom) (spoolLength defaultLoom) (twistOf defaultLoom)}

shuttle loom = if loomWidth loom > shuttleReach then throwFromBothSides loom else throwOnce loom

shade colour = case colour of Dyed dye -> saturationOf dye; Bleached -> 0; Plain -> defaultShade

tightened spun = map (adjustTension tension) spun ++ map (adjustTension (tension + 1)) (reverse spun) ++ leftovers

tension loom = let threads = loomWarp loom in sum (map threadTension threads) `div` length threads

rethreader = \loom -> rewind (reverse (loomWarp loom)) (loomTension loom + 3) (spoolsOf defaultLoom)

bobbins threads = do spun <- spinAll threads (twistOf defaultLoom); tighten spun; pure (length spun)

spools looms = [spool | loom <- looms, spool <- loomSpools loom, spoolLength spool > minimumLength]

beam loom = pure $ arrangeOnBeam (reverse (loomWarp loom)) (beamWidth defaultLoom) (beamDepth loom)
