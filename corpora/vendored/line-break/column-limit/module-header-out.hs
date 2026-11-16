-- A module header written on one line that runs past column 80 is broken
-- before its export list, which stays on one line where that fits.
module Loom.Settings.Internal
  (Settings (..), defaultSettings, warpTension, weftDensity)
where

import Loom.Settings.Types
