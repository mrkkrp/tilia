-- A record construction or update that runs past column 80 is broken, one
-- field per line, and a field that runs past it is broken after its =.
module Loom.Records where

defaultLoom =
  Loom
    { loomName = "workshop"
    , loomWarp = []
    , loomWeft = []
    , loomTension = 3
    , loomWidth = 120
    }

rethread loom =
  loom
    { loomWarp = reverse (loomWarp loom)
    , loomTension = loomTension loom + tension
    }

restrung loom =
  loom
    { loomWeft =
        respool (loomWeft loom) (spoolLength defaultLoom) (twistOf defaultLoom)
    }
