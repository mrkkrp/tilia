module Loom.Settings where

data
    Settings
        warp
        weft
        heddle
        shuttle
        reed
        beam
        treadle
        bobbin
        selvedge
        harness
        pick = Settings
    { settingsWarpTension :: warp
    , settingsWeftDensity :: weft
    , settingsReed :: reed
    }
