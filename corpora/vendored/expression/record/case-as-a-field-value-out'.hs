module Kiln.Settings where

defaults :: Mode -> Settings
defaults mode =
    Settings
        { peak =
            -- Glazes need it hotter.
            case mode of
                Bisque -> 1000
                Glaze -> 1240
        , hold = case mode of
            Bisque -> 10
            Glaze -> 20
        }

cooler :: Mode -> Settings -> Settings
cooler mode s =
    s
        { peak =
            -- Leave a margin either way.
            case mode of
                Bisque -> peak s - 50
                Glaze -> peak s - 20
        }
