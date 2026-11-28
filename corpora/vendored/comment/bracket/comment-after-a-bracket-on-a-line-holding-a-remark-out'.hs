module Tariff.Total where

total =
    base
        + ( -- the surcharge
            -- at night only
            night
          )

routes =
    local
        ++ [ -- the express lines
             -- through the centre
             central
           ]

updated =
    fares
        <> tariff -- this year's
            { -- from January
              peak = 3
            }
