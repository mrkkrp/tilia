{-# LANGUAGE TypeFamilies #-}

module Garden.Bed where

type family Harvest crop where
    Harvest Tomato =
        (Basket, Crate)
    Harvest Bean =
        [Pod]
    Harvest crop = crop

type family Soil bed season where
    Soil Raised Spring = Loam
    Soil
        bed
        season =
        Clay
