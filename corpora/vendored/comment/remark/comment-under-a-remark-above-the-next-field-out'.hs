-- A comment under a remark but left of it is not the remark carried on:
-- it goes with the field under it.
module Shipment.Parcel where

data Parcel = Parcel
    { weight :: Double -- in kilograms
      -- the declared value, for customs
    , value :: Int
    }
