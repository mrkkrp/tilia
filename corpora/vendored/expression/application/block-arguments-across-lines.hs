{-# LANGUAGE BlockArguments #-}

module Garden.Watering where

water :: IO ()
water = whenM
  do soilIsDry
  do openValve
     waitMinutes 5
     closeValve

harvest :: IO ()
harvest = bracket
  do openShed
  do lockShed
  \shed -> pick shed
