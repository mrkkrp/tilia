module Garden.Watering where

water :: Bed -> IO ()
water bed = do
    level <- moisture bed
    let litres = 30 - level
     in note ("used " <> show litres)
    let spare = 5 in note (show spare)
    done bed
