{-# LANGUAGE MultiWayIf #-}

module Kitchen.Order where

prepare :: Order -> IO Dish
prepare order =
    let station = stationFor order
     in do
            heat station
            cook station order

serve :: Table -> Order -> IO ()
serve table order =
    if
        | ready order -> do
            plate order
            deliver table order
        | late order -> case excuse order of
            Just note -> apologise table note
            Nothing -> wait
        | otherwise -> wait
