module Tariff.Band where

band :: Int -> Char
band kwh | kwh < 100 = 'A'
         | kwh < 500 = 'B'
         | otherwise = 'C'

charge :: Int -> Int
charge kwh | kwh < 100 = kwh * 3
               + standing
           | otherwise = kwh * 2
               + standing

rebate :: Int -> Int
rebate kwh | kwh > 900 = 50
rebate kwh | kwh > 600 = 20
rebate _ = 0
