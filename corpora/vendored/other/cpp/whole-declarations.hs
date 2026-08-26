{-# LANGUAGE CPP #-}

module Terminal.Palette where

reset :: String
reset  =  "\ESC[0m"

#ifdef TRUECOLOUR
paint    ::  Int -> Int -> Int -> String
paint r g b  =  "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
#else
paint    ::  Int -> Int -> Int -> String
paint _ _ _  =  ""
#endif

clear :: String
clear  =  "\ESC[2J"
