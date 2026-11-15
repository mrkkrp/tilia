{-# LANGUAGE CPP #-}

module Process.Signal where

number :: String -> Int
number name
  | name == "HUP" = 1
#if !defined(mingw32_HOST_OS)
  | name == "USR1" = 10
  | name == "USR2" = 12
#endif
  | otherwise = 15
