{-# LANGUAGE CPP #-}

module Terminal.Wrapped where

#define WRAP(a,b)   \
  ("<" ++ a         \
       ++ b ++ ">")

both :: String
both  =  WRAP("x","y")
