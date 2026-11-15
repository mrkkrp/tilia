{-# LANGUAGE CPP #-}

module Bakery.Loaves where

loaves :: [String]
loaves =
  [ "rye"
#ifdef WITH_PASTRY
  , "brioche"
#ifdef WITH_BUTTER
  , "kouign-amann"
#endif
#endif
  ]

proofMinutes :: Int
proofMinutes = scaled
#ifdef WITH_PASTRY
#ifdef WITH_BUTTER
 120
#else
 90
#endif
#else
 60
#endif
