{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ == 904
{-# LANGUAGE MagicHash #-}
#endif

-- Where the version is exactly the one asked about above, both conditionals
-- in the list are settled: the first holds and the second does not.
module Weather.Checks (checks) where

#if __GLASGOW_HASKELL__ >= 904
hailCheck :: Check
hailCheck = case radar of
  Doppler -> Check 3
  Pulse n -> Check n
  _ -> Check 0
#endif

checks :: [(String, Check)]
checks =
  [ ("wind", windCheck),
    ("rain", rainCheck)
#if __GLASGOW_HASKELL__ >= 904
    ,
    ("hail", hailCheck),
    ("snow", snowCheck)
#endif
#if __GLASGOW_HASKELL__ >= 906
    ,
    ("fog", fogCheck)
#endif
  ]
