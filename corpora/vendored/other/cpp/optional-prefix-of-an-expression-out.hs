{-# LANGUAGE CPP #-}

module Properties.Json (jsonIso) where

jsonIso :: Proxy a -> Property
jsonIso _ =
#if MIN_VERSION_QuickCheck(2,9,0)
  again $
#endif
  MkProperty $
    arbitrary >>= \x ->
      counterexample "decoded" (Just x == decode (encode x))
