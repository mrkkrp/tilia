{-# LANGUAGE CPP #-}

-- Code that one branch puts a prefix in front of, written at the column of
-- the prefix, comes out once after the conditional.
module Properties.Json (jsonIso, label, retried) where

jsonIso :: Proxy a -> Property
jsonIso _ =
#if MIN_VERSION_QuickCheck(2,9,0)
  again $
#endif
  MkProperty $
    arbitrary >>= \x ->
      counterexample "decoded" (Just x == decode (encode x))

label :: String -> Property -> Property
label s =
#ifndef LAZY_LABELS
  s `seq`
#endif
  mapTotalResult $
    \res -> res{labels = s : labels res}

-- Written in each branch, it stays in each.
retried :: Property -> Property
retried p =
#if MIN_VERSION_QuickCheck(2,9,0)
  again $
    once p
#else
  once p
#endif
