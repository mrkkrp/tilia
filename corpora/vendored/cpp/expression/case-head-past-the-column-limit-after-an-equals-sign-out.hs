{-# LANGUAGE CPP #-}

module Parse.Step (step) where

step :: Int -> Char -> Int
step acc c
#if MIN_VERSION_base(4,16,0)
  | isDigitCharacter c,
    notTooLarge acc = case multiplyTheAccumulatorByTenSafely
      acc
      (digitValueOf c) of
      Just next -> next
#else
  | isDigit c,
    notTooLarge acc = case multiplyByTen acc (digitToInt c) of
      Just next -> next
#endif
      Nothing -> acc
  | otherwise = acc
