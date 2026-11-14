{-# LANGUAGE CPP #-}

module Parse.Decimal (decimal) where

decimal :: Int -> String -> Int
decimal acc input = case input of
  [] -> acc
  w : rest -> case w of
#if MIN_VERSION_base(4,16,0)
    c
      | isDigitCharacter c,
        notTooLarge acc -> case multiplyTheAccumulatorByTenSafely
          acc
          (digitValueOf c) of
#else
    c
      | isDigit c,
        notTooLarge acc -> case multiplyByTen acc (digitToInt c) of
#endif
#if MIN_VERSION_base(4,16,0)
          Just next -> decimal next rest
#else
          Just next -> decimal next rest
#endif
          Nothing -> acc
