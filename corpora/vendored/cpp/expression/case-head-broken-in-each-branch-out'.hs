{-# LANGUAGE CPP #-}

-- A case head each branch breaks past column 80 is broken the same way on
-- every pass, and what both branches go on with stays in each.
module Parse.Step (step) where

step :: Int -> Char -> Int
step accumulator character
#if MIN_VERSION_base(4,16,0)
    | isDigitCharacter character
    , notTooLarge accumulator = case multiplyTheAccumulatorByTenSafely
        accumulator
        (digitValueOf character) of
        Just next -> next
#else
    | isDigit character
    , notTooLarge accumulator = case multiplyByTen
        accumulator
        (digitToInt character) of
        Just next -> next
#endif
        Nothing -> accumulator
    | otherwise = accumulator
