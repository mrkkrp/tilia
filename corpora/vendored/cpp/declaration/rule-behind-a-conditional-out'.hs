{-# LANGUAGE CPP #-}

module Data.Stream.Enum where

{-# RULES
"enumFromTo<Int> [Stream]"
    enumFromTo = enumFromToInt :: Monad m => Int -> Int -> Stream m Int
#if WORD_SIZE_IN_BITS > 32
"enumFromTo<Int64> [Stream]"
    enumFromTo =
        enumFromToIntlike :: Monad m => Int64 -> Int64 -> Stream m Int64
#else
"enumFromTo<Int32> [Stream]"
    enumFromTo =
        enumFromToIntlike :: Monad m => Int32 -> Int32 -> Stream m Int32
#endif
    #-}
