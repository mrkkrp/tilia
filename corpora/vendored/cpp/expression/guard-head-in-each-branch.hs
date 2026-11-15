{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module FlatParse.Common.Numbers (anyDigit, report) where

anyDigit :: Word# -> Addr# -> Result
anyDigit acc s = case indexWord8OffAddr# s 0# of
#if MIN_VERSION_base(4,16,0)
    w | 1# <- leWord8# (wordToWord8# 0x30##) w
      , 1# <- leWord8# w (wordToWord8# 0x39##)
      -> case timesWord2# acc 10## of
#else
    w | 1# <- leWord# 0x30## w
      , 1# <- leWord# w 0x39##
      -> case timesWord2# acc 10## of
#endif
          (# 0##, r #) -> OK r
          _ -> Overflow
    _ -> Fail

report :: Handle -> Status -> IO ()
report h status
#ifdef mingw32_HOST_OS
  | isFailure status
  = do
      hPutStrLn h "failed"
#else
  | isFailure status, colour
  = do
      hPutStrLn h "\ESC[31mfailed\ESC[0m"
#endif
      hFlush h
  | otherwise = pure ()
