{-# LANGUAGE CPP #-}

module Options where

data Options = Options
  { optionsLevel ::
      Maybe
#if MIN_VERSION_base(4,18,0)
        Natural,
#else
        Int,
#endif
    optionsName :: Text
  }
