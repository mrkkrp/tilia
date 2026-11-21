{-# LANGUAGE CPP #-}

module Setup.Programs where

programNames :: [Maybe String] -> [String]
programNames programs =
  [ name
#ifdef mingw32_HOST_OS
      <> ".exe"
  | Just name
#else
  | Just name
#endif
  <- programs ]
