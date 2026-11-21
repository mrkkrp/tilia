{-# LANGUAGE CPP #-}

module Setup.Programs where

programNames :: [Maybe String] -> [String]
programNames programs =
    [
#ifdef mingw32_HOST_OS
    name
        <> ".exe"
    | Just name <-
        programs
#else
    name
    | Just name <-
        programs
#endif
    ]
