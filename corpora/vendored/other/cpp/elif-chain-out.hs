{-# LANGUAGE CPP #-}

module Terminal.Newline where

newline :: String
#if defined(TARGET_WINDOWS)
newline = "\r\n"
#elif defined(TARGET_CLASSIC_MAC)
newline = "\r"
#else
newline = "\n"
#endif

indent :: Int
indent = 2
