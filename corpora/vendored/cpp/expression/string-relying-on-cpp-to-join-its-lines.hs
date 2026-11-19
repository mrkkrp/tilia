{-# LANGUAGE CPP #-}

module DirectoryTree (message, notice, separator) where

#ifdef mingw32_HOST_OS
separator :: Char
separator = '\\'
#else
separator :: Char
separator = '/'
#endif

message :: String
message = id
    "Error: not a valid directory tree expression\n\\
  \Only a subset of expressions can be converted.\n"

notice :: String
notice =
  "Error: not a valid directory tree expression\n\\
  \\n\\
  \Only a subset of expressions can be converted.\n"
