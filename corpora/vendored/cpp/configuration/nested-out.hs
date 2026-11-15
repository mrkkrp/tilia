{-# LANGUAGE CPP #-}

module Terminal.Cursor where

#ifdef ANSI
home :: String
home = "\ESC[H"

#ifdef ANSI_PRIVATE
hide :: String
hide = "\ESC[?25l"
#endif
#else
home :: String
home = "\r"
#endif

bell :: String
bell = "\a"
