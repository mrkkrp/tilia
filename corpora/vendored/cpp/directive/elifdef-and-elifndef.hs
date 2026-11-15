{-# LANGUAGE CPP #-}

module Terminal.Bell where

bell :: String
#ifdef VISUAL_BELL
bell  =  "\ESC[?5h"
#elifndef QUIET
bell  =  "\a"
#else
bell  =  ""
#endif

flash :: Int
#ifdef VISUAL_BELL
flash  =  100
#elifdef SLOW_TERMINAL
flash  =  400
#endif
