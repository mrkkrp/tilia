{-# LANGUAGE CPP #-}

module Terminal.Macro where

#define ESCAPE(n) ("\ESC[" ++ show n ++ "m")

plain :: String
plain = ESCAPE (0)
