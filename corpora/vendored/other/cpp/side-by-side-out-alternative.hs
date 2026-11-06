{-# LANGUAGE CPP #-}

module Terminal.Feature where

#ifdef WITH_MOUSE
mouse :: Bool
mouse = True
#else
mouse :: Bool
mouse = False
#endif

always :: Bool
always = True

#ifdef WITH_BRACKETED_PASTE
paste :: Bool
paste = True
#endif

version :: Int
version = 3
