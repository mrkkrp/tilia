{-# LANGUAGE CPP #-}

module Build.Flags where

#ifdef WITH_MOUSE
mouse :: Bool
mouse = True
#else
mouse :: Bool
mouse = False
#endif

#ifdef WITH_PASTE
paste :: Bool
paste = True
#else
paste :: Bool
paste = False
#endif

#ifdef WITH_COLOUR
colour :: Bool
colour = True
#else
colour :: Bool
colour = False
#endif

#ifdef WITH_TITLE
title :: Bool
title = True
#else
title :: Bool
title = False
#endif

#ifdef WITH_CURSOR
cursor :: Bool
cursor = True
#else
cursor :: Bool
cursor = False
#endif

#ifdef WITH_RESIZE
resize :: Bool
resize = True
#else
resize :: Bool
resize = False
#endif

#ifdef WITH_UNICODE
unicode :: Bool
unicode = True
#else
unicode :: Bool
unicode = False
#endif

everything :: [Bool]
everything = [mouse, paste, colour, title, cursor, resize, unicode]
