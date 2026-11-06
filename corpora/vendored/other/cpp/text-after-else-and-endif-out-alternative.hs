{-# LANGUAGE CPP #-}

module Terminal.Features
  ( supportsColour,
#ifndef NO_PASTE
    supportsPaste,
#endif /* NO_PASTE */
  )
where

#ifdef WINDOWS /* the console */
supportsColour :: Bool
supportsColour = False
#else /* a terminal */
supportsColour :: Bool
supportsColour = True
#endif /* WINDOWS */

#if MIN_VERSION_base(4,20,0)
#else   /* nothing to add */
#endif  // MIN_VERSION_base

#ifndef NO_PASTE
supportsPaste :: Bool
supportsPaste = True
#else /* NO_PASTE */
#endif
