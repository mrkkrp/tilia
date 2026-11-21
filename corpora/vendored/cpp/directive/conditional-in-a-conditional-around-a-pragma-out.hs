{-# LANGUAGE CPP #-}
#if defined(MIN_VERSION_prettyprinter)
#if MIN_VERSION_prettyprinter(1,7,0)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif

-- | Helpers for command-line options.
module Options.Help where

helpText :: String
helpText = "--help"
