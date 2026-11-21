-- | Helpers for command-line options.
{-# LANGUAGE CPP #-}
#if defined(MIN_VERSION_prettyprinter)
#if MIN_VERSION_prettyprinter(1,7,0)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif
module Options.Help where

helpText :: String
helpText = "--help"
