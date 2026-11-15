{-# LANGUAGE CPP #-}

module Terminal.Format (formatOf) where

import System.FilePath (takeExtension)
import Terminal.Format.Types

formatOf :: FilePath -> Format
formatOf path = go (takeExtension path)
  where
    go ".txt"   = Plain
    go ".ansi"  = Escaped
#if MIN_VERSION_zlib(0,6,0)
    go ".z"     = Deflated
#endif
    -- Only a new enough encoder knows the compressed form.
#if MIN_VERSION_zlib(0,7,0)
    go ".gz"    = Compressed
#endif
    go _        = Binary  -- anything else is passed through
