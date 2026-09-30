{-# LANGUAGE CPP #-}

module Build.Make (make) where

-- | The make that understands our makefiles.
make :: String
#if defined(freebsd_HOST_OS) || defined(dragonfly_HOST_OS) \
    || defined(openbsd_HOST_OS) || defined(netbsd_HOST_OS)
make = "gmake"
#else
make = "make"
#endif
