{-# LANGUAGE CPP #-}

module Storage.Open (openFor) where

import Storage.Handle

openFor :: FilePath -> Mode -> IO Handle
openFor path mode  =
  openWith path  mode
#if MIN_VERSION_storage(0,2,0)
    closeOnExec
#endif
