{-# LANGUAGE CPP #-}

module Storage.Create (createFor) where

import Storage.Handle

createFor :: FilePath -> Mode -> IO Handle
createFor path mode =
  createWith
    path
    mode
#if MIN_VERSION_storage(0,3,0)
    closeOnExec
    inheritable
#elif MIN_VERSION_storage(0,2,0)
    closeOnExec
#endif
