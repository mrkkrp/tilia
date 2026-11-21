{-# LANGUAGE CPP #-}

module Archive.Entry
    ( Entry (..)
    , packFileEntry
    ,
#if __GLASGOW_HASKELL__ >= 908
      {-# DEPRECATED "Use the directory-ospath-streaming package instead" #-}
#endif
      getDirectoryContentsRecursive
    , toTarPath
    ,
#if __GLASGOW_HASKELL__ >= 908
      {-# DEPRECATED "Use toTarPath instead" #-}
#endif
      toPath
    )
where

import Archive.Types
