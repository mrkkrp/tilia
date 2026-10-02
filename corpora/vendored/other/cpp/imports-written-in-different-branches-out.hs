{-# LANGUAGE CPP #-}

module Terminal.Errors (isUnsupported) where

import Control.Exception (IOException)
#ifndef __MHS__
import GHC.IO.Exception
#else
import System.IO.Error
#endif
  ( IOErrorType (..),
    ioe_type,
  )
import Data.Bool (bool)

isUnsupported :: IOError -> Bool
isUnsupported e = ioe_type e == UnsupportedOperation
