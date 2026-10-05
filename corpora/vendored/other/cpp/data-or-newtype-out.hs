{-# LANGUAGE CPP #-}

module Terminal.Cell where

import Data.IORef (IORef)

-- | A cell of the screen.
#ifdef PROFILING
data Cell = Cell
  { cellRef :: {-# UNPACK #-} !(IORef Char),
    cellCount :: {-# UNPACK #-} !Int
  }
#else
newtype Cell = Cell
  { cellRef :: IORef Char
  }
#endif

blank :: Char
blank = ' '
