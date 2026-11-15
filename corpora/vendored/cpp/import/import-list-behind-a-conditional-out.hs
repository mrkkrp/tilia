{-# LANGUAGE CPP #-}

module Kiln.Clock where

-- An import list that a conditional varies whole stays in each branch, on a
-- line of its own.
import Data.Word
#if MIN_VERSION_text(2,0,0)
  (Word8)
#else
  (Word16, Word32)
#endif
