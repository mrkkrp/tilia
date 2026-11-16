module Http.Header where

import Data.Word (Word8)

type Name = String -- ^ Compared without regard to case.

type Value = [Word8]
  -- ^ The bytes as they were sent.

type Field = (Name, Value) {- ^ One line of a header. -}
