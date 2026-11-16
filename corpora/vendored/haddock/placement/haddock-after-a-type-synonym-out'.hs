module Http.Header where

import Data.Word (Word8)

type Name =
    -- | Compared without regard to case.
    String

type Value =
    -- | The bytes as they were sent.
    [Word8]

type Field =
    -- | One line of a header.
    (Name, Value)
