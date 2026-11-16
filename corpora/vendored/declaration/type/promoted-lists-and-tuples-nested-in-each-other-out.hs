{-# LANGUAGE DataKinds #-}

-- A promoted list or tuple whose first item is itself promoted keeps a
-- space before that item, or its opening would read as a character.
module Registry.Flags where

type Entry = '( '[Int, Bool], "flags")

type Entries = '[ '(1, "one"), '(2, "two")]
