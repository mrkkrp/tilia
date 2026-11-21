{-# LANGUAGE CPP #-}

-- The imports of a branch are spaced and sorted as those of the first,
-- even where the first has declarations before them.
module Loom.Shuttle (weave) where

import Data.List (sortOn)
#ifdef USE_TEXT
import Data.Char (toUpper)
-- Text, where it is available.
import Data.Text (Text)

weave :: Text -> Text
weave = id
#else
import Data.Char (toUpper)
-- Strings otherwise.
import Data.String (IsString)

-- For the instance only.

import Data.Bits (xor)
import Data.Coerce (coerce)

weave :: String -> String
weave = id
#endif
