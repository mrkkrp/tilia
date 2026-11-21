{-# LANGUAGE CPP #-}

-- The imports of a branch are spaced and sorted as those of the first,
-- even where the first has declarations before them.
module Loom.Shuttle (weave) where

import Data.List (sortOn)
#ifdef USE_TEXT
-- Text, where it is available.

import Data.Text (Text)
import Data.Char (toUpper)

weave :: Text -> Text
weave = id
#else
-- Strings otherwise.

import Data.String (IsString)
import Data.Char (toUpper)

-- For the instance only.

import Data.Coerce (coerce)
import Data.Bits (xor)

weave :: String -> String
weave = id
#endif
