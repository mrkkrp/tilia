-- | Read the header of a texture file, whose levels follow it.
--
-- The version is checked first:
-- anything else is not read.
module Kiln.Header (header) where

-- | The size of the header in bytes.
header :: Int
header = 80
