{-# LANGUAGE TemplateHaskell #-}

-- A lambda keeps the space before a splice pattern, without which the
-- backslash and the $ would read as one operator.
module Wire.Frame where

import Wire.Frame.Patterns (footer, header)

isHeader :: Frame -> Bool
isHeader $header = True
isHeader _ = False

payload :: Frame -> Maybe Bytes
payload frame = case frame of
  $(header) -> Nothing
  Data bytes -> Just bytes

dropFooters :: [Frame] -> [Frame]
dropFooters = filter (\ $(footer) -> False)
