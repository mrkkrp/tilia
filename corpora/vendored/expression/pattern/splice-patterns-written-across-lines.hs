{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.Header where

import Wire.Header.Patterns (field, header)

version :: Header -> Maybe Int
version h = case h of
  $(field
      "version") -> Just 1
  [header|v2|] -> Just 2
  [header| v3
legacy
|] -> Just 3
  _ -> Nothing
