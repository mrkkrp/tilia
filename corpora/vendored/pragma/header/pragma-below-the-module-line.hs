module Kiln.Size where
{-# LANGUAGE OverloadedStrings #-}

-- GHC reads pragmas above the module line alone, so OverloadedStrings is
-- off, and the pragma stays where it is written.
size :: Int
size = length "abc"
