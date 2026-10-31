#!/usr/bin/env runghc
{-# LANGUAGE QuasiQuotes #-}

-- | Lines that GHC reads past elsewhere are text in a quasi-quotation, and a
-- script's first line is its own.
module Stitched.Script where

script :: String
script = [str|
#!/bin/sh
# 40 "Template.hs"
#pragma once
|]

after   ::   Int
after = 1
