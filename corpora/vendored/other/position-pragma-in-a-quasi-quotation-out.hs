{-# LANGUAGE QuasiQuotes #-}

-- | A position pragma in a quasi-quotation is text, and moves nothing.
module Woven.Template where

template :: String
template = [str|
{-# LINE 40 "Template.hs" #-}
|]

after :: Int
after = 1
