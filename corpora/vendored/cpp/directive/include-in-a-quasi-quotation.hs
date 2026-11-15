{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Terminal.Template where

import Terminal.Quoter (template)

banner :: String
banner =
  [template|
#include "banner.txt"
|]
