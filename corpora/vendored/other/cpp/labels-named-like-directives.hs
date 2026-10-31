{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Labels named like directives, which cpp leaves alone where they do not
-- open their line.
module Record.Fields (columns, defined) where

#if MIN_VERSION_base(4,10,0)
import GHC.OverloadedLabels (IsLabel (..))
#endif

columns :: [Column]
columns =
  [ #line,
    #if,   #else
  ]

defined :: Column -> Bool
defined =
  hasValue
    #define
