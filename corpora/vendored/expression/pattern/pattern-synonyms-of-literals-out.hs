{-# LANGUAGE PatternSynonyms #-}

module Http.Method where

pattern Get :: String
pattern Get <- "GET"
  where
    Get = "GET"

pattern Head <- "HEAD" where Head = "HEAD"
