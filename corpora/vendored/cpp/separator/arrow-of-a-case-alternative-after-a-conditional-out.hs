{-# LANGUAGE CPP #-}

module Window.Event where

describe :: Event -> String
describe event = case event of
#if MIN_VERSION_base(4,18,0)
  Resized width _ ->
#else
  Resized width ->
#endif
    "resized to " <> show width
  Closed -> "closed"
