{-# LANGUAGE CPP #-}

module Report.Render where

render ::
  Settings ->
#if MIN_VERSION_text(2,0,0)
  Text ->
#else
  String ->
#endif
  Builder
render settings = build settings . fromText
