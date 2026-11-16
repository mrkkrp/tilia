{-# LANGUAGE CPP #-}

module Text.Pretty.Doc where

instance Show Doc where
  show = render
    where
      render = layout 80
#ifdef DEBUG
      layout = debugLayout
#else
      layout = fastLayout
#endif

      debugLayout _ = "<doc>"
      fastLayout _ = ""
