{-# LANGUAGE CPP #-}

module Geometry.Line (Line (..), corners) where

data Line = Line
  { lineFrom :: Point
#ifdef WITH_STYLE
  , lineStyle :: Style
#endif
  , lineTo :: Point
  }

corners :: [Point]
corners =
  [ topLeft
#if WIDE
  , wideTopRight
#else
  , topRight
#endif
  , bottomRight
  ]
