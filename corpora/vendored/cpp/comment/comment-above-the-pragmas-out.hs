-- The chimney sweep's timetable, built with or without the long brushes.

{-# LANGUAGE CPP #-}

module Chimney.Timetable (reach) where

#ifdef LONG_BRUSHES
reach :: Int
reach = 12
#else
reach :: Int
reach = 8
#endif
