{-# LANGUAGE CPP #-}

module M where

class Shape a where
  {- |
The area, for example:

#if MIN_VERSION_base(4,10,0)
>>> area (Square 2)
4.0
#else
>>> area (Square 2.0)
4.0
#endif
  -}
  area :: a -> Double

  -- | The perimeter.
  perimeter :: a -> Double
