{-# LANGUAGE CPP #-}

module Shape.Render where

-- Arguments only some configurations have, each with a Haddock.
square
  :: Int
#if defined(WITH_SIDE)
  -- ^ The side.
  -> Int
  -- ^ The other side.
#endif
  -> Shape
square = undefined
