{-# LANGUAGE CPP #-}

module Shape.Render where

-- Arguments only some configurations have, each with a Haddock.
data Shape where
  Square ::
#if defined(WITH_SIDE)
    -- | The side.
#endif
    Int ->
#if defined(WITH_SIDE)
    -- | The other side.
    Int ->
#endif
    Shape
