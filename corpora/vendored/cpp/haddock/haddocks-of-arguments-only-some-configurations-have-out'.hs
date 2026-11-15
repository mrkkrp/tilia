{-# LANGUAGE CPP #-}

module Shape.Render where

-- Arguments only some configurations have, each with a Haddock.
data Shape where
    Square
        :: Int
#if defined(WITH_SIDE)
        -- ^ The side.
        -> Int
        -- ^ The other side.
#endif
        -> Shape
