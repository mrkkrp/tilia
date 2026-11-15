{-# LANGUAGE CPP #-}

module Shapes.Instances where

instance Shape Circle
instance Shape Square

#if defined(TRIANGLES)
instance Shape Triangle
#endif

instance Shape Hexagon
instance Shape Octagon
