{-# LANGUAGE EmptyCase #-}

module Kiln.Firing where

fire kiln = do
  { heat kiln
  -- until the cones bend
  } -- one firing

glaze kiln = do
  { dip kiln
  ; dry kiln
  -- before it goes in
  } -- twice over

never cone = case cone of
  {
  -- no cone survives
  } -- not one
