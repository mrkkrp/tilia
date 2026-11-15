{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Kiln.Rebound where

-- RebindableSyntax turns ImplicitPrelude off, so with it named last, the
-- Prelude is not imported, and this map is the only one in scope.
map :: ()
map = ()

glaze :: ()
glaze = map
