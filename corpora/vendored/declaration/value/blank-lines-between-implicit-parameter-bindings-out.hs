{-# LANGUAGE ImplicitParams #-}

module Render.Settings where

render page =
  let ?width = 80
      ?indent = 2

      ?colour = False
   in layout page

preview page = layout page
  where
    ?width = 40

    ?colour = True

session = do
  let ?width = 120

      ?colour = True
  run
