module Kiln.Choice where

chosen alternatives =
    settle
        $ fromMaybe
            ( -- the last one, when nothing fits
              last alternatives
            )
        $ firstFitting alternatives
