module Kiln.Load where

loaded =
    stack
        -- one shelf at a time
        $ shelf
            : rest

checked =
    when ready
        -- or not at all
        $ do
            fire
            cool

stacked =
    stack $ -- one shelf at a time
        shelf
            : rest
