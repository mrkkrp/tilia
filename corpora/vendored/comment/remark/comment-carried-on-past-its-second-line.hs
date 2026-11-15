module Kiln.Schedule where

rampRate :: Int -> Int
rampRate held =
  clamp
    ( between
        (floorOf held)
        (ceilingOf held) -- the kiln never reports
        -- a reading below this
        -- once the burners are lit
    )

soakTime :: Int -> Int
soakTime held =
  clamp
    ( between
        (floorOf held)
        (ceilingOf held)
        -- a remark of its own, begun here
        -- and carried on to a second line
    )
