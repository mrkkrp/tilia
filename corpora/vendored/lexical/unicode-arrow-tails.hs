{-# LANGUAGE Arrows #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Where UnicodeSyntax is in force, the arrow tails that point right come
-- out in their Unicode forms as well, whichever form they were written in.
module Circuit.Probe where

probe sensor = proc reading -> do
  level <- reading >- sensor
  scaled <- level ⤚ calibrate
  shown <- (scaled, level) >>- display level
  logged <- shown ⤜ record scaled
  returnA -< logged
