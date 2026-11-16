{-# LANGUAGE TransformListComp #-}

-- A comprehension that runs past column 80 is broken before each
-- qualifier, and a then qualifier stays on one line where it fits.
module Census.Oldest where

import GHC.Exts (sortWith)

oldest people = [name | (name, age, town) <- people, town == "Lyon", then sortWith by negate age, then take 3]
