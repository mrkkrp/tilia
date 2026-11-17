{-# LANGUAGE CPP #-}

-- Two runs of last fields, the second behind a question an import asks
-- too, and which the answer to the first settles where it fails.
module Bakery.Recipe (Recipe (..)) where

#if MIN_VERSION_bakery(2,3,0) && !GLUTEN_FREE
import Bakery.Yeast (Yeast)
#endif

data Recipe = Recipe
    { flour :: !Int
    , water :: !Int
#if MIN_VERSION_bakery(2,1,0)
    , salt :: !Int
    , sugar :: !Int
#endif
#if MIN_VERSION_bakery(2,3,0) && !GLUTEN_FREE
    , yeast :: !(Maybe Yeast)
    , rising :: !Bool
#endif
    }
