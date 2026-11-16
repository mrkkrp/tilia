{-# LANGUAGE GADTs #-}

-- GHC rejects a forall or a context inside a constructor's type, but its
-- parser takes them, and the brackets around one making up the whole type
-- keep it from being read as the constructor's own.
module Query.Shape where

data Query a where
    Fetch :: (forall b. Query b)
    Guarded :: ((Show a) => Query a)
    Late :: Int -> forall b. b -> Query b
    Checked :: Int -> (Show a) => Query a
