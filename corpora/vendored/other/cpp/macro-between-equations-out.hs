{-# LANGUAGE CPP #-}

-- A use standing for an equation stays at the margin between the equations
-- around it.
module Table.Lookup where

#define ENTRY(k, v) lookupEntry k = v

lookupEntry :: Int -> String
lookupEntry 0 = "zero"
ENTRY(1, "one")
lookupEntry _ = "many"
