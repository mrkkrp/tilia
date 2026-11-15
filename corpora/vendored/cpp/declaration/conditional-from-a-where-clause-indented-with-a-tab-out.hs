{-# LANGUAGE CPP #-}

-- A conditional that begins among bindings indented with a tab and goes on
-- into the declarations after it stays one conditional, as it does where
-- they are indented with spaces.
module T where

f :: Int -> Int
f x = y
  where
    y = x + 1
#if defined(EXTRA)
    z = 2

g :: Int
g = 1
#else

g :: Int
g = 2
#endif
