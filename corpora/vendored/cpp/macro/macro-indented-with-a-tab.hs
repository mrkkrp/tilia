{-# LANGUAGE CPP #-}

-- A use indented with a tab is at the column the tab moves it to: one at
-- the column of the code under it is a line of its own, and one further in
-- carries on the line above.
module Steps where

#define ARG(x) (x)
#define STEP(x) print (x)

g :: Int -> Maybe Int
g x = do
 y <- Just
	ARG(x)
 pure y

h :: Int -> IO ()
h x = do
	STEP(x ++ "";)
        pure ()
