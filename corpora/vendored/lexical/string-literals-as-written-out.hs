-- A string comes out as written wherever it is, in a pattern, a pragma or
-- a rule's name: its gaps line up under its opening quote, and nothing is
-- escaped or quoted that was not.
module Text.Written where

isLong :: String -> Bool
isLong
  "a very long string \
  \that goes on" = True
isLong _ = False

{-# DEPRECATED
  old
  "use the new one, \
  \which is better"
  #-}
old :: Int
old = 1

{-# WARNING new ["one", "two"] #-}
new :: Int
new = 2

{-# RULES "f/∘" forall x. f (g x) = h x #-}

f :: Int -> Int
f = id
{-# SCC f "f_label" #-}

g :: Int -> Int
g = id
{-# SCC g #-}

h :: Int -> Int
h = id

x :: Int
x = {-# SCC foo #-} 1

y :: Int
y = {-# SCC "bar" #-} 2
