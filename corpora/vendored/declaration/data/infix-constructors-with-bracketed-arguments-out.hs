module Tree.Branch where

data Tree a = Leaf a | (Tree a) :^: (Tree a)
