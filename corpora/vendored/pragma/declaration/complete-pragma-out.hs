{-# LANGUAGE PatternSynonyms #-}

module Queue.View where

data Queue a = Queue [a] [a]

pattern Empty :: Queue a
pattern Empty <- Queue [] []

pattern Front :: a -> Queue a
pattern Front x <- Queue (x : _) _

{-# COMPLETE Empty, Front #-}
{-# COMPLETE Empty, Front :: Queue #-}
