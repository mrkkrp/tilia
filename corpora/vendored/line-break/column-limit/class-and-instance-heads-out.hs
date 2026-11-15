-- The head of a class or an instance that runs past column 80 is broken
-- after its keyword, before what it constrains, and before its where.
module Loom.Heads where

class
  (Ord thread, Show thread, Hashable thread, Monoid (Spool thread)) =>
  Threadable thread
  where
  spin :: thread -> Spool thread

instance
  (Ord thread, Show thread, Hashable thread) => Threadable (Warp thread Cotton)
  where
  spin = spinWarp
