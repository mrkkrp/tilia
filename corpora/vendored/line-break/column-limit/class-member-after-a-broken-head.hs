-- A class whose head runs past column 80 puts the members written after its
-- where on lines of their own, as an instance does.
module Loom.Members where

class (Ord thread, Show thread, Hashable thread, Monoid (Spool thread)) => Spinnable thread where spin :: thread -> Spool thread

class (Ord thread, Show thread, Hashable thread) => Wound thread spool | thread -> spool where wind :: thread -> spool; unwind :: spool -> thread

instance (Ord thread, Show thread, Hashable thread, Monoid (Spool thread)) => Spinnable (Warp thread) where spin = spinWarp
