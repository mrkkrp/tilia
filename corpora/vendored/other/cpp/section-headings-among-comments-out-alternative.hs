{-# LANGUAGE CPP #-}

module Inventory where

#ifdef STRICT
count = 0
#else
count = 1
#endif

-- Plan:
--
-- * Count what comes in. Count what goes
--   out as well.
--
-- * Never count twice.

----------------------------------------------------------------------------
-- * Making items
----------------------------------------------------------------------------

-- | One thing on a shelf.
data Item = Item
