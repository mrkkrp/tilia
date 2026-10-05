module Inventory
  ( -- * Items
    -- in the order they come in
    Item,
    newItem,
  )
where

count = 0

-- Plan:
--
-- * Count what comes in. Count what goes
--   out as well.
--
-- * Never count twice.

-- | One thing on a shelf.
data Item = Item

----------------------------------------------------------------------------
-- * Making items
----------------------------------------------------------------------------

newItem :: Item
newItem = Item

-- * Checking items
-- once there is something to check
check = True
