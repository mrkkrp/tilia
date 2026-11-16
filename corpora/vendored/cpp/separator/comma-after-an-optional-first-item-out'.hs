{-# LANGUAGE CPP #-}

-- An item only some configurations have, written first with the comma
-- after it, keeps the comma after it, wherever the other items have theirs.
module Bakery.Oven (Tray (..), loaves) where

newtype Tray = Tray (Map Shelf Loaf)
    deriving
        (
#ifdef WITH_ORDER
          Ord,
#endif
          Eq
        )

loaves :: [Loaf]
loaves =
    [
#ifdef WITH_RYE
      rye,
#endif
      wheat
    , spelt
    ]
