module Pretty.Instances where

class Pretty a where
    pretty :: a -> String

instance {-# OVERLAPPABLE #-} (Show a) => Pretty a where
    pretty = show

instance {-# OVERLAPPING #-} Pretty String where
    pretty = id

instance {-# OVERLAPS #-} Pretty [Int] where
    pretty = concatMap show

instance {-# INCOHERENT #-} Pretty (Maybe a) where
    pretty _ = "?"
