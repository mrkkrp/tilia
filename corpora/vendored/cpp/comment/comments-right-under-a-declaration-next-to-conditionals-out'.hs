{-# LANGUAGE CPP #-}

module Kiln.Glaze where

instance Show Glaze where
    show = name
-- No Read instance: a glaze is never parsed.
-- instance Read Glaze where

#if MIN_VERSION_base(4,0,0)
instance Eq Glaze where
    a == b = name a == name b
#endif
-- Glazes compare by name alone, whatever the
-- kiln did to them.
#if MIN_VERSION_base(4,0,0)
deriving instance Ord Glaze
#endif
