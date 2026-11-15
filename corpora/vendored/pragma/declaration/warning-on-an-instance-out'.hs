{-# LANGUAGE StandaloneDeriving #-}

module Vault.Secret where

newtype Secret = Secret String

instance {-# WARNING "Showing a secret prints it." #-} Show Secret where
    show (Secret s) = s

deriving instance {-# DEPRECATED "Compare digests instead." #-} Eq Secret

instance {-# WARNING in "x-leak" ["Timing.", "Hash it."] #-} Ord Secret where
    compare (Secret a) (Secret b) = compare a b
