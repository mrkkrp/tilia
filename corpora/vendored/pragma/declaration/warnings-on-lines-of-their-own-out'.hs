{-# LANGUAGE StandaloneDeriving #-}

-- A warning lines up with the export item or instance head it is part of.
module Vault.Store
    ( store
    , {-# DEPRECATED "Use store instead, which also checks the digest." #-}
      storeUnchecked
    , {-# DEPRECATED "Use fetch." #-} fetchUnchecked
    )
where

newtype Secret = Secret String

instance
    {-# WARNING "Showing a secret prints it, so it is best left to tests." #-}
    Show Secret where
    show (Secret s) = s

deriving instance
    {-# DEPRECATED "Compare the digests of secrets instead of secrets." #-}
    Eq Secret
