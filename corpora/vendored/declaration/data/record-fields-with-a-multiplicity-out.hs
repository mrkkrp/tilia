{-# LANGUAGE LinearTypes #-}

module Resource.Lease where

import GHC.Exts (Multiplicity (..))

data Lease m a = Lease{leased %m :: a, leaseDays %'Many :: Int}
