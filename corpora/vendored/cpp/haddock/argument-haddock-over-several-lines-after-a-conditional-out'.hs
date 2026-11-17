{-# LANGUAGE CPP #-}

-- A method written once after a conditional holding the head of its class
-- in each branch, with the Haddock of an argument over several lines.
module Pantry.Shelf (Shelved (..)) where

#ifdef WITH_ORDER
-- | Things that go on a shelf in order.
class Ord e => Shelved e where
#else
-- | Things that go on a shelf.
class Shelved e where
#endif
    -- | Put it away.
    putAway
        :: e
        -- ^ The thing
        -> Int
        -- ^ Where it goes,
        -- when it was put there,
        --
        -- and who put it there.
        -> IO ()
