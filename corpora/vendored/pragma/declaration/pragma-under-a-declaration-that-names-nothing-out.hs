{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- A pragma is set apart from a declaration next to it that it does not
-- name, such as an instance, a splice or one with a Haddock.
module Depot.Crate where

data family Lid c

type family Load c

newtype Crate = Crate{slots :: [Int]}

{-# INLINE seal #-}
seal :: Crate -> Crate
seal = id

data instance Lid Crate = CrateLid

{-# INLINE weigh #-}
weigh :: Crate -> Int
weigh = sum . slots

type instance Load Crate = Int

{-# INLINE count #-}
count :: Crate -> Int
count = length . slots

deriving instance Show Crate

{-# INLINE label #-}
label :: Crate -> String
label = show

makeLenses ''Crate

{-# NOINLINE stamp #-}
stamp :: Crate -> String
stamp = label

unpack :: Crate -> [Int]
unpack = slots
{-# INLINE unpack #-}

-- | Whether the crate holds nothing.
isEmpty :: Crate -> Bool
isEmpty = null . slots
