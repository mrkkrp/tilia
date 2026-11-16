{-# LANGUAGE TypeFamilies #-}

module Container.Slot where

data family Slot c

data instance forall a. Slot [a] = ListSlot Int
