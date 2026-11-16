{-# LANGUAGE GADTs #-}

module Garden.Harvest where

-- Each kind of harvest says what it leaves behind.
data Harvest a where
  Early ::
    -- | the week it is picked in
    Int ->
    -- | the crop, ready to store
    Harvest Int
  Late ::
    -- | the bed
    String ->
    -- | the crop, ready to eat
    Harvest String
