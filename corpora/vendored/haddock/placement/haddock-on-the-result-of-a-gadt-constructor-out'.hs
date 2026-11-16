{-# LANGUAGE GADTs #-}

module Garden.Harvest where

-- Each kind of harvest says what it leaves behind.
data Harvest a where
    Early
        :: Int
        -- ^ the week it is picked in
        -> Harvest Int
        -- ^ the crop, ready to store
    Late
        :: String
        -- ^ the bed
        -> Harvest String
        -- ^ the crop, ready to eat
