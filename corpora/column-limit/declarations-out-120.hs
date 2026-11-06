-- Declarations of every kind that breaks, each written on one line, between
-- 90 and 120 columns wide.
module Loom.Declarations where

import Data.Map.Strict (Map, adjust, alter, delete, empty, findWithDefault, fromList, insert, toList)

data Fibre = Cotton | Wool | Silk | Linen | Hemp | Jute | Ramie | Bamboo | Alpaca | Mohair | Cashmere

data Loom = Loom{loomName :: String, loomWarp :: [Thread], loomWeft :: [Thread], loomTension :: Int}

class (Ord thread, Show thread, Hashable thread, Monoid (Spool thread)) => Threadable thread where
  spin :: thread -> Spool thread

instance (Ord thread, Show thread, Hashable thread) => Threadable (Warp thread Cotton) where
  spin = spinWarp

weave :: (Ord thread, Show colour) => Map thread colour -> thread -> colour -> Either String Int
weave = undefined

tension, shuttleCount, spoolLength, beamWidth, beamDepth, warpCount, weftCount :: Loom -> Int

shade colour | isDyed colour && saturationOf colour > threshold && lightnessOf colour < 50 = Dark
shade colour = Light
