-- A comment lined up with the last line of a construct, and not with what
-- follows the construct, stays inside it.
module Loom.Warp where

import Control.Applicative ((<|>))

threads ::
  Int ->
  Int
  -- counted across the whole width
threads ends = ends * 2

tension :: Int
tension = pull 3 4
  where
    pull ::
      Int ->
      -- \^ Weight on the beam
      Int ->
      -- \^ Turns of the brake
      Int
      -- \^ What the warp takes
    pull weight turns = weight * turns

data Shed
  = Plain Int
  | Twill
      Int
      -- Shafts raised
      Int
      -- Shafts lowered
  deriving (Show)

sett :: Maybe Int
sett =
  id
    ( Just 12
        <|> Just 16
    )
    -- ends per centimetre

reed :: Int
reed = 60

selvedge :: String
selvedge =
  "doubled"
    <> " ends"
  -- on both sides

beam :: Int
beam = 2
