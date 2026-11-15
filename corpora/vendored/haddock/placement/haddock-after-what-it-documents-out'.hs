module Kiln.Firing where

data Cone = Cone Int

firingCone :: Cone
firingCone = Cone 6
-- ^ The cone most of the glazes mature at.

bisqueCone :: Cone
bisqueCone = Cone 4
-- ^ The cone a first firing goes to.

class Kiln k where
    peak :: k -> Int
    -- ^ The hottest the kiln gets, in degrees.

    rampRate :: k -> Int
    -- ^ How fast it climbs, in degrees an hour.

    cool :: k -> k
