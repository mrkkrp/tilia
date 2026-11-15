{-# LANGUAGE MonadComprehensions #-}

module Roster.Shift where

pairing :: Maybe Worker -> Maybe Worker -> Maybe (Worker, Worker)
pairing early late = [(e, l) | e <- early, l <- late, e /= l]

cover :: Shift -> Maybe Worker
cover shift =
  [ w
  | w <- lookupWorker (shiftLead shift),
    available w,
    not (onLeave w (shiftDay shift))
  ]
