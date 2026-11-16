{-# LANGUAGE LambdaCase #-}

module Route.Planner where

legLength :: (Stop, Stop) -> Int
legLength = \case
  (Depot, stop) -> distanceFromDepot stop
  (stop, Depot) -> distanceFromDepot stop
  (from, to) -> distance from to

firstStop :: [Stop] -> Maybe Stop
firstStop = \case
  [] -> Nothing
  stop : _ -> Just stop

joinLegs :: Leg -> (Leg, Int) -> Maybe Leg
joinLegs = \cases
  leg (Leg from to, _) | to == start leg -> Just (Leg from (end leg))
  _ (_, 0) -> Nothing
  _ _ -> Nothing
