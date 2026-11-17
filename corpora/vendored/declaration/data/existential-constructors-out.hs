module Job.Queue where

data Job = forall a. Job a (a -> IO ())

data Relay = forall a b. a ~ b => Relay (a -> b)

data Worker
  = forall s. Worker s (s -> IO s)
  | forall s. Show s => Named String s

data Step
  = forall input output.
    Step (input -> output) input
