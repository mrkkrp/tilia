module Job.Queue
  ( run -- ^ The main entry point.
  , Job (..) -- ^ A job and its fields.
  , enqueue
  , Priority (High, Low) -- ^ How urgent a job is.
  ) where

run :: IO ()
run = pure ()
