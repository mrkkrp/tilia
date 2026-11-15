-- A Haddock that follows another doc item in an export list is held off it
-- by an empty line.
module Job.Queue
  ( {- $intro -}
    -- | Running jobs.
    run,
    {- | Stopping a job. -}
    -- | Stopping every job.
    stop,
  ) where

run :: IO ()
run = pure ()

stop :: IO ()
stop = pure ()
