module Job.Handler where

data Handler
  = forall a.
    Handler
      (a -> IO ())
      a

data Hook a
  = (Show a) =>
    Hook
    { hookRun :: a -> IO ()
    }

data Task
  = forall a.
    Task
    { taskRun :: a -> IO ()
    }

data Step = forall a.
  Step
  { stepRun :: a -> IO ()
  }
