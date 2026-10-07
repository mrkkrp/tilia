{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Measuring what a piece of work costs.
module Tilia.Bench.Measure
  ( Counter,
    openCounter,
    Measurement (..),
    measure,
    measureIO,
  )
where

import Control.Concurrent (yield)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Mem (performMajorGC)

-- | A counter of the instructions the thread that opened it retires in
-- user space.
newtype Counter = Counter CInt

foreign import ccall unsafe "tilia_bench_counter_open"
  counterOpen :: IO CInt

foreign import ccall unsafe "tilia_bench_counter_read"
  counterRead :: CInt -> IO Word64

-- | Open a counter, where the system lets a process count its own
-- instructions.
openCounter :: IO (Maybe Counter)
openCounter = do
  fd <- counterOpen
  pure (if fd < 0 then Nothing else Just (Counter fd))

-- | What a piece of work cost.
data Measurement = Measurement
  { -- | The bytes it allocated.
    measuredAllocated :: !Word64,
    -- | The bytes the garbage collector copied while it ran.
    measuredCopied :: !Word64,
    -- | The instructions it retired, where they were counted.
    measuredInstructions :: !(Maybe Word64),
    -- | The CPU time it took, in nanoseconds, the least of the runs.
    measuredTime :: !Int64,
    -- | How far apart the runs' allocations were, as a fraction.
    measuredSpread :: !Double
  }

-- | Run a piece of work once to warm up and then the given number of times,
-- each time on a fresh copy of its input so that nothing is shared between
-- runs, and give what the run that warmed up computed.
measure ::
  -- | The counter of instructions, if there is one.
  Maybe Counter ->
  -- | How many runs to measure.
  Int ->
  -- | The work.
  (Text -> a) ->
  -- | Force everything the work computed.
  (a -> Int) ->
  -- | Its input.
  Text ->
  IO (a, Measurement)
measure counter runs work forced input =
  measureIO counter runs set forced
  where
    set = do
      fresh <- evaluate (T.copy input)
      pure (evaluate (work fresh))

-- | Run a piece of work once to warm up and then the given number of times,
-- each time as a setup that is not measured gives it, and give what the run
-- that warmed up computed.
measureIO ::
  -- | The counter of instructions, if there is one.
  Maybe Counter ->
  -- | How many runs to measure.
  Int ->
  -- | Set a run up, giving its work.
  IO (IO a) ->
  -- | Force everything the work computed.
  (a -> Int) ->
  IO (a, Measurement)
measureIO counter runs set forced = do
  (warm, _) <- once
  samples <- fmap snd <$> replicateM (max 1 runs) once
  let allocations = [a | (a, _, _, _) <- samples]
      (allocated, copied, instructions, _) = last samples
  pure
    ( warm,
      Measurement
        { measuredAllocated = allocated,
          measuredCopied = copied,
          measuredInstructions = instructions,
          measuredTime = minimum [t | (_, _, _, t) <- samples],
          measuredSpread =
            fromIntegral (maximum allocations - minimum allocations)
              / fromIntegral (max 1 (minimum allocations))
        }
    )
  where
    count = traverse (\(Counter fd) -> counterRead fd) counter
    once = do
      work <- set
      performMajorGC
      yield
      performMajorGC
      before <- getRTSStats
      started <- count
      result <- work
      _ <- evaluate (forced result)
      ended <- count
      performMajorGC
      after <- getRTSStats
      pure
        ( result,
          ( allocated_bytes after - allocated_bytes before,
            copied_bytes after
              - copied_bytes before
              - gcdetails_copied_bytes (gc after),
            (-) <$> ended <*> started,
            cpu_ns after - cpu_ns before
          )
        )
