{-# LANGUAGE LambdaCase #-}

-- | Tables of answers, each worked out once however many threads ask for it.
module Tilia.Memo
  ( Memo,
    newMemo,
    Flights,
    newFlights,
    memoized,
  )
where

import Control.Concurrent
  ( MVar,
    ThreadId,
    modifyMVar,
    modifyMVar_,
    myThreadId,
    newEmptyMVar,
    newMVar,
    readMVar,
    tryPutMVar,
    tryReadMVar,
  )
import Control.Exception (onException)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Unique (Unique, newUnique)

-- | Answers filed under the names they are about, each worked out once.
data Memo v = Memo Unique (IORef (Map Text (MVar v)))

-- | An empty 'Memo'.
newMemo :: IO (Memo v)
newMemo = Memo <$> newUnique <*> newIORef Map.empty

-- | Which thread is working out which answer, and which answer each waiting
-- thread waits for, across all of a resolver's tables.
data Flights = Flights
  { flightOwners :: Map (Unique, Text) ThreadId,
    flightWaits :: Map ThreadId (Unique, Text)
  }

-- | Nobody working out anything, and nobody waiting.
newFlights :: IO (MVar Flights)
newFlights = newMVar (Flights Map.empty Map.empty)

-- | Look an answer up in a table, working it out and filing it the first
-- time.
memoized ::
  -- | Who is working out what.
  MVar Flights ->
  -- | Where the answers are filed.
  Memo v ->
  -- | The answer for a thread that cannot wait.
  v ->
  -- | What the answer is about.
  Text ->
  -- | Work that is being memoized.
  IO v ->
  IO v
memoized flights (Memo table answers) standIn key work =
  Map.lookup key <$> readIORef answers >>= \case
    Just slot -> tryReadMVar slot >>= maybe (waitFor slot) pure
    Nothing -> do
      me <- myThreadId
      claimed <- modifyMVar flights $ \fl -> do
        filed <- readIORef answers
        case Map.lookup key filed of
          Just slot -> pure (fl, Left slot)
          Nothing -> do
            slot <- newEmptyMVar
            writeIORef answers (Map.insert key slot filed)
            pure
              ( fl{flightOwners = Map.insert (table, key) me (flightOwners fl)},
                Right slot
              )
      case claimed of
        Left slot -> waitFor slot
        Right slot -> do
          answer <- work `onException` file slot standIn
          answer <$ file slot answer
  where
    file slot answer = modifyMVar_ flights $ \fl -> do
      _ <- tryPutMVar slot answer
      pure fl{flightOwners = Map.delete (table, key) (flightOwners fl)}
    waitFor slot = do
      me <- myThreadId
      settled <- modifyMVar flights $ \fl ->
        tryReadMVar slot >>= \case
          Just answer -> pure (fl, Just answer)
          Nothing
            | waitedOnBy fl me (table, key) -> pure (fl, Just standIn)
            | otherwise ->
                pure
                  ( fl{flightWaits = Map.insert me (table, key) (flightWaits fl)},
                    Nothing
                  )
      case settled of
        Just answer -> pure answer
        Nothing -> do
          answer <- readMVar slot
          modifyMVar_ flights $ \fl ->
            pure fl{flightWaits = Map.delete me (flightWaits fl)}
          pure answer

-- | Is this answer being worked out by the given thread, or by one that
-- waits for it, directly or through others?
waitedOnBy :: Flights -> ThreadId -> (Unique, Text) -> Bool
waitedOnBy fl me = go
  where
    go key = case Map.lookup key (flightOwners fl) of
      Nothing -> False
      Just owner
        | owner == me -> True
        | otherwise -> maybe False go (Map.lookup owner (flightWaits fl))
