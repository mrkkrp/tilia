{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

-- Conditionals each of whose branches writes a head that breaks at lower
-- limits, and a conditional context in front of a type that breaks.
module Loom.Conditionals (decimal, step, anyDigit, report, runFrame, MonadTrans, ApplyTyCon) where

import Control.Monad.ST (ST, runST)
import Terminal.Buffer (Buffer, freeze, shrink)
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

decimal :: Int -> String -> Int
decimal acc input = case input of
  [] -> acc
  w : rest -> case w of
#if MIN_VERSION_base(4,16,0)
    c
      | isDigitCharacter c,
        notTooLarge acc -> case multiplyTheAccumulatorByTenSafely acc (digitValueOf c) of
          Just next -> decimal next rest
#else
    c
      | isDigit c,
        notTooLarge acc -> case multiplyByTen acc (digitToInt c) of
          Just next -> decimal next rest
#endif
          Nothing -> acc

step :: Int -> Char -> Int
step acc c
#if MIN_VERSION_base(4,16,0)
  | isDigitCharacter c,
    notTooLarge acc = case multiplyTheAccumulatorByTenSafely acc (digitValueOf c) of
      Just next -> next
#else
  | isDigit c,
    notTooLarge acc = case multiplyByTen acc (digitToInt c) of
      Just next -> next
#endif
      Nothing -> acc
  | otherwise = acc

anyDigit :: Word# -> Addr# -> Result
anyDigit acc s = case indexWord8OffAddr# s 0# of
#if MIN_VERSION_base(4,16,0)
  w
    | 1# <- leWord8# (wordToWord8# 0x30##) w,
      1# <- leWord8# w (wordToWord8# 0x39##) -> case timesWord2# acc 10## of
#else
  w
    | 1# <- leWord# 0x30## w,
      1# <- leWord# w 0x39## -> case timesWord2# acc 10## of
#endif
        (# 0##, r #) -> OK r
        _ -> Overflow
  _ -> Fail

report :: Handle -> Status -> IO ()
report h status
#ifdef mingw32_HOST_OS
  | isFailure status = do
      hPutStrLn h "failed"
#else
  | isFailure status,
    colour = do
      hPutStrLn h "\ESC[31mfailed\ESC[0m"
#endif
      hFlush h
  | otherwise = pure ()

runFrame ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (forall s. (Buffer s -> Int -> ST s Buffer) -> ST s Buffer) -> Buffer
runFrame act = runST (act (\buffer used -> shrink buffer used >> freeze buffer))

class
#if __GLASGOW_HASKELL__ >= 806
  (forall m. Monad m => Monad (t m)) => MonadTrans t where
#else
  MonadTrans t where
#endif
  lift :: Monad m => m a -> t m a

type family
#if __GLASGOW_HASKELL__ >= 910
  ApplyTyCon @k1 @k2 @unmatchable_fun :: (k1 -> k2) -> (k1 ~> unmatchable_fun)
#else
  ApplyTyCon :: (k1 -> k2) -> (k1 ~> unmatchable_fun)
#endif
  where
  ApplyTyCon = ApplyTyConAux1
