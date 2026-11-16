{-# LANGUAGE CPP #-}

module Chimney.Flue where

newtype Flue a = Flue Int

data Narrow

type NarrowFlue = Flue Narrow

data Wide

type WideFlue = Flue Wide

width :: Flue a -> Int
#ifdef METRIC
width (Flue n) = n * 10
#else
width (Flue n) = n
#endif
