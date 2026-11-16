{-# LANGUAGE GADTs #-}

module Pottery.Order where

data Order = Order{{- | How many pots. -} count :: Int}

data Batch where
  Batch :: {{- | When it was fired. -} firedOn :: Day} -> Batch
  Sample :: {- | How many to try. -} Int -> Batch
  Seconds :: Int -> {- | What is left. -} Batch
